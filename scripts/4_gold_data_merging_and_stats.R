library(tidyverse)
library(data.table)
library(jsonlite)

#####################
# First batch
#####################
toponym_candidates_with_lads_no_dupl_lads <- read_csv("toponym_candidates_with_lads_no_dupl_lads.csv")
first_batch <- read_csv("annotators_data_no_org.csv") # there is org in here though
to_review <- first_batch |> select(doc, machine_entities_array) |> distinct()

annotators_disambiguated_data <- read_csv("annotators_disambiguated_data.csv") |> 
  mutate(values = str_to_lower(machine_entities_array)) |> 
  left_join(toponym_candidates_with_lads_no_dupl_lads, by = c("values", "annotators_choice" = "LAD24NM")) |> 
  inner_join(to_review)

entity_position <- 
  jsonlite::fromJSON("labelstudio_predictions.json") |> # contains position for first batch entities
  as.data.frame() |> 
  pull(predictions) |> # Extract the "predictions" column (a list)
  map_dfr(~ .x$result[[1]]) |> 
  unnest(value) |> 
  mutate(labels = as.character(unlist(labels))) |> 
  filter(labels != "ORG") |> 
  select(-c(id, from_name, to_name, type))

first_batch_ready <- entity_position |> 
  left_join(annotators_disambiguated_data, by = c("doc", "text" = "machine_entities_array")) %>% 
  mutate(values = str_to_lower(text)) %>% 
  anti_join(to_remove, by = c("values" = "value"))

more_removals <- c("US", "States", "#", "Wakanda", "the Emerald Isle")

first_batch_ready <- first_batch_ready %>%
  filter(
    !annotators_choice %in%
      c(
        "Entity is a location outside the UK",
        "Entity spans across several districts (e.g., a region)",
        "Entity is not a location"
      )
  ) %>% 
  filter(!text %in% more_removals) %>% select(-c(id, `...1`))

######################################################
# New 100 articles to annotate extraction (skip this part to go straight to data prep)
######################################################
# articles_to_sample_from <-
#   read_csv('articles_to_sample_from.csv')
# 
# # here we retain, whenever an article has been duplicated, the first original article
# articles_to_sample_from_2 <-
#   articles_to_sample_from |>
#   select(-c(value, n_candidates)) |>
#   group_by(article_id_group) |>
#   slice_min(order_by = tweet_date, n = 1) |>
#   ungroup() |>
#   distinct() |>
#   mutate(quarter = lubridate::quarter(str_remove(tweet_date, ";.*"),
#                                       with_year = TRUE)) |>
#   filter(!is.na(domain))
# 
# sample_no_dupl_lads <- read_csv("sample_10k_w_one_candidate_kept.csv")
# 
# final_sample <- sample_no_dupl_lads |>
#   ungroup() |>
#   select(c(tweet_date, domain, main_LAD, n_candidates, article_id_group, value)) |>
#   group_by(article_id_group) |>
#   mutate(n_entities = n_distinct(value)) |>
#   ungroup() |>
#   distinct()
# 
# new_100_article_sample <- articles_to_sample_from_2 |>
#   anti_join(final_sample, by = c('article_id_group', 'tweet_date', 'domain', 'main_LAD'))
# 
# # Set target total articles
# total_articles <- 100
# 
# # Calculate the number of unique strata (combinations of geography, domain, and quarter)
# n_geographies <- length(unique(new_100_article_sample$main_LAD))
# n_domains <- length(unique(new_100_article_sample$domain))
# 
# # Total number of unique strata (geography * domain)
# total_strata <- n_geographies * n_domains
# 
# # Calculate the expected even number of articles per stratum
# expected_articles_per_stratum <- total_articles / total_strata *100
# 
# # Set the target of 2 articles per stratum
# target_articles_per_stratum <- 1
# 
# articles_text <- articles |>
#   mutate(article_id_group = if_else(is.na(duplicate_group), article_id, duplicate_group)) |>
#   select(article_text, article_id_group)
# 
# # Sample 2 articles per stratum, if there are fewer than 2 articles, sample all that are available
# new_100_article_sample <- new_100_article_sample |>
#   group_by(main_LAD) |>
#   sample_n(size = min(target_articles_per_stratum, n()), replace = FALSE) |>
#   ungroup() |>
#   slice_sample(n = 100) |>
#   left_join(articles_text, by = "article_id_group") |>
#   distinct() |>
#   group_by(article_id_group) |>
#   slice_min(order_by = tweet_date, n = 1) |>
#   slice_sample(n = 1) # no seed, so please don't overwrite files
# 
# # write_csv(new_100_article_sample_, "parallel_scratch/sb02767/new_100_article_sample.csv")

######################################################
# Second Sample
######################################################
new_100_article_sample_ <-
  read_csv("new_100_article_sample.csv")

# these have been annotated using spaCy
new_100_article_sample_entities <- fromJSON('new_100_article_sample.json', flatten = TRUE)

long <- new_100_article_sample_entities %>%
  unnest_longer(entities) %>% 
  unnest_wider(entities) %>% 
  mutate(entity_lower = str_to_lower(entity))

to_remove <- read_csv("entities_to_remove.csv")

long_filtered <- long %>% 
  anti_join(to_remove, by = c("entity_lower" = "value"))

second_batch <-
  read_csv("Additional Data to Disambiguate.csv") |>
  mutate(
    annotators_choice = if_else(
      Agreement == FALSE,
      `Disagreement Review`,
      `Annotator 1 Selection`
    ),
    options_str = paste0(
      options,
      "; LAD not in options; Entity is not a location; Entity is a location outside the UK; Entity spans across several districts (e.g., a region); Unsure"
    )
  ) |>
  select(group_id, annotators_choice, options_str, entity)

metadata_second_batch <-
  read_csv("new_100_article_sample_w_candidates_for_google.csv") |> # needed this step because doc did not merge properly
  left_join(second_batch, by = c("group_id", "entity")) |> 
  select(-c(options, options_str, group_id))

second_batch_ready <- long_filtered %>% 
  left_join(metadata_second_batch, by = c("doc", "entity")) %>% 
  filter(!entity %in% c("US", "the Irish Sea")) %>% 
  filter(
    !annotators_choice %in%
      c(
        "Entity is a location outside the UK",
        "Entity spans across several districts (e.g., a region)",
        "Entity spans across multiple districts",
        "Entity spans across multiple ditricts",
        "Entity is not a location"
      )
  ) %>% 
  left_join(toponym_candidates_with_lads_no_dupl_lads, 
            by = c("entity_lower" = "values", 
                   "annotators_choice" = "LAD24NM")) %>% 
  rename(labels = label, values = entity_lower, 
         text = entity)
  
#########################################
# Merge
########################################
intersect(colnames(first_batch_ready), colnames(second_batch_ready))
  
# ner_corpus
# ner_corpus <- read_csv("ner_corpus_only_relevant_domains.csv")
# ner_corpus_simple <- ner_corpus |> select(text_clean, text_old, group_articles)
# write_csv(ner_corpus_simple, "ner_corpus_simple.csv")
ner_corpus_simply = read_csv("ner_corpus_simple.csv")
articles <- read_csv("articles.csv")

first_batch_ready <- first_batch_ready |>
  inner_join(ner_corpus_simple, by = c("doc" = "text_clean")) |>
  left_join(articles, by = c("text_old" = "article_text")) |>
  mutate(article_id_group = if_else(is.na(duplicate_group), article_id, duplicate_group)) |>
  group_by(text, start, end, doc) |>
  slice_min(order_by = tweet_date, n = 1) %>% 
  rename(domain = domain.x,
         main_LAD = main_LAD.x) %>% 
  select(colnames(second_batch_ready))

###########################
gold_data = bind_rows(first_batch_ready, second_batch_ready) %>% 
  filter(text != "Gazprom Arena") %>% 
  filter(text != "the DW Stadium") %>% 
  mutate(domain = str_remove(domain, ";.*"))

main_lad <- read_sheet("https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=1604560610#gid=1604560610",
                       sheet = "Domains") %>% select(domain, LAD,	Main_LAD) %>% 
  mutate(main_LAD = if_else(is.na(Main_LAD), LAD, Main_LAD)) %>% select(domain, main_LAD)

gold_data = gold_data %>% 
  group_by(doc) %>%
  fill(main_LAD, domain, .direction = "downup") %>% 
  left_join(main_lad, by = "domain") %>% 
  rename(main_LAD = main_LAD.y) %>% 
  select(-c(main_LAD.x, LAD24NMW))

# assess LAD_not_in_options
library(googlesheets4)
gs4_deauth()

lad_not_in_options <-
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=990951076#gid=990951076",
    sheet = "LAD not in options"
  ) %>%
  select(c(doc, machine_entities_array, true_lad)) %>%
  rename(text = machine_entities_array)

# join
gold_data_ <- gold_data %>%
  left_join(lad_not_in_options, by = c("doc", "text")) %>%
  mutate(recoded_choice = if_else(!is.na(true_lad), true_lad, annotators_choice))

gs4_deauth()

# manually tag true_lad for missing entities
write_csv(gold_data_ %>% filter(is.na(recoded_choice)), "Final Disambiguations.csv")

# manually annotated
final_disamb <- read_sheet("https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=990951076#gid=990951076",
                           sheet = "Final Disambiguations") %>% 
  mutate(Latitude = as.character(Latitude),
    Longitude = as.character(Longitude)
  )

# get centroid coordinates for LADs for remaining unassigned ones
district_coord <- read_sheet("https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=990951076#gid=990951076",
                             sheet = "Districts") %>% select(LAD, Latitude, Longitude) %>% 
  mutate(across(c(Latitude, Longitude), ~ as.character(.)))
  

gold_data_fixed <- gold_data_ %>%
  mutate(across(c(Latitude, Longitude), ~ as.character(.))) %>%
  left_join(final_disamb, by = c("doc", "text", "domain", "main_LAD", "article_id_group")) %>%
  mutate(
    recoded_choice = coalesce(recoded_choice.x, recoded_choice.y),
    Latitude = coalesce(Latitude.x, Latitude.y),
    Longitude = coalesce(Longitude.x, Longitude.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y")) %>%
  filter(!str_detect(recoded_choice, "Entity")) %>%
  left_join(district_coord, by = c("main_LAD" = "LAD")) %>% # Join district coordinates
  mutate(
    Latitude = coalesce(Latitude.x, Latitude.y), # Replace NA with district coordinates
    Longitude = coalesce(Longitude.x, Longitude.y)
  ) %>%
  select(-c(Latitude.y, Longitude.y, Latitude.x, Longitude.x, true_lad))  %>% # Clean up temporary columns
  ungroup()

# Fill NA values in the 'id' column with a sequential number pattern
gold_data_fixed <- gold_data_fixed %>%
  mutate(article_id_group = ifelse(is.na(article_id_group), 
                     paste0("article_unnumbered_", row_number()),  # Create a sequential number
                     article_id_group)) %>% 
  select(-c(os, osm, POSTCODE_DISTRICT, clean_name, BNG_E, BNG_N, LONG, LAT, geometry, LAD24CD))

write_csv(gold_data_fixed, "gold_data.csv")

gold_data <- gold_data_fixed

##################
# Statistics
##################

# Number of articles
gold_data %>% select(doc) %>% n_distinct() # 182
gold_data %>% select(text) %>% n_distinct() # 838
gold_data %>% group_by(doc) %>% select(text) %>% n_distinct() # 925

gold_data |> select(doc, domain) |> distinct() |> count(domain) |> 
  summarise(mean(n), median(n), sd(n))

gold_data |> count(doc) |> 
  summarise(mean(n), median(n), sd(n))

gold_data |> select(doc, main_LAD) |> distinct() |> count(main_LAD) |> 
  summarise(mean(n), median(n), sd(n))

gold_data |> 
  count(labels)

gold_data |> select(text, labels) |> distinct() |> count(labels) 

gold_data |> filter(labels == "FAC") |> count(labels, doc) |> 
  summarise(mean(n), median(n), sd(n))

gold_data |> filter(labels == "GPE") |> count(labels, doc) |> 
  summarise(mean(n), median(n), sd(n))

gold_data |> filter(labels == "LOC") |> count(labels, doc) |> 
  summarise(mean(n), median(n), sd(n))

publishers <- read_sheet("https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=1604560610#gid=1604560610",
                         sheet = "Domains") |> 
  select(domain, Owner) |> 
  inner_join(gold_data |> select(domain) |> distinct(), by = c("domain")) |> count(Owner)

publishers |> 
  summarise(mean(n), median(n), sd(n))

summary <- gold_data %>%
  ungroup() %>% 
  summarise(across(everything(), n_distinct))

# word count
library(tidytext)
words <- gold_data |> 
  select(doc) |> 
  distinct() |> 
  unnest_tokens(word, doc, drop = FALSE) #|> select(word) |> n_distinct()
  count(doc, word) |> 
  group_by(doc) |> 
  summarise(n_words = n())

mean(words$n_words)
median(words$n_words)
sd(words$n_words)


n_labels <- gold_data %>% count(labels)
########################################################
# SAVE FOR EXPORT
########################################################
# Additional constant fields
geonamesID <- "0"
type <- "OTHER"

# Transform the dataframe to the desired structure
result <-
  lapply(split(gold_data_fixed, gold_data_fixed$article_id_group), function(subset_df) {
    lapply(1:nrow(subset_df), function(i) {
      # Dynamically create the list with the 'label' value as the key
      setNames(
        list(
          subset_df$text[i],
          start = subset_df$start[i],
          end = subset_df$end[i],
          geonamesID = geonamesID,
          Latitude = subset_df$Latitude[i],
          Longitude = subset_df$Longitude[i],
          type = type
        ),
        c(
          subset_df$labels[i],
          "start",
          "end",
          "geonamesID",
          "Latitude",
          "Longitude",
          "type"
        ) # Set names dynamically
      )
    })
  })

# Convert to JSON
json_result <- toJSON(result, pretty = TRUE)
write(json_result,
      "gold_data_annotations.json")

# Specify the folder path
output_folder <- "gold_data_articles/"

# Iterate over the data frame split by article_id_group
lapply(split(gold_data_fixed, gold_data_fixed$article_id_group), function(subset_df) {
  # Get the article_id_group value (assuming it's consistent within each subset)
  article_id_group <- unique(subset_df$article_id_group)

  # Define the file path for saving
  file_path <- paste0(output_folder, article_id_group, ".txt")

  # Save the `doc` variable to the file
  writeLines(subset_df$doc, file_path)
})


######
# File for Harvard Dataverse
off <- read_csv("LMUK-Geo.csv")
off <- off |> left_join(publishers) |> 
  rename(publisher = Owner)

# Transform the dataframe to the desired structure
result <- lapply(1:nrow(off), function(i) {
  # Dynamically create the list with the 'label' value as the key
  setNames(
    list(
      toponym = off$text[i],
      start = off$start[i],
      end = off$end[i],
      label = off$label[i],
      article = off$doc[i],
      domain = off$domain[i],
      domain_lad = off$domain_lad[i],
      local_authority_district = off$local_authority_district[i],
      Latitude = off$Latitude[i],
      Longitude = off$Longitude[i],
      publisher = off$publisher[i]
    ),
    c("toponym",
      "start",
      "end",
      "label",
      "article",
      "domain",
      "domain_lad",
      "local_authority_district",
      "Latitude",
      "Longitude",
      "publisher"
    )
  )
})

# Convert to JSON
json_result <- toJSON(result, pretty = TRUE)
write(json_result, "LMUK-Geo.json")