getwd()
setwd("..")
setwd("..")

library(tidyverse)

################################################################################
# STEP 1 - Create candidates list from linked toponyms

# Load the three dataframes: OS and OSM generated candidates, and entities list
osm <-
  read_csv("users/sb02767/processed_geocode_results_including_missing_postcode_entities.csv") |>
  mutate(osm = "OSM") |>
  rename(value = Entity) |> 
  mutate(POSTCODE_DISTRICT = if_else(POSTCODE_DISTRICT == "None", NA_character_, POSTCODE_DISTRICT))

os <-
  read_csv("users/sb02767/os_gazetteer_lookup_match.csv") |>
  select(-c(`...1`, NAME1, entity)) |>
  mutate(os = "OS") |> mutate(value = str_to_lower(value))

ents2 <- read_csv("/parallel_scratch/sb02767/unique_entities.csv")

# MERGE THE THREE
ents_merged <-
  ents2 |>
  left_join(os, by = "value") |>
  full_join(osm, by = c("value", "POSTCODE_DISTRICT"))

# start cleaning: for any combination of value and postcode, retain the most complete row
ents_merged_clean <-
  ents_merged |>
  select(c(value, POSTCODE_DISTRICT, os, osm, Latitude, Longitude))

ents_merged_clean <- ents_merged_clean|>
  filter(!is.na(value)) |>
  group_by(value, POSTCODE_DISTRICT) |>
  fill(names(ents_merged_clean), .direction = "downup") |>
  ungroup() |>
  distinct()

# some entities have nearly identical names, simply differ in terms of capitalisation, presence of symbol or numbers.
# let's merge these cases and spread their information across (e.g., a match was found for Albion Road but not 12 Albion Road - share the info of Albion Road with 12 Albion Road)
os_osm <- ents_merged_clean |>
  mutate(clean_name = str_to_lower(value),
         clean_name = str_remove(clean_name, "'s"),
         clean_name = stringr::str_replace_all(clean_name,"[^a-zA-Z\\s]", " "),
         clean_name = trimws(clean_name)) |>
  group_by(clean_name, POSTCODE_DISTRICT) |>
  mutate(values = paste0(value, collapse = "; ")) |> ungroup() |> select(-value)

os_osm <- os_osm |>
  group_by(clean_name, POSTCODE_DISTRICT) |>
  fill(names(os_osm), .direction = "downup") |>
  distinct() |>
  ungroup() |>
  filter(clean_name != "")

set.seed(13829) # stochastic reproducibility ensurer
os_osm_ <-
  os_osm |>
  group_by(clean_name, POSTCODE_DISTRICT) |>
  slice_sample(n = 1)

os_osm_2 <- os_osm_ |> 
  filter_at(vars(POSTCODE_DISTRICT, Latitude, Longitude), any_vars(!is.na(.)))

write_csv(os_osm_2, "/parallel_scratch/sb02767/os_osm_candidates.csv") 
