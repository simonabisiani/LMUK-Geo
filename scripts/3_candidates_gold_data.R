library(tidyverse)
library(data.table)  
library(geosphere)   
library(httr)
library(PostcodesioR)
library(jsonlite)
library(sf)
library(maps)

gold_data_expanded_by_candidates <- read_csv("candidates_of_gold_data.csv")
candidates_gold_data <- read_csv("candidates_gold_data_raw.csv")

# 52 unique place names, representing 67 location mentions failed to have candidates
gold_data_expanded_by_candidates %>% anti_join(candidates_gold_data, by = "values") %>%
  filter(is.na(os) & is.na(osm)) %>% select(text) |> n_distinct()

colSums(is.na(candidates_gold_data)) # Whenever OS retrieved, we had postcode by not coordinates. We miss 62 postcodes, and 4669 coordinates
# rm(candidates)

# number of unique place names: 739
candidates_gold_data |> 
  select(text) |> 
  n_distinct()

################################################################################
# FILL IN MISSING DATA LIKE POSTCODES AND COORDINATES
################################################################################
# # 1. Fetching missing coordinates based on postcode (outward code)
# missing_coordinates <- candidates_gold_data |>
#   filter(is.na(Latitude) | is.na(Longitude)) |>
#   select(POSTCODE_DISTRICT) |>
#   distinct() |>
#   pull()
# 
# # Using purrr::map_df to apply the function and store the results in a data frame
# results <- purrr::map_df(missing_coordinates, function(code) {
#   lat_long <- PostcodesioR::outward_code_lookup(code)
#   if (!is.null(lat_long)) {
#     return(
#       data.frame(
#         POSTCODE_DISTRICT = code,
#         Latitude = lat_long$latitude,
#         Longitude = lat_long$longitude
#       )
#     )
#   }
#   return(NULL)
# })
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 2. Fetching missing postcode based on coordinates
# missing_postcode <- candidates_gold_data |>
#   filter(is.na(POSTCODE_DISTRICT)) |>
#   select(Latitude, Longitude) |>
#   distinct()
# 
# get_postcode <- function(lat, long) {
#   # Construct the API URL using the provided latitude and longitude
#   url <- sprintf("https://api.postcodes.io/outcodes?lon=%s&lat=%s",
#                  long,
#                  lat)
#   
#   # Make the GET request to the API
#   response <- GET(url)
#   
#   # Check if the response is successful
#   if (status_code(response) == 200) {
#     # Parse the response content as text and then convert it to JSON
#     data <- fromJSON(content(response, as = "text"))
#     
#     # Check if the result contains an outcode and return it
#     if (!is.null(data$result$outcode)) {
#       return(data$result$outcode[[1]])
#     } else {
#       return(NA)  # No postcode found
#     }
#   } else {
#     return(NA)  # Return NA if the request was not successful
#   }
# }
# 
# batch_size <- 10  # Define your batch size
# 
# # Split the data into batches
# batches <-
#   split(missing_postcode, ceiling(seq_along(1:nrow(missing_postcode)) / batch_size))
# 
# # Function to process each batch
# process_batch <- function(batch_data) {
#   purrr::map_df(1:nrow(batch_data), function(i) {
#     lat <- batch_data$Latitude[i]
#     long <- batch_data$Longitude[i]
#     
#     # Ensure longitude and latitude are valid
#     if (is.na(lat) || is.na(long) ||
#         nchar(sub(".*\\.", "", as.character(long))) < 2 ||
#         nchar(sub(".*\\.", "", as.character(lat))) < 2) {
#       return(data.frame(
#         Latitude = lat,
#         Longitude = long,
#         POSTCODE_DISTRICT = NA
#       ))
#     }
#     
#     # Call the API to get the postcode
#     outcode <- get_postcode(lat, long)
#     
#     return(data.frame(
#       Latitude = lat,
#       Longitude = long,
#       POSTCODE_DISTRICT = outcode
#     ))
#   })
# }
# 
# # Initialize an empty list to store the results
# results_list <- list()
# 
# # Process each batch
# for (i in seq_along(batches)) {
#   message(sprintf("Processing batch %d of %d", i, length(batches)))
#   batch_result <- process_batch(batches[[i]])
#   results_list[[i]] <- batch_result
# }
# 
# # Combine all results into a single data frame
# reverse_geocoded <- bind_rows(results_list) |> drop_na()
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 3. Bind results and update candidates
# candidates_updated <-
#   candidates_gold_data |>
#   rows_update(results, by = "POSTCODE_DISTRICT") |>
#   rows_update(reverse_geocoded, by = c("Latitude", "Longitude")) |>
#   filter(!is.na(Latitude) & !is.na(Longitude))
# 
# candidates_updated_slim <-
#   candidates_updated |>
#   group_by(POSTCODE_DISTRICT, values) |>
#   slice(which.min(rowSums(is.na(
#     across(everything())
#   ))))
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 4. Get Local Authority District for each coordinate
# lad_shapefile <-
#   read_sf("geo_data/LAD_MAY_2024_UK_BFE.shp")
# 
# # Convert candidates to sf object with the correct CRS
# candidates_coords <- candidates_updated_slim |>
#   select(Longitude, Latitude) |>
#   distinct() |>
#   st_as_sf(
#     coords = c("Longitude", "Latitude"),
#     crs = 4326,
#     remove = FALSE
#   ) |>
#   st_transform(crs = st_crs(lad_shapefile))
# 
# # Perform spatial join to find the corresponding LAD
# points_to_LAD <-
#   st_join(candidates_coords, lad_shapefile, left = TRUE)
# 
# # Add LAD to original data and identify missing LADs
# candidates_lads <-
#   candidates_updated_slim |>
#   left_join(points_to_LAD |> select(-values, -POSTCODE_DISTRICT), by = c("Longitude", "Latitude")) |> 
#   distinct()
# 
# # Handle missing LADs by finding the nearest LAD for NA values
# is_na <-
#   candidates_lads |>
#   filter(is.na(LAD24NM)) |>
#   select(Latitude, Longitude, values)
# 
# is_na_sf <- is_na |>
#   st_as_sf(
#     coords = c("Longitude", "Latitude"),
#     crs = 4326,
#     remove = FALSE
#   ) |>
#   st_transform(crs = st_crs(lad_shapefile))
# 
# # Find the nearest LAD for the points with NA LAD
# nearest_LADs <-
#   st_nearest_feature(is_na_sf, lad_shapefile)
# 
# # Add the nearest LAD info to the NA points
# is_na_nearest <-
#   st_drop_geometry(lad_shapefile[nearest_LADs, ])
# 
# # Combine the original LAD data with the nearest LAD for NA points
# is_na <- is_na |>
#   bind_cols(is_na_nearest) |>
#   distinct()
# 
# # Combine back with the rest of the data
# candidates_gold_data_with_lads <-
#   candidates_lads |>
#   rows_update(is_na, by = c("Latitude", "Longitude", "values"))

# write_csv(candidates_gold_data_with_lads, "toponym_candidates_with_lads_gold_data.csv")
candidates_gold_data_with_lads <- read_csv("toponym_candidates_with_lads_gold_data.csv")

################################################################################
# PLOTS
################################################################################
# number of candidates: 7371
candidates_gold_data_with_lads |> 
  select(text, LAD24NM) |> 
  distinct() |> 
  nrow()

# number of place names to resolve using the one sense per referent
gold_data_expanded_by_candidates |>
  select(text, labels, doc) |> 
  n_distinct()

# number of multiple LADs per toponym
candidates_gold_data_with_lads |> 
  distinct() |> 
  count(LAD24NM, text) |> filter(n > 1) # none

labels <- gold_data_expanded_by_candidates |> select(text, labels) |> distinct()

candidates_gold_data_with_lads <- candidates_gold_data_with_lads |>
  left_join(labels, by = "text")

# number of ambiguous vs unambiguous place names
a <- candidates_gold_data_with_lads |> 
  select(text, labels, LAD24NM) |> 
  distinct() |>
  group_by(text, labels) |> 
  summarise(n_candidates = n()) |> 
  ggplot(aes(x = n_candidates)) +
  geom_histogram(fill = "cornflowerblue", color = "white", size = 0.1, alpha = 0.8)+
  facet_wrap(~labels, nrow = 3, ncol = 1)+
  scale_x_sqrt(breaks = c(0,10, 100,300))+
  scale_y_continuous(breaks = c(0,100,200))+
  theme_minimal()+
  labs(title = "(a)", x = "Number of candidates", y = "Number of place names")+
  theme(plot.title = element_text(hjust = 0.5))

a <- candidates_gold_data_with_lads |> 
  select(text, labels, LAD24NM) |> 
  distinct() |>
  group_by(text, labels) |> 
  summarise(n_candidates = n()) |> 
  ggplot(aes(x = n_candidates, y = labels)) +
  geom_boxplot(fill = "cornflowerblue", size = 0.1, alpha = 0.8)+
  geom_jitter(size = 0.2)+
  scale_x_sqrt(breaks = c(0,10, 100,300))+
  theme_minimal()+
  labs(title = "(a)", x = "Number of candidates", y = "Number of place names")+
  theme(plot.title = element_text(hjust = 0.5))

a <- gold_data_expanded_by_candidates |> 
  group_by(text, labels) |> 
  summarise(n_doc = n_distinct(doc),
         n_cands = n_distinct(LAD24NM)) |> 
  ungroup() |> 
  ggplot(aes(x = n_doc, y = n_cands)) +
  geom_jitter(size = 0.4, height = 0.2, width = 0.4)+
  theme_minimal()+
  facet_wrap(~labels, nrow = 3, ncol = 1)+
  scale_y_log10()+
  labs(title = "(a)", x = "Number of articles", y = "Number of candidates")+
  theme(plot.title = element_text(hjust = 0.5))

gold_data_expanded_by_candidates |> 
  group_by(text, labels) |> 
  summarise(n_doc = n_distinct(doc)) |> 
  # perc of n_doc == 1
  group_by(labels) |>
  summarise(n = sum(n_doc == 1), total = n()) |>
  mutate(perc = n/total) |> 
  summarise(mean = mean(perc))

gold_data_expanded_by_candidates |> 
  group_by(text, labels) |> 
  summarise(n_cands = n_distinct(LAD24NM)) |> 
  group_by(labels) |>
  summarise(mean = mean(n_cands)) 

# # toponym types combination across articles
# gold_data_expanded_by_candidates |>
#   select(doc, labels) |> 
#   distinct() |> 
#   group_by(doc) |> 
#   mutate(combination = paste(labels, collapse = ", ")) |> 
#   ungroup() |> 
#   select(-c(labels, doc)) |> 
#   group_by(combination) |> 
#   count() |> 
#   # Split the string into individual components and sort them
#   rowwise() %>%
#   mutate(normalised_combination = paste(sort(unlist(strsplit(combination, ", "))), collapse = ", ")) %>%
#   ungroup() %>%
#   # Group by the normalised combination and sum the counts
#   group_by(normalised_combination) %>%
#   summarise(total_n = sum(n), .groups = "drop") |> 
#   arrange(desc(total_n)) |> 
#   ggplot(aes(x = reorder(normalised_combination, total_n), y = total_n))+
#   geom_bar(fill = "cornflowerblue", stat = "identity")+
#   theme_minimal()+
#   labs(x = "Toponym types in article", y = "Number of articles")+
#   coord_flip()

# # the origin of the candidate list from both Ordnance Survey and OpenStreetMap, with their overlap; 
# candidates_gold_data_with_lads |> 
#   group_by(os, osm) |>
#   count() |> 
#   mutate(perc = n/nrow(candidates_gold_data_with_lads)) 

# the distribution of the number of candidates_gold_dataper toponym across both sources; 
b <- candidates_gold_data_with_lads |> 
  select(os, osm, labels, text, LAD24NM) |>
  distinct() |> 
  group_by(os, osm, labels) |>
  count() |> 
  mutate(perc = n/nrow(candidates_gold_data_with_lads),
         combination = str_remove(paste(os, osm), "NA"),
         combination = str_remove(combination, " "),
         combination = if_else(combination == "OSOSM", "OS/OSM", combination),
         combination = replace_na(combination, "missing from both")) |> 
  ggplot(aes(x = combination, y = perc))+
  geom_bar(stat = "identity", aes(fill = labels))+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(title = "(b)", x = "Gazzetteer in Which Toponym Appears", y = "Percentage of Toponyms")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("GPE" = "#6395EE", "LOC" = "#266CE7", "FAC" = "#A0BEF5", name = "Toponym Type"))

# the distribution of candidates within the same local administrative district (LAD) versus those outside; 
c <- gold_data_expanded_by_candidates |> 
  group_by(doc) |> 
  count(LAD24NM) |> 
  summarise(n_lads = n_distinct(LAD24NM)) |> 
  ggplot(aes(x = n_lads))+
  geom_histogram(fill = "cornflowerblue", alpha = 0.8, binwidth = 0.4)+
  theme_minimal()+
  scale_x_sqrt(breaks = c(1,10,50,100,200,300))+
  labs(title = "(c)", x = "Number of districts", y = "Number of articles")+
  theme(plot.title = element_text(hjust = 0.5))

p <- ggpubr::ggarrange(a,b,c, ncol=3, nrow=1, common.legend = FALSE, legend="top")

ggplot2::ggsave(
  "candidate_list.pdf", p,
  width = 23,
  height = 8,
  dpi = 300,
  units = "cm"
)



