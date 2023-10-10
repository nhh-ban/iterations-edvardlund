library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

transform_metadata_to_df <- function(metadata) {
  transformed_data <- metadata[[1]] %>%
    map(as_tibble) %>%
    list_rbind() %>%
    mutate(
      latestData = map_chr(
        latestData,
        1,
        .default = NA_character_
      )) %>%
    mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>%
    unnest_wider(location) %>%
    unnest_wider(latLon)
  
  return(transformed_data)
}

