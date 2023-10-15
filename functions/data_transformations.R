library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

# Function to transfrm metadata to a readable dataframe
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
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>%
    unnest_wider(location) %>%
    unnest_wider(latLon)
  
  return(transformed_data)
}


#Function to return a date with the offset added

to_iso8601 <- function(datetime, offset_days) {
  #Converting the inpu to a date-time object
  
  datetime_obj <- as_datetime(datetime)
  #Applying the offset
  
  datetime_obj <- datetime_obj + days(offset_days)
  
  #Formating the date-time as an ISO8601 string and append "Z" 
  iso8601_str <- format(datetime_obj, "%Y-%m-%dT%H:%M:%S") %>% 
    paste0("Z")
  
  return(iso8601_str)
}

#Testing to_iso8601 function
source('functions/data_transformations.r')

to_iso8601("2016-09-01 10:11:12", -4)



