library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)


### 5: Final volume query: 

source("gql-queries/vol_qry.r")

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>%
  ggplot(aes(x=from, y=volume)) + 
  geom_line() + 
  theme_classic()

#TASK 6 - Rewriting the code above to include legend with the name of station

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %>%
  # Creating a new column, a list containing the results of the query
  mutate(
    volume_data = list(
      vol_qry(
        id = .$id,
        from = to_iso8601(.$latestData, -4),
        to = to_iso8601(.$latestData, 0)
      ) %>% 
        GQL(., .url = configs$vegvesen_url) %>%
        transform_volumes()
    )
  ) %>%
  # Using unnest to flatten the list-column into a ggplot format
  unnest(volume_data) %>%
  # Creating a ned column with the name of the station
  mutate(station = first(.$name)) %>% 
  # Making the plot
  ggplot(aes(x = from, y = volume, colour = station)) + 
  geom_line() +
  labs(colour = "Traffic Station", y = "Volume", x = "From",
       title = "Hourly volume") +
  theme_classic()


  









