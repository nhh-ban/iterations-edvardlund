library(tidyverse)
library(glue)

# Problem 4B
# Creating a function to return a query
vol_qry <- function(id, from, to) {
  query_t <- '{
    trafficData(trafficRegistrationPointId: "%s") {
      volume {
        byHour(from: "%s", to: "%s") {
          edges {
            node {
              from
              to
              total {
                volumeNumbers {
                  volume
                }
              }
            }
          }
        }
      }
    }
  }'
  query <- sprintf(query_t, id, from, to)
  return(query)
}


# Testing
GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)



