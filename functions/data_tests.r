# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

#Function to test if the dataframe has the expected col names
test_stations_metadata_colnames <-
  function(df) {
    #Defining the expected column names for the dataframe
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    #Checking if the column names matches the expected names
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns") #Print if they match
    } else{
      # Print if they dont match
      print("FAIL: Columns do not match the correct specification")
    }
  }

#Function to test the number of rows in a dataframe
test_stations_metadata_nrows <-
  function(df) {
    
    #Defining the expected range for the number of rows
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    
    # Checking if the number of rows is within the interval
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows)
      {
      #Print if between interval
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      #Print if below minimum expected
      print("FAIL: Data has suspiciously few rows")
    } else {
      #Print if more than maximum expected
      print("FAIL: Data has suspiciously many rows")
    }
  }

# Function to test if the column data types match the expected types
test_stations_metadata_coltypes <-
  function(df) {
    #Deefining the expected names of columns 
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    # Checking if the data types of all columns matches the expected types
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      #Print if true
      print("PASS: All cols have the correct specifications")
    } else{
      # Print if not true 
      print("FAIL: Columns do not have the correct specification")
    }
  }

#Function to test the number of missing values in a dataframe
test_stations_metadata_nmissing <-
  function(df) {
    # Defining the maximum allowed number of missing values
    max_miss_vals <- 200
    
    # Calculating the total number of missing values across all columns
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      #If the number of missing values is below max, print
      print("PASS: Amount of missing values is reasonable")
    } else {
      #If the number of missing values is over max, print
      print("FAIL: Too many missing values in data set")
    }
  }

# Function to test if the 'latestData' column has the expected UTC time zone
test_stations_metadata_latestdata_timezone <-
  function(df) {
    # Checking if the latestData column has the expected UTC time zone
    if (attr(df$latestData,"tzone")=="UTC") {
      # If latestData has the UTC time zone, print a PASS message
      print("PASS: latestData has UTC-time zone")
    } else {
      # If latestData does not have the UTC time zone, print a FAIL message
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }

# Function to perform a series of tests on a metadata dataframe
test_stations_metadata <- 
  function(df){
    #Calling individual test functions to check different 
    #aspects of the dataframe
    test_stations_metadata_colnames(df)  # Check column names
    test_stations_metadata_coltypes(df)  # Check column data types
    test_stations_metadata_nmissing(df)  # Check missing values
    test_stations_metadata_nrows(df)     # Check number of rows
    test_stations_metadata_latestdata_timezone(df) #Check latestData timezone
  }





