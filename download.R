# Load necessary libraries
library(httr)
library(jsonlite)

# URL for the live station status data
url <- "https://gbfs.citibikenyc.com/gbfs/en/station_status.json"

# Make the GET request
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content
  data <- content(response, "text", encoding = "UTF-8")
  json_data <- fromJSON(data)
  
  # Extract the station data
  station_data <- json_data$data$stations
  
  # Extract relevant columns
  station_info <- station_data[, c("station_id", "num_bikes_available", "num_docks_available")]
    
  # Print the first few rows
  print(head(station_info))
  
  # Save the data to a CSV file
  write.csv(station_info, "citibike_live_data.csv", row.names = FALSE)
} else {
  stop("Failed to fetch data: HTTP status code ", status_code(response))
}

