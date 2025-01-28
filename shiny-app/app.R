library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Fetch Citi Bike data function
fetch_live_data <- function() {
  status_url <- "https://gbfs.citibikenyc.com/gbfs/en/station_status.json"
  info_url <- "https://gbfs.citibikenyc.com/gbfs/en/station_information.json"
  
  # Fetch station status (availability of bikes and docks)
  status_data <- fromJSON(content(GET(status_url), "text", encoding = "UTF-8"))$data$stations
  
  # Fetch station info (lat/lon details)
  info_data <- fromJSON(content(GET(info_url), "text", encoding = "UTF-8"))$data$stations
  
  # Merge the two datasets on station ID
  merged_data <- merge(info_data, status_data, by = "station_id")
  
  # Convert to a data frame and return
  merged_data <- as.data.frame(merged_data)
  return(merged_data)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Animated Bike Availability Heatmap with Enhanced Gradient"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "time",
        "Select Time (Simulated):",
        min = 1,
        max = 24,
        value = 1,
        step = 1,
        animate = animationOptions(interval = 1000, loop = TRUE)
      ),
      actionButton("pause", "Pause"),
      actionButton("play", "Play")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

# Modified server function with enhanced visualization
server <- function(input, output, session) {
  # Fetch live data
  stations <- fetch_live_data()
  
  # Simulate time series data (hourly changes)
  set.seed(123)
  stations_time <- stations %>%
    group_by(station_id) %>%
    mutate(hourly_bikes = list(sample(0:num_bikes_available, 24, replace = TRUE))) %>%
    tidyr::unnest(hourly_bikes) %>%
    mutate(hour = rep(1:24, length.out = n())) %>%
    ungroup()
  
  # Reactive data based on slider input
  reactive_data <- reactive({
    stations_time %>%
      filter(hour == input$time) %>%
      select(station_id, lat, lon, weight = hourly_bikes)
  })
  
  # Timer controls
  auto_play <- reactiveVal(TRUE)
  
  observeEvent(input$pause, {
    auto_play(FALSE)
  })
  
  observeEvent(input$play, {
    auto_play(TRUE)
  })
  
  # Auto-play slider
  timer <- reactiveTimer(1000)
  
  observe({
    if (auto_play()) {
      timer()
      current_time <- isolate(input$time)
      new_time <- ifelse(current_time < 24, current_time + 1, 1)
      updateSliderInput(session, "time", value = new_time)
    }
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.97, lat = 40.75, zoom = 13)
  })
  
  # Observe time changes and update heatmap with enhanced gradient
  observe({
    data <- reactive_data()
    
    # Enhanced scaling function with better distribution
    data <- data %>%
      mutate(
        # Use cube root transformation for more balanced distribution
        weight_transformed = weight^(1/3),
        # Normalize the transformed values
        weight_norm = (weight_transformed - min(weight_transformed)) / 
          (max(weight_transformed) - min(weight_transformed)),
        # Apply modified sigmoid for better spread
        weight_norm = 1 / (1 + exp(-4 * (weight_norm - 0.4)))
      )
    
    # Identify top busy stations
    top_stations <- data %>% top_n(5, wt = weight)
    
    # Create a more balanced color palette with distinct steps
    colors <- colorRampPalette(c(
      "#000080",  # Navy Blue (low)
      "#0000FF",  # Blue
      "#00BFFF",  # Deep Sky Blue
      "#00FF00",  # Green
      "#FFFF00",  # Yellow
      "#FFA500",  # Orange
      "#FF4500",  # Orange Red
      "#FF0000"   # Red (high)
    ))(12)
    
    leafletProxy("map") %>%
      clearHeatmap() %>%
      clearMarkers() %>%
      addHeatmap(
        lng = data$lon,
        lat = data$lat,
        intensity = data$weight_norm,
        radius = 20,     # Increased radius
        blur = 22,       # Adjusted blur
        gradient = colors,
        max = 1,
        minOpacity = 0.35 # Slightly increased minimum opacity
      ) %>%
      addPulseMarkers(
        lng = top_stations$lon,
        lat = top_stations$lat,
        icon = makePulseIcon(color = "red", iconSize = 12),
        popup = paste0(
          "Station ID: ", top_stations$station_id,
          "<br>Available Bikes: ", top_stations$weight
        )
      )
  })
}
# Run the Shiny app
shinyApp(ui, server)
