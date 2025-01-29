# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(gganimate)
library(gifski)
library(lubridate)
library(readr)

# Function to read and process data
read_citibike_data <- function(file) {
  read_csv(file, col_types = cols(
    started_at = col_datetime(),
    .default = col_character()
  ))
}

# Read and combine June data
june_files <- c("202406-citibike-tripdata_1.csv", 
                "202406-citibike-tripdata_2.csv", 
                "202406-citibike-tripdata_3.csv", 
                "202406-citibike-tripdata_4.csv", 
                "202406-citibike-tripdata_5.csv")

june_data <- map_dfr(june_files, read_citibike_data)

# Read and combine December data
december_files <- c("202412-citibike-tripdata_1.csv",
                    "202412-citibike-tripdata_2.csv",
                    "202412-citibike-tripdata_3.csv")

december_data <- map_dfr(december_files, read_citibike_data)

# Combine all data
trip_data <- bind_rows(june_data, december_data)

# Process data for visualization
hourly_demand_weekly <- trip_data |>
  mutate(
    hour = hour(started_at),
    date = as.Date(started_at),
    weekday = wday(started_at, label = TRUE, abbr = FALSE),
    month = format(started_at, "%B")
  ) |>
  filter(month %in% c("June", "December")) |>
  group_by(hour, weekday, month) |>
  summarise(
    total_trips = n(),
    unique_days = n_distinct(date),
    avg_trips_per_hour = total_trips / unique_days,
    .groups = "drop"
  )

# Create the plot
p <- ggplot(hourly_demand_weekly, 
            aes(x = hour, y = avg_trips_per_hour, group = weekday, color = weekday)) +
  geom_line(size = 1.2) +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~month) +
  labs(title = "Average Bike Demand by Hour of the Day",
       subtitle = "Day: {closest_state}",
       x = "Hour of the Day",
       y = "Average Trips per Hour") +
  scale_x_continuous(breaks = seq(0, 23, 6), labels = paste0(seq(0, 23, 6), ":00")) +
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines")
  )

# Create animation
anim <- p +
  transition_states(weekday, transition_length = 2, state_length = 2) +
  ease_aes('cubic-in-out')

# Ensure the output directory exists
dir.create("images", showWarnings = FALSE)

# Save the animation
anim_save("images/monthly_demand.gif",
          animate(anim, duration = 10, fps = 10, width = 1000, height = 600, renderer = gifski_renderer()))

print("Animation has been saved to images/monthly_demand.gif")
