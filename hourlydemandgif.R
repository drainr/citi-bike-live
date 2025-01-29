
hourly_demand_weekly <- trip_data |>
  mutate(
    hour = as.numeric(format(started_at, "%H")),
    date = as.Date(started_at),
    weekday = wday(started_at, label = TRUE, abbr = FALSE)
  ) |>
  group_by(hour, weekday) |>  # No month grouping
  summarise(
    total_trips = n(),
    unique_days = n_distinct(date),
    avg_trips_per_hour = total_trips / unique_days,
    .groups = "drop"
  )

p <- ggplot(hourly_demand_weekly, 
            aes(x = hour, y = avg_trips_per_hour, group = weekday, color = weekday)) +
  geom_line(size = 1.2) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Average Bike Demand by Hour of the Day",
       subtitle = "Day: {closest_state}",
       x = "Hour of the Day",
       y = "Average Trips per Hour (Aggregated Across Months)") +
  scale_x_continuous(breaks = seq(0, 23, 6), 
                     labels = paste0(seq(0, 23, 6), ":00")) +
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Create animation
anim <- p +
  transition_states(weekday, transition_length = 2, state_length = 2) +
  ease_aes('cubic-in-out')

# Save the animation
anim_save("images/hourly_demand.gif",
          animate(anim,
                  duration = 10,
                  fps = 10,
                  width = 1000,
                  height = 600,
                  renderer = gifski_renderer()))

print("Animation has been saved to images/hourly_demand.gif")
