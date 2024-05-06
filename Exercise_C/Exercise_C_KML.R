# Load required libraries
library("readr")
library("sf")
library("dplyr")
library("ggplot2")
library("lubridate")
library("tmap")

# Read the CSV file
file_path <- "C:/_Data/Master/PaT_24/week_2/cma-week2-rexercise/data/KML_Zeit_interpoliert.csv"
csv_data <- read_csv(file_path)

# Transform columns to proper formats using rowwise computation for coordinates
csv_data <- csv_data %>%
  rowwise() %>%
  mutate(
    time = as.POSIXct(Time_Point, format = "%Y-%m-%d %H:%M:%S"),
    geom = st_sfc(st_point(c(Longitude_LV95, Latitude_LV95)), crs = 2056),
    speed = as.numeric(Speed_kmh)  # Use speed directly from CSV
  ) %>%
  ungroup() %>%
  st_as_sf() %>%
  arrange(time)

# Filter data to include only moving points where speed > 0 km/h
moving_data <- csv_data %>%
  filter(speed > 0)

# Identify pauses between movements
csv_data <- csv_data %>%
  mutate(
    moving = speed > 0,
    pause_start = moving == FALSE & lag(moving, default = FALSE) == TRUE
  )

# Filter for pauses for visualization
pause_data <- csv_data %>%
  filter(pause_start)

# Combine moving and pause data for plotting
plot_data <- bind_rows(moving_data, pause_data)

# Add plotting for Speed Over Time where movement occurred, highlighting pauses
moving_speed_plot_with_pauses <- ggplot(plot_data, aes(x = time, y = speed)) +
  geom_line(data = moving_data, aes(color = "Movement")) +
  geom_point(data = pause_data, aes(color = "Pause"), size = 3) +
  scale_color_manual(values = c("Movement" = "blue", "Pause" = "red")) +
  labs(title = "Speed Over Time (Movement and Pauses)", x = "Time", y = "Speed (km/h)") +
  theme_minimal()

# Print the plot
print(moving_speed_plot_with_pauses)

# Save the plot as a PNG file
ggsave("moving_speed_over_time_with_pauses.png", moving_speed_plot_with_pauses, width = 10, height = 8, dpi = 300)

# Set tmap mode to view
tmap_mode("view")
