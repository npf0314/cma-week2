# Exercise C @ Nadja Pfister

#____________________________________________________________________________________________________________

# Load required libraries
library("readr")    # To read CSV files
library("sf")       # For spatial data operations
library("dplyr")    # For data manipulation
library("ggplot2")  # For creating graphics
library("lubridate")# For handling date and time
library("tmap")     # For creating thematic maps

#____________________________________________________________________________________________________________

# Read the GPX file using its path
file_path <- "C:/_Data/Master/PaT_24/week_2/cma-week2-rexercise/data/walking_Davos.gpx"
gpx_data <- st_read(file_path, layer = "track_points")

# Transform the data to the Swiss LV95 coordinate system (EPSG:2056)
gpx_data <- st_transform(gpx_data, 2056)

# Ensure the correct time format and sort the data by time
gpx_data$time <- as.POSIXct(gpx_data$time, format = "%Y-%m-%dT%H:%M:%SZ")
gpx_data <- gpx_data %>% arrange(time)

# Extract coordinates after transformation
gpx_data <- gpx_data %>% mutate(
  easting = st_coordinates(geometry)[, 1],
  northing = st_coordinates(geometry)[, 2]
)

#____________________________________________________________________________________________________________

# Calculate speeds by determining the Euclidean distance between consecutive points
gpx_data <- gpx_data %>%
  mutate(
    time_diff = as.numeric(difftime(time, lag(time), units = "secs")),
    dist_diff = sqrt((lag(easting) - easting)^2 + (lag(northing) - northing)^2),
    speed = dist_diff / time_diff * 3.6  # Convert speed to km/h
  )

# Convert points to lines for visualizing paths, calculate average speed per segment
gpx_data_lines <- gpx_data %>%
  group_by(track_fid) %>%  # Assuming 'track_fid' identifies different tracks
  summarise(
    geometry = st_combine(geometry), 
    avg_speed = mean(speed, na.rm = TRUE),  # Calculate average speed for each segment
    .groups = "drop"
  ) %>%
  st_cast("LINESTRING")

# Check the range of avg_speed and adjust if too narrow
speed_range <- range(gpx_data_lines$avg_speed, na.rm = TRUE)
if (diff(speed_range) < 1) {  # If the range is less than 1 km/h
  speed_range <- speed_range[1] + c(-0.5, 0.5)  # Expand the range a bit
}

#____________________________________________________________________________________________________________

# Assuming each measurement has an uncertainty of ±5 meters
# Adjust speed to account for GNSS inaccuracy: Speed change is proportional to distance change
# Here we estimate a ±10% variation in speed as an example
gpx_data <- gpx_data %>%
  mutate(speed_lower = speed * 0.9,  # 10% lower speed due to GNSS inaccuracy
         speed_upper = speed * 1.1)  # 10% higher speed due to GNSS inaccuracy

# Calculate the median speed
median_speed <- median(gpx_data$speed, na.rm = TRUE)

# Create the plot with confidence interval
speed_plot <- ggplot(gpx_data, aes(x = time, y = speed)) +
  geom_ribbon(aes(ymin = speed_lower, ymax = speed_upper), fill = "grey80", alpha = 0.5) +  # Shaded area for uncertainty
  geom_line(color = "black") +  # Plot speed in black
  geom_hline(yintercept = median_speed, linetype = "dashed", color = "red", size = 1) +  # Add median speed line
  labs(title = "Speed Over Time with GNSS Inaccuracy",
       x = "Time",
       y = "Speed (km/h)") +  # Add titles and labels
  theme_minimal() +  # Use a minimal theme
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"))


# Print the plot to check it
print(speed_plot)

# Save the plot as a PNG file
ggsave("speed_over_time_with_uncertainty.png", speed_plot, width = 10, height = 8, path = "./", dpi = 300)

#____________________________________________________________________________________________________________

# Create breaks for the color gradient
num_breaks <- 10  # Define the number of breaks
speed_breaks <- seq(from = speed_range[1], to = speed_range[2], length.out = num_breaks)

# Set tmap mode to interactive for viewing maps interactively
tmap_mode("view")

# Create the map using tmap with a color gradient based on average speed
tm_map <- tm_shape(gpx_data_lines) +
  tm_lines(col = "avg_speed", palette = c("yellow", "red"), style = "cont", 
           breaks = speed_breaks, title.col = "Speed (km/h)") +
  tm_shape(gpx_data) +
  tm_dots(col = "red", size = 0.1) +
  tm_basemap(server = "OpenStreetMap")

# Display the map
tm_map


#_________________________________________________________________________________________________________________

# Monte Carlo

# Monte Carlo Simulation Funktion definieren
monte_carlo_simulation <- function(sigma, duration, n_sim = 1000) {
  dt <- duration / n_sim
  times <- seq(dt, duration, by = dt)
  # Simuliere den Weg basierend auf normalverteiltem Rauschen
  paths <- sapply(1:n_sim, function(x) cumsum(rnorm(n_sim, mean = 0, sd = sigma * sqrt(dt))))
  data.frame(Time = rep(times, each = n_sim), Path = as.vector(paths), Sigma = as.factor(sigma), Duration = as.factor(duration))
}

# Parameter festlegen
sigmas <- c(1, 3, 10)
durations <- c(5, 10, 60, 300, 600, 1200)
all_data <- do.call(rbind, lapply(sigmas, function(sigma) {
  do.call(rbind, lapply(durations, function(duration) {
    monte_carlo_simulation(sigma, duration)
  }))
}))

# Boxplot für alle Daten erstellen
p <- ggplot(all_data, aes(x = interaction(Sigma, Duration), y = Path, fill = Sigma)) +
  geom_boxplot() +
  labs(title = "Monte Carlo Boxplots Across All Sigmas and Durations",
       x = "Sigma-Duration Combination", y = "Path Variation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Text der x-Achse drehen für bessere Lesbarkeit
  scale_fill_discrete(name = "Sigma Value") + 
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"))


# Plot speichern
ggsave("C:/_Data/Master/PaT_24/week_2/cma-week2-rexercise/Exercise_C/all_monte_carlo_boxplots.png", plot = p, width = 12, height = 8, dpi = 300)

# Plot anzeigen
print(p)
