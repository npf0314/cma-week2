library(sf)
library(dplyr)
library(xml2)
library(geosphere)
library(ggplot2)
library(tmap)
library(lubridate)

# Pfad zur KML-Datei
file_path <- "C:/_Data/Master/PaT_24/week_2/cma-week2-rexercise/data/history-2024-05-01.kml"

# KML-Datei als XML lesen
kml_xml <- read_xml(file_path)
ns <- c(kml = "http://www.opengis.net/kml/2.2")
placemarks <- xml_find_all(kml_xml, ".//kml:Placemark[kml:TimeSpan]", ns = ns)

# Daten extrahieren und als Datenrahmen vorbereiten
data <- data.frame(
  longitude = numeric(length(placemarks)),
  latitude = numeric(length(placemarks)),
  begin_time = character(length(placemarks)),
  end_time = character(length(placemarks)),
  stringsAsFactors = FALSE
)

for (i in seq_along(placemarks)) {
  placemark <- placemarks[[i]]
  coords <- xml_text(xml_find_first(placemark, ".//kml:coordinates", ns = ns))
  coords <- strsplit(coords, " ")[[1]][1]  # Erstes Koordinatenpaar nehmen
  coords <- strsplit(coords, ",")[[1]]
  begin <- xml_text(xml_find_first(placemark, ".//kml:TimeSpan/kml:begin", ns = ns))
  end <- xml_text(xml_find_first(placemark, ".//kml:TimeSpan/kml:end", ns = ns))
  data$longitude[i] <- as.numeric(coords[1])
  data$latitude[i] <- as.numeric(coords[2])
  data$begin_time[i] <- begin
  data$end_time[i] <- end
}

data$begin_time <- as.POSIXct(data$begin_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
data$end_time <- as.POSIXct(data$end_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Transform the data to the Swiss LV95 coordinate system (EPSG:2056)
data <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
data <- st_transform(data, 2056)

# Ensure the correct time format and sort the data by time
data$begin_time <- as.POSIXct(data$begin_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
data$end_time <- as.POSIXct(data$end_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
data <- data %>% arrange(begin_time)

# Extract coordinates after transformation
data <- data %>% mutate(
  easting = st_coordinates(geometry)[, 1],
  northing = st_coordinates(geometry)[, 2]
)

# Interpolate time between coordinates
interpolated_times <- vector("list", length = nrow(data))
for (i in 1:(nrow(data) - 1)) {
  time_diff <- difftime(data$end_time[i], data$begin_time[i], units = "secs")
  interpolated_times[[i]] <- seq.POSIXt(from = data$begin_time[i], 
                                        to = data$end_time[i], 
                                        length.out = as.numeric(time_diff))
}
# Letztes Element hinzufügen
interpolated_times[[nrow(data)]] <- data$end_time[nrow(data)]

data$interpolated_time <- unlist(interpolated_times)

# Calculate speeds by determining the Euclidean distance between consecutive points
data <- data %>%
  mutate(
    time_diff = as.numeric(difftime(interpolated_time, lag(interpolated_time), units = "secs")),
    dist_diff = sqrt((lag(easting) - easting)^2 + (lag(northing) - northing)^2),
    speed = dist_diff / time_diff * 3.6  # Convert speed to km/h
  )

# Convert points to lines for visualizing paths, calculate average speed per segment
data_lines <- data %>%
  summarise(
    geometry = st_combine(geometry), 
    avg_speed = mean(speed, na.rm = TRUE)  # Calculate average speed for each segment
  ) %>%
  st_cast("LINESTRING")

# Check the range of avg_speed and adjust if too narrow
speed_range <- range(data_lines$avg_speed, na.rm = TRUE)
if (diff(speed_range) < 1) {  # If the range is less than 1 km/h
  speed_range <- speed_range[1] + c(-0.5, 0.5)  # Expand the range a bit
}

# Assuming each measurement has an uncertainty of ±5 meters
# Adjust speed to account for GNSS inaccuracy: Speed change is proportional to distance change
# Here we estimate a ±10% variation in speed as an example
data <- data %>%
  mutate(speed_lower = speed * 0.9,  # 10% lower speed due to GNSS inaccuracy
         speed_upper = speed * 1.1)  # 10% higher speed due to GNSS inaccuracy

# Calculate the median speed
median_speed <- median(data$speed, na.rm = TRUE)

# Erste und letzte Zeitpunkte der Daten ermitteln
min_time <- min(data$begin_time, na.rm = TRUE)
max_time <- max(data$end_time, na.rm = TRUE)

# Create the plot with confidence interval
speed_plot <- ggplot(data, aes(x = begin_time, y = speed)) +
  geom_ribbon(aes(ymin = speed_lower, ymax = speed_upper), fill = "grey80", alpha = 0.5) +  # Shaded area for uncertainty
  geom_line(color = "black") +  # Plot speed in black
  geom_hline(yintercept = median_speed, linetype = "dashed", color = "red", size = 1) +  # Add median speed line
  labs(title = "Speed Over Time with GNSS Inaccuracy",
       x = "Time",
       y = "Speed (km/h)") +  # Add titles and labels
  scale_x_datetime(limits = c(min_time, max_time)) +  # Set x-axis limits
  theme_minimal() +  # Use a minimal theme
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"))

# Print the plot to check it
print(speed_plot)

# Save the plot as a PNG file
ggsave("speed_over_time_with_uncertainty_KML.png", speed_plot, width = 10, height = 8, path = "./", dpi = 300)

# Create breaks for the color gradient
num_breaks <- 10  # Define the number of breaks
speed_breaks <- seq(from = speed_range[1], to = speed_range[2], length.out = num_breaks)

# Set tmap mode to interactive for viewing maps interactively
tmap_mode("view")

# Create the map using tmap with a color gradient based on average speed
tm_map <- tm_shape(data_lines) +
  tm_lines(col = "avg_speed", palette = c("yellow", "red"), style = "cont", 
           breaks = speed_breaks, title.col = "Speed (km/h)") +
  tm_shape(data) +
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
ggsave("C:/_Data/Master/PaT_24/week_2/cma-week2-rexercise/Exercise_C/all_monte_carlo_boxplots_KML.png", plot = p, width = 12, height = 8, dpi = 300)

# Plot anzeigen
print(p)
