# Exercise A @ Nadja Pfister

## Load necessary libraries
library("readr")    # For reading CSV files
library("sf")       # For spatial data operations
library("dplyr")    # For data manipulation
library("lubridate")# For date-time operations
library("ggplot2")    # For data visualization
library("tmap") # For maps

#________________________________________________________________________________________________________________________________________

# Task 1

## Import wildschwein_BE with delim, change it to a sf feature and overwrite the original
wildschwein_BE <- read_delim("C:/_Data/Master/PaT_24/week_2/cma-week2-rexercise/data/wildschwein_BE_2056.csv", ",") 
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056)

print(wildschwein_BE) # Verification of data import and transformation

#________________________________________________________________________________________________________________________________________

# Task 2

## Define the function to calculate time differences in seconds
difftime_secs <- function(x, y) {
  as.numeric(difftime(y, x, units = "secs"))
}

## Calculate time differences and add a new column 'timelag'
# Sort data by individual ID and Timestamp, then calculate the time difference
wildschwein_BE <- wildschwein_BE %>%
  arrange(TierID, DatetimeUTC) %>%
  group_by(TierID) %>%
  mutate(timelag = difftime_secs(lag(DatetimeUTC), DatetimeUTC)) 

## Count unique individuals
num_individuals <- n_distinct(wildschwein_BE$TierID)

## Analyze tracking details for each individual
individual_tracking_info <- wildschwein_BE %>%
  group_by(TierID) %>%
  summarise(
    start_time = min(DatetimeUTC), # First timestamp for the individual
    end_time = max(DatetimeUTC), # Last timestamp for the individual
    duration = as.numeric(difftime(max(DatetimeUTC), min(DatetimeUTC), units = "days")), # Total duration of tracking in days
    max_gap = max(timelag, na.rm = TRUE), # Maximum gap in seconds between records
    median_sampling_interval = median(timelag, na.rm = TRUE) # Median time in seconds between records
  )

## Output results to the console
print(paste("Number of individuals tracked:", num_individuals))
print("Tracking details for each individual:")
print(individual_tracking_info)

## Visualization of results

# 1. Bar plot showing the duration each animal was tracked
ggplot(individual_tracking_info, aes(x = reorder(TierID, -duration), y = duration)) +
  geom_col(fill = "turquoise") +
  labs(title = "Duration of Tracking for Each Individual", x = "Animal ID", y = "Duration (days)") +
  theme_minimal()

# 2. Histogram showing the distribution of time gaps (temporal sampling intervals)
ggplot(wildschwein_BE, aes(x = timelag)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Time Gaps Between Observations", x = "Time Gap (seconds)", y = "Frequency") +
  theme_minimal()

# 3. Line plot to show tracking gaps and overlaps
ggplot(wildschwein_BE, aes(x = DatetimeUTC, group = TierID, color = TierID)) +
  geom_line(aes(y = ifelse(is.na(timelag), 1, 0)), size = 1.5) +
  labs(title = "Tracking Gaps and Overlaps", x = "Time", y = "Gap Detected (binary)") +
  scale_y_continuous(breaks = c(0, 1), labels = c("No Gap", "Gap")) +
  theme_minimal()

# Save the plot outputs
ggsave("E1_T2_tracking_duration.png")
ggsave("E1_T2_time_gaps_distribution.png")
ggsave("E1_T2_tracking_gaps_overlaps.png")

#________________________________________________________________________________________________________________________________________

# Task 3

# Define the function to calculate distance between locations
distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

# Calculate the distance between subsequent locations
# Add a new column 'steplength' to the data frame that calculates the distance
wildschwein_BE <- wildschwein_BE %>%
  arrange(TierID, DatetimeUTC) %>%
  group_by(TierID) %>%
  mutate(
    steplength = distance_by_element(geometry, lag(geometry))
  )

# Check results
print(head(wildschwein_BE))

# Optional: Visualization of step lengths for each tracked individual

# Plot showing step lengths for each animal
ggplot(wildschwein_BE, aes(x = DatetimeUTC, y = steplength, color = as.factor(TierID))) +
  geom_line() +
  labs(title = "Step Length Over Time for Each Individual", x = "Time", y = "Step Length (meters)") +
  theme_minimal()

# Save the plot
ggsave("E1_T3_step_length_over_time.png")

#________________________________________________________________________________________________________________________________________

# Task 4: Analyzing Movement Patterns

# Load necessary libraries

# Assuming 'wildschwein_BE' is already loaded and is an sf object and contains 'geometry'
# Calculate summary statistics for step lengths
summary_steplength <- wildschwein_BE %>%
  group_by(TierID) %>%
  summarise(
    mean_steplength = mean(steplength, na.rm = TRUE),
    sd_steplength = sd(steplength, na.rm = TRUE),
    max_steplength = max(steplength, na.rm = TRUE),
    min_steplength = min(steplength, na.rm = TRUE)
  )

# Define thresholds for significant change
thresholds <- summary_steplength %>%
  mutate(significant_change = mean_steplength + 2 * sd_steplength)

# Convert wildschwein_BE to a regular dataframe for the join operation
wildschwein_BE_df <- as.data.frame(wildschwein_BE)

# Perform the join with the non-spatial thresholds data frame
wildschwein_BE_df <- wildschwein_BE_df %>%
  left_join(thresholds, by = "TierID")

# Extract coordinates from sf object if necessary
if (!("E" %in% names(wildschwein_BE_df)) | !("N" %in% names(wildschwein_BE_df))) {
  # Assuming the geometry column still exists and contains valid geometries
  coords <- st_coordinates(wildschwein_BE)
  wildschwein_BE_df$E <- coords[, "X"]
  wildschwein_BE_df$N <- coords[, "Y"]
}

# Convert back to sf object if necessary
if (all(c("E", "N") %in% names(wildschwein_BE_df))) {
  wildschwein_BE <- st_as_sf(wildschwein_BE_df, coords = c("E", "N"), crs = 2056, remove = FALSE)
} else {
  stop("Coordinate columns 'E' and 'N' are still missing.")
}

# Plot to show significant changes
ggplot(wildschwein_BE, aes(x = DatetimeUTC, y = steplength, color = as.factor(TierID))) +
  geom_line() +
  geom_hline(data = thresholds, aes(yintercept = significant_change, color = as.factor(TierID)), linetype = "dashed") +
  labs(title = "Significant Changes in Step Length Over Time", x = "Time", y = "Step Length (meters)") +
  theme_minimal()

# Save the plot
ggsave("E1_T4_significant_step_changes_over_time.png")

#________________________________________________________________________________________________________________________________________

# Task 5

# Extract a sample of the dataset for a specific individual named "Sabi"
wildschwein_sample <- wildschwein_BE %>%
  filter(TierName == "Sabi") %>%
  slice_head(n = 100)  # Use slice_head to get the first 100 observations for consistency

# Set tmap to interactive viewing mode
tmap_mode("view")

# Visualize the initial point data using tmap
tm_shape(wildschwein_sample) + 
  tm_dots() +
  tm_basemap(server = "OpenStreetMap") # Using OpenStreetMap as the basemap

# Transform point data to a line representation
# Ensure the data is ordered by datetime before casting to lines
wildschwein_sample_line <- wildschwein_sample %>%
  arrange(DatetimeUTC) %>%
  summarise(geometry = st_combine(geometry), do_union = FALSE) %>% # Combine into MULTIPOINT
  st_cast("LINESTRING") # Cast MULTIPOINT to LINESTRING

# Update tmap options to set OpenStreetMap as the default basemap
tmap_options(basemaps = "OpenStreetMap")

# Visualize the line and point data on the map
tm_shape(wildschwein_sample_line) +
  tm_lines() +
  tm_shape(wildschwein_sample) +
  tm_dots() +
  tm_layout(main.title = "Path of Sabi Over Time", main.title.position = "center", 
            outer.margins = 0, inner.margins = 0.02)
