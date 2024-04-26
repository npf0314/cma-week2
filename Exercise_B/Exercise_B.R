# Exercise B @ Nadja Pfister

## Load necessary libraries
library("readr")    # For reading CSV files
library("sf")       # For spatial data operations
library("dplyr")    # For data manipulation
library("ggplot2")
library("tidyr")

#________________________________________________________________________________________________________________________________________


## Need following funtions from Exercise A

difftime_secs <- function(x, y){
  as.numeric(difftime(x, y, units = "secs"))
}

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

#________________________________________________________________________________________________________________________________________

# Intro

caro <- read_csv("C:/_Data/Master/PaT_24/week_2/cma-week2-rexercise/data/caro60.csv") |>
  st_as_sf(coords = c("E","N"), crs = 2056) |> 
  select(DatetimeUTC)

print(caro)

#________________________________________________________________________________________________________________________________________

# Task 1 
## Calculate speed at scale 1

# Sort the caro data by DatetimeUTC to ensure it's in chronological order
caro <- caro %>% arrange(DatetimeUTC)

# Calculate the time lag between consecutive timestamps (in seconds)
caro$timelag <- difftime_secs(caro$DatetimeUTC, lag(caro$DatetimeUTC))

# Calculate the distance traveled between consecutive locations
caro$steplength <- distance_by_element(caro, lag(caro))

# Calculate speed (distance / time)
caro$speed <- caro$steplength / caro$timelag

# Print the resulting dataset with speed calculations
print(caro)

#________________________________________________________________________________________________________________________________________

# Task 2
## Calculate speed at scale 2
# Assuming a sampling window of 240 seconds

# Sort the caro data by DatetimeUTC to ensure it's in chronological order
caro <- caro %>% arrange(DatetimeUTC)

# Calculate the time lag between timestamps with an offset of 2 (for scale 2)
caro$timelag2 <- difftime_secs(caro$DatetimeUTC, lag(caro$DatetimeUTC, n = 2))

# Calculate the distance traveled between locations with an offset of 2 (for scale 2)
caro$steplength2 <- distance_by_element(caro, lag(caro, n = 2))

# Calculate speed at scale 2 (distance / time)
caro$speed2 <- caro$steplength2 / caro$timelag2

# Print the resulting dataset with speed calculations at scale 2
caro |> 
  # drop geometry and select only specific columns
  # to display relevant data only
  st_drop_geometry() |> 
  select(timelag2, steplength2, speed2) |> 
  head()

#________________________________________________________________________________________________________________________________________

# Task 3: Calculate speed at scale 3
# Assuming a sampling window of 480 seconds

# Sort the caro data by DatetimeUTC to ensure it's in chronological order
caro <- caro %>% arrange(DatetimeUTC)

# Calculate the time lag between timestamps with an offset of 4 (for scale 3)
caro$timelag3 <- difftime_secs(caro$DatetimeUTC, lag(caro$DatetimeUTC, n = 4))

# Calculate the distance traveled between locations with an offset of 4 (for scale 3)
caro$steplength3 <- distance_by_element(caro, lag(caro, n = 4))

# Calculate speed at scale 3 (distance / time)
caro$speed3 <- caro$steplength3 / caro$timelag3

# Print the resulting dataset with speed calculations at scale 3
caro |> 
  st_drop_geometry() |> 
  select(timelag3, steplength3, speed3) |> 
  head()

#________________________________________________________________________________________________________________________________________

# Task 4
## Compare speed across scales

# Simplify the dataframe and select relevant columns
caro2 <- caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

# Convert the dataframe from wide to long format using pivot_longer
caro_long <- caro2 |> 
  pivot_longer(cols = c(speed, speed2, speed3), names_to = "name", values_to = "value")

# Plot box plots to compare speed measurements at different scales
ggplot(caro_long, aes(name, value)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outliers for better visualization
  labs(x = "Scale", y = "Speed") +  # Set axis labels
  theme_minimal() + # Apply minimal theme for better clarity
  scale_y_continuous(limits = c(0, 0.1))