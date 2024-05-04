# Bibliotheken laden
library(xml2)
library(sf)
library(dplyr)

# Pfad zur KML-Datei
file_path <- "C:/_Data/Master/PaT_24/week_2/cma-week2-rexercise/data/history-2024-05-01.kml"

# KML-Datei einlesen
doc <- read_xml(file_path)

# Namespace definieren
ns <- c(kml = "http://www.opengis.net/kml/2.2")

# Hilfsfunktion zum Extrahieren der Koordinaten, Namen, Zeiten, Distanzen, Zeitdifferenzen und Geschwindigkeiten
extract_placemark_details <- function(placemark) {
  name_node <- xml_find_first(placemark, ".//kml:name", ns)
  name <- if (!is.null(name_node)) xml_text(name_node) else NA
  
  coords_node <- xml_find_first(placemark, ".//kml:coordinates", ns)
  coords_text <- if (!is.null(coords_node)) xml_text(coords_node) else ""
  coords <- strsplit(coords_text, "\\s+|,")[[1]]
  longitudes <- as.numeric(coords[seq(1, length(coords), by=3)])
  latitudes <- as.numeric(coords[seq(2, length(coords), by=3)])
  
  coordinates_sf <- st_as_sf(data.frame(Longitude = longitudes, Latitude = latitudes), 
                             coords = c("Longitude", "Latitude"), 
                             crs = 4326)
  
  coordinates_lv95 <- st_transform(coordinates_sf, 2056)
  
  transformed_coords <- st_coordinates(coordinates_lv95)
  longitudes_lv95 <- transformed_coords[,1]
  latitudes_lv95 <- transformed_coords[,2]
  
  # Pythagoras-Distanzberechnung
  pythagoras_distances_km <- numeric(length(longitudes) - 1)
  if (length(longitudes) > 1) {
    for (i in 1:(length(longitudes) - 1)) {
      dx <- longitudes_lv95[i+1] - longitudes_lv95[i]
      dy <- latitudes_lv95[i+1] - latitudes_lv95[i]
      pythagoras_distances_km[i] <- sqrt(dx^2 + dy^2) / 1000 # Umwandlung von Metern in Kilometer
    }
  }
  pythagoras_distances_km <- c(NA, pythagoras_distances_km) # Erste Distanz als NA setzen
  
  # Distanzinformation aus der Beschreibung extrahieren
  description_node <- xml_find_first(placemark, ".//kml:description", ns)
  description <- if (!is.null(description_node)) xml_text(description_node) else ""
  distance_m <- as.numeric(gsub(".*Distance (\\d+)m.*", "\\1", description))
  description_distance_km <- distance_m / 1000  # Umrechnung von Meter in Kilometer
  
  # Zeitinformationen
  begin_node <- xml_find_first(placemark, ".//kml:TimeSpan/kml:begin", ns)
  start_time_full <- if (!is.null(begin_node)) xml_text(begin_node) else NA
  end_node <- xml_find_first(placemark, ".//kml:TimeSpan/kml:end", ns)
  end_time_full <- if (!is.null(end_node)) xml_text(end_node) else NA
  
  # Zeitdifferenzberechnungen
  if (!is.na(start_time_full) && !is.na(end_time_full)) {
    start_time <- as.POSIXct(start_time_full, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
    end_time <- as.POSIXct(end_time_full, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
    time_diff_hours <- as.numeric(difftime(end_time, start_time, units="hours"))
  } else {
    time_diff_hours <- NA
  }
  
  # Geschwindigkeit berechnen
  speed_kmh <- ifelse(!is.na(time_diff_hours), description_distance_km / time_diff_hours, NA)
  
  # Erste und letzte Zeit festlegen
  start_times <- ifelse(seq_along(longitudes) == 1, start_time_full, NA)
  end_times <- ifelse(seq_along(longitudes) == length(longitudes), end_time_full, NA)
  
  # Datenrahmen für den einzelnen Placemark zurückgeben
  data.frame(Placemark = rep(name, length(longitudes)), 
             Longitude = longitudes, Latitude = latitudes,
             Longitude_LV95 = longitudes_lv95, Latitude_LV95 = latitudes_lv95,
             Start_Time = start_times, End_Time = end_times,
             Description_Distance_km = rep(description_distance_km, length(longitudes)),
             Pythagoras_Distance_km = pythagoras_distances_km,
             Time_Diff_Hours = rep(time_diff_hours, length(longitudes)),
             Speed_kmh = speed_kmh)
}

# Koordinaten und Placemarks extrahieren
placemarks <- xml_find_all(doc, ".//kml:Placemark", ns)

# Datenrahmen für alle Placemarks erstellen
coord_df <- bind_rows(lapply(placemarks, extract_placemark_details))

# Datenrahmen anzeigen
print(coord_df)
