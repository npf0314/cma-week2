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
  
  begin_node <- xml_find_first(placemark, ".//kml:TimeSpan/kml:begin", ns)
  start_time_full <- if (!is.null(begin_node)) as.POSIXct(xml_text(begin_node), format="%Y-%m-%dT%H:%M:%S", tz="UTC") else NA
  end_node <- xml_find_first(placemark, ".//kml:TimeSpan/kml:end", ns)
  end_time_full <- if (!is.null(end_node)) as.POSIXct(xml_text(end_node), format="%Y-%m-%dT%H:%M:%S", tz="UTC") else NA
  
  # Extrahieren der Distanz aus dem Description-Tag
  description_node <- xml_find_first(placemark, ".//kml:description", ns)
  description_text <- if (!is.null(description_node)) xml_text(description_node) else ""
  distance_m <- as.numeric(gsub(".*Distance (\\d+)m.*", "\\1", description_text))
  distance_km <- if (!is.na(distance_m)) distance_m / 1000 else NA  # Umwandlung von Meter in Kilometer
  
  time_diff_hours <- if (!is.na(start_time_full) && !is.na(end_time_full)) as.numeric(difftime(end_time_full, start_time_full, units="hours")) else NA
  
  speed_kmh <- ifelse(!is.na(time_diff_hours) && !is.na(distance_km), distance_km / time_diff_hours, NA)
  
  start_times <- c(start_time_full, rep(NA, length(longitudes) - 1))
  end_times <- c(end_time_full, rep(NA, length(longitudes) - 1))  # Setze die Endzeit für alle Koordinatenpaare
  distances_km <- c(rep(NA, length(longitudes) - 1), distance_km)  # Setze die Distanz nur beim letzten Koordinatenpaar
  
  data.frame(Placemark = rep(name, length(longitudes)), 
             Longitude_LV95 = longitudes_lv95, Latitude_LV95 = latitudes_lv95,
             Start_Time = start_times, End_Time = end_times,
             Distance_km = distances_km,  # Hinzugefügt für KML-basierte Distanz
             Time_Diff_Hours = c(rep(NA, length(longitudes) - 1), time_diff_hours),
             Speed_kmh = c(rep(NA, length(longitudes) - 1), speed_kmh))
}

# Koordinaten und Placemarks extrahieren
placemarks <- xml_find_all(doc, ".//kml:Placemark", ns)

# Datenrahmen für alle Placemarks erstellen
coord_df <- bind_rows(lapply(placemarks, extract_placemark_details))

# Ausgabe der angepassten Tabelle
print(coord_df)
