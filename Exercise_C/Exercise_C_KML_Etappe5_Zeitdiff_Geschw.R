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

# Koordinaten und Placemarks extrahieren
placemarks <- xml_find_all(doc, ".//kml:Placemark", ns)

# Hilfsfunktion zum Extrahieren der Koordinaten, Namen, Zeiten, Distanzen, Zeitdifferenzen und Geschwindigkeiten
extract_placemark_details <- function(placemark) {
  name_node <- xml_find_first(placemark, ".//kml:name", ns)
  name <- if (!is.null(name_node)) xml_text(name_node) else NA
  
  coords_node <- xml_find_first(placemark, ".//kml:coordinates", ns)
  coords_text <- if (!is.null(coords_node)) xml_text(coords_node) else ""
  coords <- strsplit(coords_text, "\\s+|,")[[1]]
  longitudes <- as.numeric(coords[seq(1, length(coords), by=3)])
  latitudes <- as.numeric(coords[seq(2, length(coords), by=3)])
  
  # Zeitinformation extrahieren
  begin_node <- xml_find_first(placemark, ".//kml:TimeSpan/kml:begin", ns)
  start_time_full <- if (!is.null(begin_node)) xml_text(begin_node) else NA
  
  end_node <- xml_find_first(placemark, ".//kml:TimeSpan/kml:end", ns)
  end_time_full <- if (!is.null(end_node)) xml_text(end_node) else NA
  
  n <- length(longitudes)
  start_times <- rep(NA, n)
  end_times <- rep(NA, n)
  if (n > 0) {
    start_times[1] <- start_time_full
    end_times[n] <- end_time_full
  }
  
  # Distanzinformation aus der Beschreibung extrahieren
  description_node <- xml_find_first(placemark, ".//kml:description", ns)
  description <- if (!is.null(description_node)) xml_text(description_node) else ""
  distance_m <- as.numeric(gsub(".*Distance (\\d+)m.*", "\\1", description))
  distance_km <- distance_m / 1000  # Umrechnung von Meter in Kilometer
  
  # Zeitdifferenz in Stunden berechnen
  time_diff_hours <- rep(NA, n)
  if (!is.na(start_time_full) && !is.na(end_time_full)) {
    start_time <- as.POSIXct(start_time_full, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
    end_time <- as.POSIXct(end_time_full, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
    if (!is.na(start_time) && !is.na(end_time)) {
      time_diff_hours[n] <- as.numeric(difftime(end_time, start_time, units="hours"))
    }
  }
  
  # Geschwindigkeit berechnen (km/h)
  speed_kmh <- rep(NA, n)
  if (!is.na(time_diff_hours[n]) && time_diff_hours[n] > 0) {
    speed_kmh[n] <- distance_km / time_diff_hours[n]
  }
  
  # Datenrahmen für den einzelnen Placemark zurückgeben
  data.frame(Placemark = name, Longitude = longitudes, Latitude = latitudes,
             Start_Time = start_times, End_Time = end_times, Distance_km = distance_km, 
             Time_Diff_Hours = time_diff_hours, Speed_kmh = speed_kmh)
}

# Datenrahmen für alle Placemarks erstellen
coord_df <- bind_rows(lapply(placemarks, extract_placemark_details))

# Datenrahmen anzeigen
print(coord_df)
