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

# Hilfsfunktion zum Extrahieren der Koordinaten, Namen und Zeiten
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
  
  timestamp_node <- xml_find_first(placemark, ".//kml:TimeStamp/kml:when", ns)
  if (is.null(start_time_full) && !is.null(timestamp_node)) {
    start_time_full <- xml_text(timestamp_node)
    end_time_full <- start_time_full  # Wenn kein Enddatum vorhanden ist, verwende das Startdatum
  }
  
  # Nur Startzeit für die erste Koordinate und Endzeit für die letzte Koordinate zuordnen
  n <- length(longitudes)
  start_times <- rep(NA, n)
  end_times <- rep(NA, n)
  if (n > 0) {
    start_times[1] <- start_time_full
    end_times[n] <- end_time_full
  }
  
  # Zeitdifferenz berechnen und nur am Ende setzen
  time_diff <- rep(NA, n)
  if (!is.na(start_time_full) && !is.na(end_time_full)) {
    time_diff[n] <- as.numeric(difftime(as.POSIXct(end_time_full, format="%Y-%m-%dT%H:%M:%S", tz="UTC"),
                                        as.POSIXct(start_time_full, format="%Y-%m-%dT%H:%M:%S", tz="UTC"),
                                        units="mins"))  # Zeitdifferenz in Minuten
  }
  
  # Für jeden Punkt Start- und Endzeit sowie Zeitdifferenz übernehmen
  data.frame(Placemark = name, Longitude = longitudes, Latitude = latitudes, Start_Time = start_times, End_Time = end_times, Zeitdifferenz = time_diff)
}

# Datenrahmen für alle Placemarks erstellen
coord_df <- bind_rows(lapply(placemarks, extract_placemark_details))





# Koordinaten in ein sf-Objekt umwandeln
coordinates_sf <- st_as_sf(coord_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Koordinaten nach LV95 transformieren
coordinates_sf_lv95 <- st_transform(coordinates_sf, 2056)

# LV95 Koordinaten extrahieren und dem Datenrahmen hinzufügen
coord_df <- cbind(coord_df, st_coordinates(coordinates_sf_lv95))

# Umbenennen der LV95-Spalten
colnames(coord_df)[6:7] <- c("LV95_Longitude", "LV95_Latitude")

# Berechnung der Distanzen zwischen aufeinanderfolgenden Punkten
distances <- numeric(nrow(coordinates_sf_lv95) - 1)
for (i in 1:(nrow(coordinates_sf_lv95) - 1)) {
  distances[i] <- st_distance(coordinates_sf_lv95[i, ], coordinates_sf_lv95[i + 1, ])
}

# Distanzen von Metern in Kilometer umwandeln
distances_km <- distances / 1000

# Die erste Distanz ist NA, danach folgen die berechneten Distanzen
distances_km <- c(NA, distances_km)

# Hinzufügen der Distanz zum Datenrahmen
coord_df$Distanz_km <- distances_km

# Datenrahmen anzeigen
print(coord_df)


# Datenrahmen anzeigen
print(coord_df)

