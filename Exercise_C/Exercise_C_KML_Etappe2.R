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

# Hilfsfunktion zum Extrahieren der Koordinaten und Namen
extract_placemark_details <- function(placemark) {
  name <- xml_text(xml_find_first(placemark, ".//kml:name", ns))
  coords_text <- xml_text(xml_find_first(placemark, ".//kml:coordinates", ns))
  coords <- strsplit(coords_text, "\\s+|,")[[1]]
  longitudes <- as.numeric(coords[seq(1, length(coords), by=3)])
  latitudes <- as.numeric(coords[seq(2, length(coords), by=3)])
  data.frame(Placemark = name, Longitude = longitudes, Latitude = latitudes)
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
colnames(coord_df)[4:5] <- c("LV95_Longitude", "LV95_Latitude")

# Datenrahmen anzeigen
print(coord_df)
