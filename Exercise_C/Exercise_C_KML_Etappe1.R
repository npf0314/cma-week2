# Bibliotheken laden
library(XML)
library(tibble)

# Pfad zur KML-Datei
file_path <- "C:/_Data/Master/PaT_24/week_2/cma-week2-rexercise/data/history-2024-05-01.kml"

# KML-Datei einlesen
doc <- xmlParse(file_path)

# Namespace definieren
ns <- c(kml = "http://www.opengis.net/kml/2.2")

# Koordinaten extrahieren
placemark_nodes <- getNodeSet(doc, "//kml:Placemark", namespaces = ns)
coord_df <- tibble(Placemark = character(), Longitude = numeric(), Latitude = numeric())

# Funktion zur Extraktion der Koordinaten aus einem Placemark
extract_coords <- function(placemark_node) {
  name <- xmlValue(placemark_node[["name"]])
  coordinates <- xmlValue(placemark_node[["Point"]][["coordinates"]])
  coords_list <- strsplit(coordinates, ",")[[1]]
  coords <- as.numeric(coords_list)
  return(data.frame(Placemark = name, Longitude = coords[1], Latitude = coords[2]))
}

# Koordinaten für jeden Placemark extrahieren und in den Datenrahmen einfügen
for (placemark_node in placemark_nodes) {
  coord_df <- rbind(coord_df, extract_coords(placemark_node))
  
  # Überprüfen, ob das Placemark Koordinaten in einem LineString hat
  if (!is.null(placemark_node[["LineString"]])) {
    coordinates <- xmlValue(placemark_node[["LineString"]][["coordinates"]])
    coords_list <- strsplit(coordinates, "\\s+")[[1]]
    for (coord_str in coords_list) {
      coord <- as.numeric(strsplit(coord_str, ",")[[1]])
      coord_df <- rbind(coord_df, data.frame(Placemark = NA, Longitude = coord[1], Latitude = coord[2]))
    }
  }
}

# XML-Objekt freigeben
free(doc)

# Datenrahmen anzeigen
print(coord_df)
