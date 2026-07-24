library(sf)
library(httr)
library(leaflet)
library(tidyverse)

# --- Pull the Klamath Basin Complex refuge boundaries ---

base_url <- "https://services.arcgis.com/QVENGdaPbd4LUkLV/arcgis/rest/services/National_Wildlife_Refuge_System_Boundaries/FeatureServer/0/query"

refuge_names <- c(
  "LOWER KLAMATH NATIONAL WILDLIFE REFUGE",
  "UPPER KLAMATH NATIONAL WILDLIFE REFUGE",
  "KLAMATH MARSH NATIONAL WILDLIFE REFUGE",
  "TULE LAKE NATIONAL WILDLIFE REFUGE",
  "CLEAR LAKE NATIONAL WILDLIFE REFUGE",
  "BEAR VALLEY NATIONAL WILDLIFE REFUGE"
)

where_clause <- paste0("ORGNAME IN ('", paste(refuge_names, collapse = "','"), "')")

resp <- GET(base_url, query = list(
  where = where_clause,
  outFields = "*",
  f = "geojson"
))

tmp <- tempfile(fileext = ".geojson")
writeBin(content(resp, "raw"), tmp)

klamath_refuges_raw <- st_read(tmp, quiet = TRUE)

# Reproject to WGS84 for leaflet (leaflet requires EPSG:4326)
klamath_refuges_raw <- st_transform(klamath_refuges_raw, crs = 4326)

# --- Build the leaflet map ---

# Color palette keyed to refuge name
pal <- colorFactor(
  palette = "Set2",
  domain = klamath_refuges_raw$ORGNAME
)

leaflet(klamath_refuges_raw) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(
    fillColor = ~pal(ORGNAME),
    fillOpacity = 0.5,
    color = "#333333",
    weight = 1.5,
    label = ~ORGNAME,
    popup = ~paste0(
      "<strong>", ORGNAME, "</strong><br>",
      "Region: ", FWSREGION, "<br>",
      "Type: ", RSL_TYPE
    ),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#000000",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~ORGNAME,
    title = "Refuge",
    opacity = 0.7
  )


# process data ------------------------------------------------------------

klamath_refuges <- klamath_refuges_raw |>
  janitor::clean_names() |>
  select(orgname, geometry) |>
  mutate(orgname = tolower(orgname)) |>
  glimpse()


# save data ---------------------------------------------------------------
usethis::use_data(klamath_refuges, overwrite = TRUE)
