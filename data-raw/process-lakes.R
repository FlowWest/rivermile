library(sf)
library(leaflet)
library(tidyverse)

# --- Pull lake/reservoir boundaries from the local NHDPlus HR geodatabase ---
# Source: NHDPLUS_H_1801_HU4_GDB.gdb, layer NHDWaterbody
# (same source geodatabase used in data-raw/clean_and_process_nhd.Rmd)

gdb_path <- "data-raw/shapefiles/NHDPLUS_H_1801_HU4_GDB.gdb"

nhd_waterbody_raw <- st_read(gdb_path, layer = "NHDWaterbody", quiet = TRUE)

# Major named lakes and reservoirs in the Klamath Basin
lake_names <- c(
  "Upper Klamath Lake",
  "Lower Klamath Lake",
  "Tule Lake",
  "Clear Lake Reservoir",
  #"Copco Lake",
  #"Iron Gate Reservoir",
  "Gerber Reservoir"
)

# FType 390 = LakePond (excludes swamp/marsh, playa, etc.)
# st_zm() drops the Z dimension present in the source data; leaflet/htmlwidgets
# can't serialize XYZ geometries and errors with an unrelated-looking
# "'options' must be a fully named list" message if it's left in.
klamath_lakes_raw <- nhd_waterbody_raw |>
  filter(FType == 390, GNIS_Name %in% lake_names) |>
  st_transform(crs = 4326) |>
  st_zm(drop = TRUE, what = "ZM")

# --- Build the leaflet map ---

pal <- colorFactor(
  palette = "Set2",
  domain = klamath_lakes_raw$GNIS_Name
)

leaflet(klamath_lakes_raw) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(
    fillColor = ~pal(GNIS_Name),
    fillOpacity = 0.5,
    color = "#333333",
    weight = 1.5,
    label = ~GNIS_Name,
    popup = ~paste0(
      "<strong>", GNIS_Name, "</strong><br>",
      "Area: ", round(AreaSqKm, 2), " sq km"
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
    values = ~GNIS_Name,
    title = "Lake",
    opacity = 0.7
  )

# process data ------------------------------------------------------------

tule_lake_parts <- klamath_lakes_raw |>
  filter(GNIS_Name == "Tule Lake") |>
  st_cast("POLYGON", warn = FALSE)
tule_lake_main <- tule_lake_parts[which.max(st_area(tule_lake_parts)), ]

klamath_lakes_new <- klamath_lakes_raw |>
  filter(GNIS_Name != "Tule Lake") |>
  bind_rows(tule_lake_main)

# Some lakes (e.g. Tule Lake) are split into multiple impoundment polygons;
# dissolve to one feature per named lake.
klamath_lakes <- klamath_lakes_new |>
  janitor::clean_names() |>
  group_by(gnis_name) |>
  summarise(area_sq_km = sum(area_sq_km), .groups = "drop") |>
  mutate(gnis_name = tolower(gnis_name)) |>
  rename(lake_name = gnis_name) |>
  glimpse()

ggplot() +
  geom_sf(data = klamath_lakes)

# save data ---------------------------------------------------------------
usethis::use_data(klamath_lakes, overwrite = TRUE)
