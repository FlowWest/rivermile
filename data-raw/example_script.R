library(tidyverse)
library(purrr)
library(pins)
library(rivermile)
library(sf)
library(leaflet)
#aws sso login --profile flowwest

# setting up aws bucket
wq_data_board <- pins::board_s3(bucket = "klamath-sdm", region = "us-east-1")

# Temperature
wqx_gage <- wq_data_board |>
  pins::pin_read("water_quality/processed-data/temperature_gage") |>
  gage_data_format(filter_streams = FALSE)

wqx_gage_with_rivermiles <- rivermile::find_nearest_river_miles(wqx_gage)

# DO
wqx_do <- wq_data_board |>
  pins::pin_read("water_quality/processed-data/do_gage") |>
  distinct() |>
  gage_data_format(filter_streams = FALSE)

wqx_do_with_rivermiles <- rivermile::find_nearest_river_miles(wqx_do)

# pH USGS
wqx_ph <- wq_data_board |>
  pins::pin_read("water_quality/processed-data/ph_gage") |>
  distinct() |>
  gage_data_format(filter_streams = TRUE)

wqx_pH_with_rivermiles <- rivermile::find_nearest_river_miles(wqx_ph)



leaflet() |>
  addTiles() |>
  addCircleMarkers(data = all_klamath_rivers_pts |> filter(river == "Wood River"),
                   popup = ~as.character(round(river_mile, 2))) |>
  addMarkers(data = wqx_pH_with_rivermiles |> filter(stream == "Wood River"),
             popup = ~paste0("river: ", stream, "<br>",
                                   "river_mile: ", river_mile))



