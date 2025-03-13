library(tidyverse)
library(purrr)
library(pins)

# setting up aws bucket
wq_data_board <- pins::board_s3(bucket = "klamath-sdm", region = "us-east-1")

wqx_gage <- wq_data_board |>
  pins::pin_read("water_quality/processed-data/gage_temperature_wqx") |>
  mutate(stream_short = extract_waterbody_short(stream)) |>
  filter(stream_short %in% rivermile::klamath_rivers$river) |>
  select(stream_short, latitude, longitude) |>
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

find_nearest_river_miles(wqx_gage)




