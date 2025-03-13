get_nearest_river_mile <- function(river_mile_points, spatial_pt) {

  if (is.null(st_crs(spatial_pt))) {
    st_crs(spatial_pt) <- 4326
  }

  if (is.null(st_crs(river_mile_points))) {
    st_crs(river_mile_points) <- 4326
  }


  if (st_crs(spatial_pt) != st_crs(river_mile_points)) {
    spatial_pt <- st_transform(spatial_pt, st_crs(river_mile_points))
  }


  distances <- st_distance(spatial_pt, river_mile_points)
  min_distance_index <- which.min(distances)

  nearest_river_mile <- river_mile_points[min_distance_index, ]

  return(nearest_river_mile$river_mile)
}

find_nearest_river_miles <- function(points_sf) {

  with_rms <- points_sf |>
    mutate(river_name_lower = gsub(" ", "_", tolower(stream_short))) |>
    rowwise() |>
    mutate(
      nearest_river_mile = {
        river_mile_data <- load_river_mile_data(river_name_lower) |>
          rename(geometry = x) |>
          st_as_sf(crs = 4326)

        if (!is.null(river_mile_data)) {
          get_nearest_river_mile(river_mile_data, geometry)
        } else {
          NA  # If no data, return NA
        }
      }
    ) |>
    ungroup() |>
    select(-river_name_lower)

   return(with_rms)
}
