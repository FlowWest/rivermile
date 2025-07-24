#' Assign Sub Basin
#'
#' This function takes an `sf` object containing sub basin location in the Klamath Basin (e.g., trinity, upper klamath, etc)
#' and assigns a sub-basin to each data point.
#'
#' @param data A data frame or `sf` object with location data.
#' @param sub_basin An `sf` polygon object containing sub-basin geometries and names.
#'
#' @return A data frame (or sf object) with a new `sub_basin` column in lowercase.
#' @export
assign_sub_basin <- function(data, sub_basin, is_point = TRUE, lon_col = "longitude", lat_col = "latitude", sub_basin_col = "NAME") {
  if (is_point) {
    sf_data <- st_as_sf(data, coords = c(lon_col, lat_col), crs = 4326)
  } else {
    sf_data <- st_as_sf(data)
  }
  sf_data <- sf_data |>
    st_transform(st_crs(sub_basin)) |>
    st_join(sub_basin[sub_basin_col]) |>
    rename(sub_basin = !!sub_basin_col) |>
    mutate(sub_basin = tolower(sub_basin))
  if (is_point) {
    coords <- st_coordinates(sf_data)
    sf_data[[lon_col]] <- coords[, 1]
    sf_data[[lat_col]] <- coords[, 2]
    sf_data <- st_drop_geometry(sf_data)
  }
  return(sf_data)
}
