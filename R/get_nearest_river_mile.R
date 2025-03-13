#' Get Nearest River Mile Point
#'
#' Finds the nearest river mile for a given spatial point by computing the
#' shortest geodesic distance between the point and a set of river mile locations.
#'
#' @param river_mile_points An `sf` object containing river mile locations,
#'        with a `geometry` column representing point geometries and a `river_mile` column.
#' @param spatial_pt An `sf` object representing a single point location (e.g., a sampling point).
#'
#' @return The river mile value from `river_mile_points` that is closest to `spatial_pt`.
#' If no match is found, returns `NA`.
#'
#' @details
#' - Ensures that both `spatial_pt` and `river_mile_points` have a defined CRS (`EPSG:4326`).
#' - Transforms `spatial_pt` to match the CRS of `river_mile_points` if necessary.
#' - Computes distances using `st_distance()` and returns the nearest river mile.
#'
#' @import sf
#' @export
get_nearest_river_mile_pt <- function(river_mile_points, spatial_pt) {

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

#' Find Nearest River Miles
#'
#' This function takes an `sf` object containing point locations (e.g., sampling points)
#' and finds the nearest river mile for each point.
#'
#' @param points_sf An `sf` object containing points with a `stream_short` column.
#' @return An `sf` object with an added `nearest_river_mile` column.
#' If no river mile data exists, the value will be `NA`.
#' @import sf dplyr
#' @export
find_nearest_river_miles <- function(points_sf) {
  with_rms <- points_sf |>
    mutate(river_name_lower = gsub(" ", "_", tolower(stream_short))) |>
    rowwise() |>
    mutate(
      river_mile = {
        # Safely attempt to load river mile data
        river_mile_data <- tryCatch({
          data <- load_river_mile_data(river_name_lower)
          if (!is.null(data)) {
            data |> rename(geometry = x) |> st_as_sf(crs = 4326)
          } else {
            NULL
          }
        }, error = function(e) {
          message(paste("No point layer created for this river:", river_name_lower))
          return(NULL)
        })

        # If river mile data exists, compute nearest river mile
        if (!is.null(river_mile_data)) {
          tryCatch({
            get_nearest_river_mile_pt(river_mile_data, geometry)
          }, error = function(e) {
            message(paste("Error computing nearest river mile for", river_name_lower))
            return(NA)  # Return NA in case of an error
          })
        } else {
          NA  # Return NA if no river mile data exists
        }
      }
    ) |>
    ungroup() |>
    select(-river_name_lower)

  return(with_rms)
}
