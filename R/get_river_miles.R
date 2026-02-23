# get_river_miles <- function(river_layer, river_name, interval_no = 10) {
#
#   river_layer <- river_layer |>
#     filter(river == river_name) |>
#     st_combine() |>
#     st_cast("LINESTRING") |>
#     st_transform(crs = 32610)
#
#   interval <- interval_no #interval_no is 10 meters by default
#   line_length <- st_length(river_layer)
#   n_points <- as.numeric(line_length %/% interval)
#
#   river_points <- st_line_sample(river_layer, n = n_points)  |>
#     st_cast("POINT") |>
#     st_as_sf()
#
#   river_points <- st_sf(geometry = river_points, crs = st_crs(river_layer))
#
#   # units are in meters
#   distances <- sapply(1:(length(river_points$x) - 1), function(i) {
#     st_distance(river_points$x[i], river_points$x[i + 1])
#   })
#   cumulative_distances <- c(0, cumsum(distances)) |> sort(decreasing = TRUE)
#
#   river_mile_points <- river_points  |>
#     mutate(river_meter = cumulative_distances,
#            river_km = river_meter/1000,
#            river_mile = river_meter/1609.34,
#            river = river_name) |>
#     st_transform(crs = "+proj=longlat +datum=WGS84")
#
#
#   return(river_mile_points)
#
# }

#' Get river miles
#'
#' @export
get_river_miles <- function(river_layer, river_name,
                                          interval_m = 10,
                                          downstream_end = "end",
                                          crs_proj = 32610) {

  #downstream_end <- match.arg(downstream_end)

  # 1) filter + project + merge as far as possible
  parts <- river_layer |>
    dplyr::filter(river == river_name) |>
    sf::st_transform(crs_proj) |>
    sf::st_union() |>
    sf::st_line_merge()

  parts <- sf::st_cast(parts, "LINESTRING")

  # 2) If multiple parts remain, connect nearest endpoints ONCE
  if (length(parts) > 1) {

    ends <- sf::st_cast(sf::st_boundary(parts), "POINT") # endpoints for each part
    ends_sf <- sf::st_as_sf(ends)

    dmat <- sf::st_distance(ends_sf)
    dnum <- as.matrix(dmat)
    dnum <- units::drop_units(dnum)
    dnum[dnum == 0] <- NA

    ij <- which(dnum == min(dnum, na.rm = TRUE), arr.ind = TRUE)[1, ]
    p1 <- sf::st_coordinates(ends_sf[ij[1], ])
    p2 <- sf::st_coordinates(ends_sf[ij[2], ])

    connector <- sf::st_sfc(sf::st_linestring(rbind(p1, p2)), crs = sf::st_crs(parts))

    # union + merge again
    parts <- sf::st_union(sf::st_union(parts), connector) |>
      sf::st_line_merge() |>
      sf::st_cast("LINESTRING")
  }

  # 3) Force a single line: pick longest if merge still returns multiple
  if (length(parts) > 1) {
    lens <- as.numeric(sf::st_length(parts))
    parts <- parts[which.max(lens)]
  }
  line <- parts

  line_len <- as.numeric(sf::st_length(line))

  # 4) sample points in order along the line
  n_pts <- floor(line_len / interval_m) + 1

  pts <- sf::st_line_sample(line, n = n_pts, type = "regular") |>
    sf::st_cast("POINT") |>
    sf::st_as_sf()

  pts$dist_from_start_m <- seq(0, by = interval_m, length.out = nrow(pts))
  pts$dist_from_start_m[nrow(pts)] <- line_len

  # 5) downstream = 0
  pts$river_meter <- if (downstream_end == "start") {
    pts$dist_from_start_m
  } else {
    line_len - pts$dist_from_start_m
  }

 pts |>
    dplyr::mutate(
      river_km   = river_meter / 1000,
      river_mile = river_meter / 1609.34,
      river      = river_name
    ) |>
    sf::st_transform(4326)
}
