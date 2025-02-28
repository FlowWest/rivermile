get_river_miles <- function(river_layer, interval_no = 10) {

  interval <- interval_no #interval_no is 10 meters by default
  line_length <- st_length(river_layer)
  n_points <- as.numeric(line_length %/% interval)

  # if (st_geometry_type(river_layer) == "MULTILINESTRING") {
  #   river_layer <- st_combine(river_layer) |> st_cast("LINESTRING")
  # }

  river_points <- st_line_sample(river_layer, n = n_points)  |>
    st_cast("POINT") |>
    st_as_sf()

  river_points <- st_sf(geometry = river_points, crs = st_crs(river_layer))

  # units are in meters
  distances <- sapply(1:(length(river_points$geometry) - 1), function(i) {
    st_distance(river_points$geometry[i], river_points$geometry[i + 1])
  })
  cumulative_distances <- c(0, cumsum(distances)) |> sort(decreasing = TRUE)

  river_mile_points <- river_points  |>
    mutate(river_meter = cumulative_distances,
           river_km = river_meter/1000,
           river_mile = river_meter/1609.34) |>
    st_transform(crs = "+proj=longlat +datum=WGS84")

  return(river_mile_points)

}
