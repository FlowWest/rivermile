get_river_miles <- function(river_layer, river_name, interval_no = 10) {

  interval <- interval_no #interval_no is 10 meters by default
  line_length <- st_length(river_layer)
  n_points <- as.numeric(line_length %/% interval)

  river_layer <- river_layer |>
    filter(river == river_name) |>
    st_combine() |>
    st_cast("LINESTRING") |>
    st_transform(crs = 32610)

  river_points <- st_line_sample(river_layer, n = n_points)  |>
    st_cast("POINT") |>
    st_as_sf()

  river_points <- st_sf(geometry = river_points, crs = st_crs(river_layer))

  # units are in meters
  distances <- sapply(1:(length(river_points$x) - 1), function(i) {
    st_distance(river_points$x[i], river_points$x[i + 1])
  })
  cumulative_distances <- c(0, cumsum(distances)) |> sort(decreasing = TRUE)

  river_mile_points <- river_points  |>
    mutate(river_meter = cumulative_distances,
           river_km = river_meter/1000,
           river_mile = river_meter/1609.34,
           river = river_name) |>
    st_transform(crs = "+proj=longlat +datum=WGS84")


  return(river_mile_points)

}
