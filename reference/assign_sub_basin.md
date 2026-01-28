# Assign Sub Basin

This function takes an \`sf\` object containing sub basin location in
the Klamath Basin (e.g., trinity, upper klamath, etc) and assigns a
sub-basin to each data point.

## Usage

``` r
assign_sub_basin(
  data,
  sub_basin,
  is_point = TRUE,
  lon_col = "longitude",
  lat_col = "latitude",
  sub_basin_col = "NAME"
)
```

## Arguments

- data:

  A data frame or \`sf\` object with location data.

- sub_basin:

  An \`sf\` polygon object containing sub-basin geometries and names.

## Value

A data frame (or sf object) with a new \`sub_basin\` column in
lowercase.
