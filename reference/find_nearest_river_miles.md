# Find Nearest River Miles

This function takes an \`sf\` object containing point locations (e.g.,
sampling points) and finds the nearest river mile for each point.

## Usage

``` r
find_nearest_river_miles(points_sf)
```

## Arguments

- points_sf:

  An \`sf\` object containing points with a \`stream\` column.

## Value

An \`sf\` object with an added \`nearest_river_mile\` column. If no
river mile data exists, the value will be \`NA\`.
