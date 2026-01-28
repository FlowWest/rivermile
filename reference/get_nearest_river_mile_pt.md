# Get Nearest River Mile Point

Finds the nearest river mile for a given spatial point by computing the
shortest geodesic distance between the point and a set of river mile
locations.

## Usage

``` r
get_nearest_river_mile_pt(river_mile_points, spatial_pt)
```

## Arguments

- river_mile_points:

  An \`sf\` object containing river mile locations, with a \`geometry\`
  column representing point geometries and a \`river_mile\` column.

- spatial_pt:

  An \`sf\` object representing a single point location (e.g., a
  sampling point).

## Value

The river mile value from \`river_mile_points\` that is closest to
\`spatial_pt\`. If no match is found, returns \`NA\`.

## Details

\- Ensures that both \`spatial_pt\` and \`river_mile_points\` have a
defined CRS (\`EPSG:4326\`). - Transforms \`spatial_pt\` to match the
CRS of \`river_mile_points\` if necessary. - Computes distances using
\`st_distance()\` and returns the nearest river mile.
