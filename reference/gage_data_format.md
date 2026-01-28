# Format and Filter Gage Data for Spatial Analysis

Converts a gage dataset with latitude and longitude columns into an
\`sf\` spatial object, with an option to filter to streams included in
the \`rivermile::all_klamath_rivers_line\` reference dataset.

## Usage

``` r
gage_data_format(data, filter_streams = TRUE)
```

## Arguments

- data:

  A data frame containing at least the columns \`longitude\`,
  \`latitude\`, and \`stream\`.

- filter_streams:

  Logical; if \`TRUE\`, the output will only include gages whose
  \`stream_short\` names match those in
  \`rivermile::all_klamath_rivers_line\$river\`. Defaults to \`TRUE\`.

## Value

An \`sf\` object with point geometries in WGS84 (EPSG:4326), optionally
filtered by stream.

## Details

The function:

- Extracts a short stream name from the \`stream\` column using
  \`extract_waterbody_short()\`

- Removes the \`river_mile\` column if present

- Filters out rows with missing longitude

- Converts to an \`sf\` object using \`longitude\` and \`latitude\` as
  coordinates

- Optionally filters to include only streams listed in the \`rivermile\`
  package’s \`all_klamath_rivers_line\$river\`

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
formatted_gages <- gage_data_format(my_gage_data)
} # }
```
