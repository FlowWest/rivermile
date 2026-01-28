# Extract Simplified Waterbody Names

Parses and simplifies waterbody names from a character vector by
removing directional and structural descriptors to retain only the core
\`"River"\` or \`"Creek"\` name.

## Usage

``` r
extract_waterbody_short(names)
```

## Arguments

- names:

  A character vector containing descriptive waterbody names, typically
  from site or logger metadata.

## Value

A character vector of simplified waterbody names (e.g., \`"Adobe
Creek"\`, \`"Klamath River"\`), with optional descriptors and noise
removed.

## Details

This function is designed to reduce waterbody names to a consistent and
minimal form by:

- Standardizing \`"Rvr"\` and \`"r"\` to \`"River"\`

- Removing prefixes such as \`"Fork"\`, \`"Slough"\`, \`"Branch"\`,
  \`"Tributary"\`, \`"Side Channel"\`, etc.

- Removing directional descriptors like \`"Upper"\`, \`"Lower"\`,
  \`"North"\`, \`"East"\`, etc.

- Extracting only names ending in \`"Creek"\` or \`"River"\`

- Stripping common suffix noise like \`"at"\` and \`"HOBO"\`

- Trimming whitespace from the final result

This is useful when comparing or filtering by standardized stream names
across datasets.

## Examples

``` r
extract_waterbody_short(c(
  "Upper Adobe Creek at HOBO",
  "South Fork Rvr",
  "Side Channel Klamath River",
  "West Branch Willow Creek"
))
#> [1] "Adobe Creek"   "River"         "Klamath River" "Willow Creek" 
# Returns: "Adobe Creek", "South River", "Klamath River", "Willow Creek"
```
