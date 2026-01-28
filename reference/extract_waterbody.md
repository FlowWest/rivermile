# Extract and Clean Waterbody Names from Text Strings

Parses and standardizes waterbody names from a character vector using
pattern matching and cleanup rules.

## Usage

``` r
extract_waterbody(names)
```

## Arguments

- names:

  A character vector of site names or location descriptions containing
  waterbody names.

## Value

A character vector of cleaned waterbody names (e.g., "North Fork River",
"Adobe Creek").

## Details

The function:

- Replaces common abbreviations such as \`"Rvr"\` or \`"r"\` with
  \`"River"\`

- Extracts phrases matching a pattern that includes optional directional
  or descriptor prefixes (e.g., "Upper", "Fork") followed by one to
  three words and ending in \`"Creek"\` or \`"River"\`

- Removes common suffix terms like \`"at"\` and \`"HOBO"\` that may
  appear in logger names

- Trims leading and trailing whitespace

## Examples

``` r
extract_waterbody(c("Upper Adobe Creek at HOBO", "South Fork Rvr", "Bell Cr", "Argonaut r"))
#> [1] "Upper Adobe Creek" "South Fork River"  NA                 
#> [4] "Argonaut River"   
# Returns: "Upper Adobe Creek", "South Fork River", NA, "Argonaut River"
```
