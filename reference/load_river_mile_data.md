# Load River Mile Data from the \`rivermile\` Package

Dynamically loads a river mile dataset from the \`rivermile\` package by
name. If the specified object is not found or an error occurs, the
function returns \`NULL\` and prints an informative message.

## Usage

``` r
load_river_mile_data(river_name)
```

## Arguments

- river_name:

  A character string specifying the name of a dataset, assumed to exist
  within the \`rivermile\` package.

## Value

A data frame containing the river mile data if the object is found;
otherwise, \`NULL\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load a specific river mile dataset from the rivermile package
rm_data <- load_river_mile_data("wood_river")

# If the dataset doesn't exist, a message will be printed and NULL returned
rm_data <- load_river_mile_data("nonexistent_river")
} # }
```
