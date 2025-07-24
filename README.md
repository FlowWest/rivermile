# `rivermile`

## Overview

This package provides tools for calculating river miles of monitoring locations along rivers and streams.

## Installation

You can install the `rivermile` package from GitHub using:

``` r
devtools::install_github("flowwest/rivermile")
```

### Example Usage

```r
?rivermile::find_nearest_river_miles()
```

This function takes an `sf` object containing point locations (e.g., sampling points) and finds the nearest river mile for each point. If a location is outside of the bounds of the processed data, an `NA` is returned. 

``` r
river_miles <- rivermile::find_nearest_river_miles(points_sp_df)
```
### Limitations

The package is currently only set up to process rivers of interest in the Klamath River watershed HUC6 180102, which includes: Blue Creek, Bogus Creek, Clear Creek, Indian Creek, Jenny Creek, Klamath River, Link River, Lost River, Salmon River, Scott River, Shasta River, Sprague River, Trinity River, Williamson River, and Wood River.

If a point is outside of this range, an NA is returned. 
