# Klamath Basin lake and reservoir boundaries

Dissolved polygon boundaries for the major named lakes and reservoirs in
the Klamath Basin: Upper Klamath Lake, Lower Klamath Lake, Tule Lake,
Clear Lake Reservoir, and Gerber Reservoir. Lakes made up of multiple
NHD impoundment polygons (e.g. Tule Lake) are dissolved into a single
feature.

## Usage

``` r
klamath_lakes
```

## Format

An `sf` tibble with 7 rows and 3 variables:

- lake_name:

  Lake name (character).

- area_sq_km:

  Total surface area (sq km).

- geometry:

  Lake boundary (`POLYGON`/`MULTIPOLYGON`, EPSG:4326).

## Source

NHDPlus High Resolution, NHDWaterbody feature class:
<https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data>
