# NHD Catchments

National Hydrography Dataset of catchments, filtered to nhdplusids for
rivers included in the Klamath Basin Science Collaborative. Source:
https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data

## Usage

``` r
nhd_catchments
```

## Format

A tibble with 4591 rows and 13 columns

- `nhdplusid`: unique identifier for the NHD dataset

- `areasqkm`: area of catchment (sq km)

- `geometry`: sf geometry
