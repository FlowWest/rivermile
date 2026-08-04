#' @title NHD Catchments
#' @name nhd_catchments
#' @description National Hydrography Dataset of catchments, filtered to nhdplusids for rivers included in the Klamath Basin Science Collaborative. Source: https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data
#' @format A tibble with 4591 rows and 13 columns
#' \itemize{
#'   \item \code{nhdplusid}: unique identifier for the NHD dataset
#'   \item \code{areasqkm}: area of catchment (sq km)
#'   \item \code{geometry}: sf geometry
#'   }
'nhd_catchments'

#' @title NHD Flowlines
#' @name processed_nhd
#' @description National Hydrography Dataset of flowlines, filtered to nhdplusids for rivers included in the Klamath Basin Science Collaborative. Source:https://www.sciencebase.gov/catalog/item/5d30c29ae4b01d82ce84aa5e
#' @format A tibble with 4236 rows and 73 columns
'processed_nhd'

#' @title All Klamath River Lines
#' @name all_klamath_rivers_line
#' @description National Hydrography Dataset of flowlines, processed and cleaned. Includes: Blue Creek, Bogus Creek, Camp Creek, Clear Creek, Fall Creek, Indian Creek, Jenny Creek, Klamath River, Link River, Lost River, Salmon River, Scotch Creek, Scott River, Shasta River, Shovel Creek, Spencer Creek, Sprague River, Trinity River, Williamson River, Wood River
#' @format A sf df with 20 rows and 2 columns
'all_klamath_rivers_line'

#' @title Klamath River HUCs
#' @name klamath_hucs
#' @description National Hydrography Dataset of HUCs, processed and cleaned. Source: https://www.sciencebase.gov/catalog/item/5d30c29ae4b01d82ce84aa5e
#' @format A sf df with 12 rows and 3 columns
#' \itemize{
#'   \item \code{huc8}: 8 digit HUC code
#'   \item \code{name}: HUC name
#'   \item \code{geometry}: sf polygon
#'   }
'klamath_hucs'

#' Klamath Basin National Wildlife Refuge Complex boundaries
#'
#' Simplified polygon boundaries for the six National Wildlife Refuges
#' comprising the Klamath Basin National Wildlife Refuge Complex (Bear
#' Valley, Clear Lake, Klamath Marsh, Lower Klamath, Tule Lake, and Upper
#' Klamath NWRs), spanning southern Oregon and northern California.
#'
#' @format An \code{sf} tibble with 6 rows and 2 variables:
#' \describe{
#'   \item{orgname}{Refuge name (character).}
#'   \item{geometry}{Refuge boundary (\code{MULTIPOLYGON}, EPSG:4326).}
#' }
#' @source U.S. Fish and Wildlife Service, National Wildlife Refuge System
#'   Boundaries feature service:
#'   \url{https://services.arcgis.com/QVENGdaPbd4LUkLV/arcgis/rest/services/National_Wildlife_Refuge_System_Boundaries/FeatureServer/0}
"klamath_refuges"

#' Klamath Basin lake and reservoir boundaries
#'
#' Dissolved polygon boundaries for the major named lakes and reservoirs in
#' the Klamath Basin: Upper Klamath Lake, Lower Klamath Lake, Tule Lake,
#' Clear Lake Reservoir, and Gerber
#' Reservoir. Lakes made up of multiple NHD impoundment polygons (e.g. Tule
#' Lake) are dissolved into a single feature.
#'
#' @format An \code{sf} tibble with 7 rows and 3 variables:
#' \describe{
#'   \item{lake_name}{Lake name (character).}
#'   \item{area_sq_km}{Total surface area (sq km).}
#'   \item{geometry}{Lake boundary (\code{POLYGON}/\code{MULTIPOLYGON}, EPSG:4326).}
#' }
#' @source NHDPlus High Resolution, NHDWaterbody feature class:
#'   \url{https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data}
"klamath_lakes"

