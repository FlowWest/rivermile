
#' Extract and Clean Waterbody Names from Text Strings
#'
#' Parses and standardizes waterbody names from a character vector using pattern matching and cleanup rules.
#'
#' @param names A character vector of site names or location descriptions containing waterbody names.
#'
#' @return A character vector of cleaned waterbody names (e.g., "North Fork River", "Adobe Creek").
#'
#' @details
#' The function:
#' \itemize{
#'   \item Replaces common abbreviations such as `"Rvr"` or `"r"` with `"River"`
#'   \item Extracts phrases matching a pattern that includes optional directional or descriptor prefixes (e.g., "Upper", "Fork") followed by one to three words and ending in `"Creek"` or `"River"`
#'   \item Removes common suffix terms like `"at"` and `"HOBO"` that may appear in logger names
#'   \item Trims leading and trailing whitespace
#' }
#'
#' @importFrom stringr str_extract
#'
#' @examples
#' extract_waterbody(c("Upper Adobe Creek at HOBO", "South Fork Rvr", "Bell Cr", "Argonaut r"))
#' # Returns: "Upper Adobe Creek", "South Fork River", NA, "Argonaut River"
#'
#' @export
extract_waterbody <- function(names) {
  names <- gsub("\\bRvr\\b", "River", names, ignore.case = TRUE)
  names <- gsub("\\br\\b", "River", names, ignore.case = TRUE)

  names <- stringr::str_extract(names, "(?i)(\\b(?:upper|lower|north|south|east|west|middle|fork|branch)?\\s*(?:\\w+\\s){0,3}(?:Creek|River))")

  cleaned_names <- gsub("\\b(?:at|HOBO)\\b", "", names, ignore.case = TRUE)
  result <- trimws(cleaned_names)
  return(result)
}

#' Extract Simplified Waterbody Names
#'
#' Parses and simplifies waterbody names from a character vector by removing directional and structural descriptors to retain only the core `"River"` or `"Creek"` name.
#'
#' @param names A character vector containing descriptive waterbody names, typically from site or logger metadata.
#'
#' @return A character vector of simplified waterbody names (e.g., `"Adobe Creek"`, `"Klamath River"`), with optional descriptors and noise removed.
#'
#' @details
#' This function is designed to reduce waterbody names to a consistent and minimal form by:
#' \itemize{
#'   \item Standardizing `"Rvr"` and `"r"` to `"River"`
#'   \item Removing prefixes such as `"Fork"`, `"Slough"`, `"Branch"`, `"Tributary"`, `"Side Channel"`, etc.
#'   \item Removing directional descriptors like `"Upper"`, `"Lower"`, `"North"`, `"East"`, etc.
#'   \item Extracting only names ending in `"Creek"` or `"River"`
#'   \item Stripping common suffix noise like `"at"` and `"HOBO"`
#'   \item Trimming whitespace from the final result
#' }
#'
#' This is useful when comparing or filtering by standardized stream names across datasets.
#'
#' @importFrom stringr str_extract
#'
#' @examples
#' extract_waterbody_short(c(
#'   "Upper Adobe Creek at HOBO",
#'   "South Fork Rvr",
#'   "Side Channel Klamath River",
#'   "West Branch Willow Creek"
#' ))
#' # Returns: "Adobe Creek", "South River", "Klamath River", "Willow Creek"
#'
#' @export
extract_waterbody_short <- function(names) {
  # Standardizing abbreviations for "River"
  names <- gsub("\\bRvr\\b", "River", names, ignore.case = TRUE)
  names <- gsub("\\br\\b", "River", names, ignore.case = TRUE)

  # Remove unwanted prefixes like "Side Channel", "Branch", "Fork", etc.
  names <- gsub("(?i)\\b(side channel|branch|fork|tributary|slough|stream)\\b\\s*", "", names)

  # Remove "Upper" and "Lower" to keep only the core name
  names <- gsub("(?i)\\b(upper|lower|east|west|north|south|middle)\\b\\s*", "", names)

  # Extract only the main waterbody name (Creek or River)
  names <- stringr::str_extract(names, "(?i)(\\b(?:north|south|east|west|middle)?\\s*(?:\\w+\\s){0,3}(?:Creek|River))")

  # Remove unnecessary words like "at" or "HOBO"
  cleaned_names <- gsub("\\b(?:at|HOBO)\\b", "", names, ignore.case = TRUE)

  # Trim whitespace and return result
  result <- trimws(cleaned_names)
  return(result)
}


#' Load River Mile Data from the `rivermile` Package
#'
#' Dynamically loads a river mile dataset from the `rivermile` package by name.
#' If the specified object is not found or an error occurs, the function
#' returns `NULL` and prints an informative message.
#'
#' @param river_name A character string specifying the name of a dataset, assumed to exist within the `rivermile` package.
#'
#' @return A data frame containing the river mile data if the object is found; otherwise, `NULL`.
#'
#' @examples
#' \dontrun{
#' # Load a specific river mile dataset from the rivermile package
#' rm_data <- load_river_mile_data("wood_river")
#'
#' # If the dataset doesn't exist, a message will be printed and NULL returned
#' rm_data <- load_river_mile_data("nonexistent_river")
#' }
#'
#' @export
load_river_mile_data <- function(river_name) {
  tryCatch({
    # Attempt to load the data
    river_mile_data <- as.data.frame(do.call(`::`, list(pkg = "rivermile", name = river_name)))
    return(river_mile_data)
  }, error = function(e) {
    # If an error occurs, print a message and return NULL
    message(paste("No point layer created for this river:", river_name))
    return(NULL)
  })
}


#' Format and Filter Gage Data for Spatial Analysis
#'
#' Converts a gage dataset with latitude and longitude columns into an `sf` spatial object, with an option to filter to streams included in the `rivermile::all_klamath_rivers_line` reference dataset.
#'
#' @param data A data frame containing at least the columns `longitude`, `latitude`, and `stream`.
#' @param filter_streams Logical; if `TRUE`, the output will only include gages whose `stream_short` names match those in `rivermile::all_klamath_rivers_line$river`. Defaults to `TRUE`.
#'
#' @return An `sf` object with point geometries in WGS84 (EPSG:4326), optionally filtered by stream.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Extracts a short stream name from the `stream` column using `extract_waterbody_short()`
#'   \item Removes the `river_mile` column if present
#'   \item Filters out rows with missing longitude
#'   \item Converts to an `sf` object using `longitude` and `latitude` as coordinates
#'   \item Optionally filters to include only streams listed in the `rivermile` package’s `all_klamath_rivers_line$river`
#' }
#'
#' @importFrom dplyr mutate select filter
#' @importFrom sf st_as_sf
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Example usage
#' formatted_gages <- gage_data_format(my_gage_data)
#' }
#'
#' @export
gage_data_format <- function(data, filter_streams = TRUE) {

  tmp <- data |>
    mutate(stream_short = extract_waterbody_short(stream)) |>
    select(-river_mile) |>
    filter(!is.na(longitude)) |>
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

  if (filter_streams == TRUE) {
    tmp <- tmp |>
      filter(stream_short %in% rivermile::all_klamath_rivers_line$river)

    return(tmp)
  } else {
    return(tmp)
  }

}
