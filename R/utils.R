extract_waterbody <- function(names) {
  names <- gsub("\\bRvr\\b", "River", names, ignore.case = TRUE)
  names <- gsub("\\br\\b", "River", names, ignore.case = TRUE)

  names <- stringr::str_extract(names, "(?i)(\\b(?:upper|lower|north|south|east|west|middle|fork|branch)?\\s*(?:\\w+\\s){0,3}(?:Creek|River))")

  cleaned_names <- gsub("\\b(?:at|HOBO)\\b", "", names, ignore.case = TRUE)
  result <- trimws(cleaned_names)
  return(result)
}

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
