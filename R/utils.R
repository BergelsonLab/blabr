#' Extracts a subset of column specification created with readr::cols()
subset_col_types <- function(col_types, subset_cols) {
  col_types_subset <- readr::cols()
  col_types_subset$cols <- col_types$cols[subset_cols]
  return(col_types_subset)
}

#' Combines two column specifications created with readr::cols()
add_col_types <- function(col_types, col_types_to_add) {
  col_types$cols <- c(col_types$cols, col_types_to_add$cols)
  return(col_types)
}


#' A wrapper around semver::parse_version that handles:
#' - 0.0.0.9xxx versions by parsing them as 0.0.0-9xxx,
#' - and versions prefixed with a "v" by removing the "v".
parse_version <- function(version) {
  if (startsWith(version, 'v')) {
    version <- substr(version, 2, nchar(version))
  }
  if (startsWith(version, '0.0.0.')) {
    version <- paste0('0.0.0-', substr(version, 7, nchar(version)))
  }
  semver::parse_version(version)
}
