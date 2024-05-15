# issue: Split internal fuctnions into dev.R

#' Extracts a subset of column specification created with readr::cols()
#' @noRd
subset_col_types <- function(col_types, subset_cols) {
  col_types_subset <- readr::cols()
  col_types_subset$cols <- col_types$cols[subset_cols]
  return(col_types_subset)
}

#' Combines two column specifications created with readr::cols()
#' @noRd
add_col_types <- function(col_types, col_types_to_add) {
  col_types$cols <- c(col_types$cols, col_types_to_add$cols)
  return(col_types)
}


#' A wrapper around semver::parse_version that handles:
#' - 0.0.0.9xxx versions by parsing them as 0.0.0-9xxx,
#' - and versions prefixed with a "v" by removing the "v".
#' @noRd
parse_version <- function(version) {
  if (startsWith(version, 'v')) {
    version <- substr(version, 2, nchar(version))
  }
  if (startsWith(version, '0.0.0.')) {
    version <- paste0('0.0.0-', substr(version, 7, nchar(version)))
  }
  semver::parse_version(version)
}

#' `dplyr::rename` for lists
#'
#' @param .x A list.
#' @parem ... Any number of `new_name = old_name` pairs.
#'
#' @export
list_rename <- function(.x, ..., .strict = TRUE) {
  # From https://github.com/tidyverse/purrr/issues/804#issuecomment-729070112
  pos <- tidyselect::eval_rename(quote(c(...)), .x, strict = .strict)
  names(.x)[pos] <- names(pos)
  .x
}
