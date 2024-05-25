# issue: Split internal functions into dev.R


#' Removes a subset of column specification created with readr::cols()
#' @noRd
#' @param col_types A column specification created with readr::cols()
#' @param cols_to_remove A vector of column names to remove
#' @return A column specification with the columns in `cols_to_remove` removed
#' from `col_types`
remove_col_types <- function(col_types, cols_to_remove) {
  col_types$cols <- col_types$cols[!names(col_types$cols) %in% cols_to_remove]
  return(col_types)
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
#' @param ... Any number of `new_name = old_name` pairs.
#' @param .strict If `TRUE`, the function will throw an error if any of the
#'  new names do not exist in the list.
#'
#' @export
list_rename <- function(.x, ..., .strict = TRUE) {
  # From https://github.com/tidyverse/purrr/issues/804#issuecomment-729070112
  pos <- tidyselect::eval_rename(quote(c(...)), .x, strict = .strict)
  names(.x)[pos] <- names(pos)
  .x
}
