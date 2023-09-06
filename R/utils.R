#' Create Folder
#'
#' A wrapper around dir.create that adds a new parameter `exist_ok` which, if
#' set to TRUE, will prevent a warning about folder already existing.
#'
#' @export
#'
dir_create <-function(path, exist_ok = FALSE, ...) {
  if (isTRUE(exist_ok) & dir.exists(path)) {
    return(invisible(TRUE))
  }
  dir.create(path, ...)
}


`%??%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' Shorten File Paths
#'
#' Takes a list of file paths, keeps only the file name for files with unique
#' names, and keeps paths as-is for files with non-unique names.
#'
#' This is useful when each file has to be processed and the output needs
#' to be written to a file but we want all these output files to be in the same
#' folder.
#'
shorten_file_paths <- function(file_paths) {
  filenames <- purrr:::map_chr(file_paths, basename)
  is_dupe <- duplicated(filenames) | duplicated(filenames, fromLast = TRUE)
  filenames[is_dupe] <- file_paths %>%
    # Prepend "./" to paths without path separators to distinguish duplicates
    # in the current folder - "a.txt" -> "./a.txt" but "b/a.txt" -> "b/a.txt"
    purrr::map_chr(~ file.path(dirname(.x), basename(.x))) %>%
    `[`(is_dupe)
  return(filenames)
}
