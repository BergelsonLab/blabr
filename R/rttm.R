# .rttm files do not have column names, so we'll give them some.
rttm_col_names <- c('SPEAKER', 'filename', 'column3',
                    'onset', 'duration', 'column6', 'column7',
                    'voice_type', 'column9', 'column10')

#' Read an .rttm file produced by Voice Type Classifier
#'
#' @param rttm_path Path to the .rttm file to be read.
#'
#' @return a tibble with onset and duration as numeric and other columns as
#' character
#' @export
#'
read_rttm <- function(rttm_path) {
  # In the Seedlings-derived rttm files the SPEAKER column is just a constant
  # "SPEAKER" and column<n> columns are all "<NA>".
  rttm_path %>%
    readr::read_delim(delim = ' ',
                      col_names = rttm_col_names,
                      col_types = readr::cols(
                        onset = readr::col_double(),
                        duration = readr::col_double(),
                        .default = readr::col_character()))
}


#' Write a tibble to an .rttm file
#'
#' @param rttm_df A dataframe with rttm data as produced by `read_rttm`.
#' @param rttm_path A path to write to.
#'
#' @return Returns input `rttm_df`
#' @export
write_rttm <- function(rttm_df, rttm_path) {
  assertthat::are_equal(colnames(rttm_df), rttm_col_names)
  rttm_df %>%
    readr::write_delim(rttm_path, delim = ' ', col_names = FALSE)
}
