#' Read audio sparse code csv from the Seedlings annotations.
#'
#' @param file_path Path to the sparse code csv file.
#' @param parse_timestamp_audio Should the timestamp column be split into the
#'   "onset" and "offset" integer columns?
#'
#' @return A tibble with all the columns in the file at `file_path` read as
#' character vector. If `parse_timestamp_audio` is true, there will be two
#' additional integer columns: `onset` and `offset`.
#' @export
read_seedlings_audio_annotations <- function(file_path,
                                             parse_timestamp_audio = T) {
  annotations <- readr::read_csv(
    file_path,
    col_types = readr::cols(
      tier = readr::col_character(),
      word = readr::col_character(),
      utterance_type = readr::col_character(),
      object_present = readr::col_character(),
      speaker = readr::col_character(),
      annotid = readr::col_character(),
      timestamp = readr::col_character(),
      basic_level = readr::col_character(),
      comment = readr::col_character(),
      pho = readr::col_character()
    )
  )
  if (parse_timestamp_audio) {
    annotations <- annotations %>%
      tidyr::separate(
        timestamp,
        into = c('onset', 'offset'),
        sep = '_',
        convert = TRUE,
        remove = FALSE
      )
  }

  return(annotations)
}


#' Write the seedlings audio annotation tibble back to the csv file
#'
#' @param annotations A tibble such as read by read_seedlings_audio_annotations.
#' @param file_path File path to write to.
#'
#' @return Input `annotations` with any extra columns removed and the "pho"
#'   column having NA filled with empty strings - the tibble that is fed to
#'   `readr::write_csv`.
#' @export
write_seedlings_audio_annotations <- function(annotations, file_path) {
  annotations %>%
    dplyr::select(
      tier,
      word,
      utterance_type,
      object_present,
      speaker,
      annotid,
      timestamp,
      basic_level,
      comment,
      pho
    ) %>%
    # TODO: update annotations to use "NA" when there is no transcription
    # (blabpy #16)
    dplyr::mutate(pho = tidyr::replace_na(pho, replace = '')) %>%
    readr::write_csv(file_path)
}
