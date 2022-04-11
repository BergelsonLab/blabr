#' Read audio sparse code csv from the Seedlings annotations.
#'
#' @param file_path Path to the sparse code csv file.
#' @param parse_timestamp_audio Should the timestamp column be split into the
#'   "onset" and "offset" integer columns?
#'
#' @return
#' @export
read_seedlings_audio_annotations <- function(file_path,
                                             parse_timestamp_audio = T) {
  annotations <- readr::read_csv(
    file_path,
    col_types = cols(
      tier = col_character(),
      word = col_character(),
      utterance_type = col_character(),
      object_present = col_character(),
      speaker = col_character(),
      annotid = col_character(),
      timestamp = col_character(),
      basic_level = col_character(),
      comment = col_character(),
      pho = col_character()
    )
  )
  if (parse_timestamp_audio) {
    annotations <- annotations %>%
      separate(
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
#'   `dplyr::write_csv`.
#' @export
write_seedlings_audio_annotations <- function(annotations, file_path) {
  annotations %>%
    select(
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
    mutate(pho = replace_na(pho, replace = '')) %>%
    readr::write_csv(file_path)
}
