#' Load VIHI annotation data
#'
#' Clone BLAB-private [vihi_annotations](https://github.com/bergelsonlab/vihi_annotations.git)
#' repo to `~/BLAB_DATA` once before using this function.
#'
#' The speaker TIER is identified by the `participant` column. Other tiers are
#' in columns.
#'
#' Notes:
#' - Annotations marked as PI are included. Filter them out if you don't want
#'   them.
#' - The transcribed utterance can be empty (''). Normally, that means that a
#'   code interval has been segmented but not annotated. But there might be
#'   other stray utterance segments like that.
#' - (relevant for non-speaker TIERs only) Currently, there is no way to tell
#'   whether an annotation is missing because it was not segmented or because it
#'   was segmented but not yet annotated: both are represented as NA. This will
#'   change in the future: missing segment will still be NA, but missing
#'   annotation will be ''.
#'
#' @inheritParams get_seedlings_nouns
#' @param table Which of the two tables should be loaded?
#' @param merge Should annotations be merged with the intervals info? Not
#' applicable if `table == 'intervals'`. True by default.
#'
#' @return A tibble with
#' @export
#'
#' @examples
#' vihi_annotaitons <- get_vihi_annotations(version='0.0.0.9000')
get_vihi_annotations <- function(
    version = NULL,
    table = c('annotations', 'intervals', 'merged')) {

  table <- match.arg(table)

  col_types <- list(
    annotations = readr::cols(
      eaf_filename = readr::col_character(),
      participant = readr::col_character(),
      onset = readr::col_integer(),
      offset = readr::col_integer(),
      transcription = readr::col_character(),
      transcription_id = readr::col_character(),
      mwu = readr::col_character(),
      lex = readr::col_character(),
      vcm = readr::col_character(),
      cds = readr::col_character(),
      xds = readr::col_character(),
      # The type of `code_num` has to match (a) blabpy, (b) intervals. Change
      # all three together if necessary.
      code_num = readr::col_character(),
      PI = readr::col_character(),
      inq = readr::col_character(),
      utt = readr::col_character(),
      fun = readr::col_character(),
      pro = readr::col_character(),
      rep = readr::col_character()
    ),
    intervals= readr::cols(
      eaf_filename = readr::col_character(),
      # See not before code_num in `annotations` above
      code_num = readr::col_character(),
      sampling_type = readr::col_character(),
      onset = readr::col_integer(),
      offset = readr::col_integer(),
      context_onset = readr::col_integer(),
      context_offset = readr::col_integer(),
      is_silent = readr::col_character(),
      rank = readr::col_integer())
  )

  version <- handle_dataset_version(repo = 'vihi_annotations',
                                    version = version,
                                    tags_already_updated = FALSE,
                                    check_for_updates = TRUE)

  # Load necessary tables
  get_table <- function(table_name) {
    get_df_file(repo = 'vihi_annotations',
                filename = glue::glue('{table_name}.csv'),
                version = version,
                col_types = col_types[[table_name]])}

  if (table %in% c('annotations', 'merged')) {
    annotations <- get_table('annotations')
  }

  if (table %in% c('intervals', 'merged')) {
    intervals <- get_table('intervals')
  }

  result <- switch(table,
                   'annotations' = annotations,
                   'intervals' = intervals,
                   'merged' = dplyr::left_join(annotations, intervals,
                                               by=c('eaf_filename', 'code_num'),
                                               relationship = 'many-to-one'))

  if (rlang::is_null(result)) {
    stop(glue::glue('Unknown values of the `table` argument: `table`'))
  }

  return(result)
}
