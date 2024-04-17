#' Load all tables from the vihi_annotations repo
#' @noRd
get_vihi_annotations_tables <- function(version = NULL) {
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
    intervals = readr::cols(
      eaf_filename = readr::col_character(),
      # See note before code_num in `annotations` above
      code_num = readr::col_character(),
      sampling_type = readr::col_character(),
      onset = readr::col_integer(),
      offset = readr::col_integer(),
      context_onset = readr::col_integer(),
      context_offset = readr::col_integer(),
      is_silent = readr::col_character(),
      rank = readr::col_integer()),
    vi_td_matches = readr::cols(
        pair = readr::col_double(),
        VIHI_ID = readr::col_character(),
        match_group = readr::col_character())
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
                col_types = col_types[[table_name]],
                version_already_handled = TRUE)}

  tables <- list(
    annotations = get_table('annotations'),
    intervals = get_table('intervals'),
    vi_td_matches = get_table('vi_td_matches'))

  tables$intervals <- tables$intervals %>%
    dplyr::mutate(
      is_top_5_high_vol = (sampling_type == 'high-volubility')
      & (dplyr::dense_rank(rank) <= 5),
      .by = eaf_filename)

  return(tables)
}


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
#' @param subset Which pre-defined subset of the data should be loaded?
#' - 'random' (the default) loads the annotations from the 15 randomly sampled
#' intervals from all recordings in the corpus.
#' - 'VI+TD-VI' loads the annotations from the random and the top-5
#' high-volubility intervals from VI recordings and their TD matches.
#' - 'everything' loads all annotations from all tiers. Exercise caution with
#' this option: the data will include incomplete and unchecked annotations.
#' @param include_all_tier_types Should all tier types be included in the
#' output? If `FALSE` (the default), only tiers that are relevant to the subset
#' are returned. For the 'random' and 'VI+TD-VI' subsets, the relevant tier
#' types are: transcription, vcm, lex, mwu, xds. For the 'everything' subset,
#' this parameter is ignored as all tier types are returned.
#'
#' @return A tibble with
#' @export
#'
#' @examples
#' vihi_annotaitons <- get_vihi_annotations(version='0.0.0.9006-dev.2')
get_vihi_annotations <- function(
    version = NULL,
    subset = c('random', 'everything', 'VI+TD-VI'),
    table = c('annotations', 'intervals', 'merged'),
    include_all_tier_types = FALSE) {

  subset <- match.arg(subset)
  table <- match.arg(table)

  tables <- get_vihi_annotations_tables(version)

  if (subset %in% c('random', 'VI+TD-VI')) {
    # Filter out recordings by removing the corresponding intervals
    if (subset == 'VI+TD-VI') {
      tables$intervals <- tables$intervals %>%
        dplyr::filter(fs::path_ext_remove(eaf_filename)
                      %in% tables$vi_td_matches$VIHI_ID)
    }

    # Select the intervals
    if (subset == 'random') {
      tables$intervals <- tables$intervals %>%
        dplyr::filter(sampling_type == 'random')
    } else if (subset == 'VI+TD-VI') {
      tables$intervals <- tables$intervals %>%
        dplyr::filter(
          fs::path_ext_remove(eaf_filename) %in% tables$vi_td_matches$VIHI_ID,
          sampling_type == 'random' | is_top_5_high_vol
        )
    }

    # Keep only annotations from the selected intervals
    tables$annotations <- tables$annotations %>%
      dplyr::semi_join(tables$intervals, by=c('eaf_filename', 'code_num'))


    # Select tiers, unless include_all_tier_types is TRUE
    if (!isTRUE(include_all_tier_types)) {
      tables$annotations <- tables$annotations %>%
        dplyr::select(-any_of(c('utt', 'inq', 'fun', 'pro', 'rep', 'cds')))
    }

  }

  result <- switch(table,
                   'annotations' = tables$annotations,
                   'intervals' = tables$intervals,
                   'merged' = dplyr::left_join(tables$annotations,
                                               tables$intervals,
                                               by=c('eaf_filename', 'code_num'),
                                               relationship = 'many-to-one'))

  if (rlang::is_null(result)) {
    stop(glue::glue('Unknown values of the `table` argument: `table`'))
  }

  return(result)
}
