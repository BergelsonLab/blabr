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

find_errors_in_vihi_annotations <- function(annotations, raise_error = TRUE) {
  # For interactive debugging
  # tables <- get_vihi_annotations(version = '0.0.0.9006-dev.2',
  #                                table = 'all', subset = 'VI+TD-VI')
  # annotations <- tables$annotations

  # Helper functions
  is_empty <- function(column) {
    tidyr::replace_na({{ column }}, '') == ''
  }

  is_not_empty <- function(...) {!is_empty(...)}

  add_age_in_months <- function(df) {
    df %>%
      dplyr::mutate(
        days = stringr::str_match(eaf_filename,
                                  '\\w_\\d{3}_(\\d{3})\\.eaf')[, 2],
        months = as.numeric(days) / 30.25) %>%
      dplyr::select(-days)
  }

  errors <- dplyr::bind_rows(
    # "Empty" transcriptions
    annotations %>%
      dplyr::filter(transcription == '0.',
                    !(participant %in% c('CHI', 'EE1'))) %>%
      dplyr::mutate(
        error = 'transcription is 0. but participant is not CHI or EE1'),
    empty_transcriptions <- annotations %>%
      dplyr::filter(is_empty(transcription)) %>%
      dplyr::mutate(error = 'transcription is empty'),
    # Tiers that should be empty but aren't for CHI/non-CHI
    too_young_for_lex_or_mwu <- annotations %>%
      add_age_in_months %>%
      dplyr::filter(months < 8,
                    participant == 'CHI',
                    is_not_empty(lex) | is_not_empty(mwu)) %>%
      dplyr::select(-months) %>%
      dplyr::mutate(error = glue::glue('participant is younger than 8 months',
                                       ' but lex/mwu is filled')),
    chi_with_xds <- annotations %>%
      dplyr::filter(participant == 'CHI', is_not_empty(xds)) %>%
      dplyr::mutate(error = glue::glue('participant is CHI but xds is filled')),
    non_chi_with_chi_subtiers <- annotations %>%
      dplyr::filter(
        participant != 'CHI',
        is_not_empty(lex) | is_not_empty(mwu) | is_not_empty(vcm)) %>%
      dplyr::mutate(error = 'participant is not CHI but lex/mwu/vcm is filled'),
    # Tiers that shouldn't be empty but are for CHI/non-CHI
    non_chi_without_xds <- annotations %>%
      dplyr::filter(participant != 'CHI', is_empty(xds)) %>%
      dplyr::mutate(error = 'participant is not CHI but xds is empty'),
    old_chi_without_vcm <- annotations %>%
      add_age_in_months %>%
      dplyr::filter(participant == 'CHI',
                    months >= 8,
                    is_empty(vcm)) %>%
      dplyr::select(-months) %>%
      dplyr::mutate(
        error = 'participant is older than 8 months but vcm is empty'),
    # vcm/lex/mwu dependencies
    annotations %>%
      add_age_in_months %>%
      dplyr::filter(months >= 8) %>%
      dplyr::select(-months) %>%
      # vcm is 'C', lex should be filled; if lex is 'W', mwu should be filled
      dplyr::mutate(error = dplyr::case_when(
        vcm == 'C' & is_empty(lex) ~ 'vcm is C but lex is empty',
        vcm != 'C' & is_not_empty(lex) ~ 'vcm is not C but lex is filled',
        lex == 'W' & is_empty(mwu) ~ 'lex is W but mwu is empty',
        lex != 'W' & is_not_empty(mwu) ~ 'lex is not W but mwu is filled',
        TRUE ~ NA
      )) %>%
      dplyr::filter(!is.na(error)),
    # vcm, lex, mwu, xds only take expected values
    annotations %>%
      dplyr::filter(is_not_empty(vcm),
             !vcm %in% c('C', 'N', 'Y', 'L', 'U')) %>%
      dplyr::mutate(error = 'vcm is not C, N, Y, L, or U'),
    annotations %>%
      dplyr::filter(is_not_empty(lex),
                    !lex %in% c('W', '0')) %>%
      dplyr::mutate(error = 'lex is not W or 0'),
    annotations %>%
      dplyr::filter(is_not_empty(mwu),
                    !mwu %in% c('1', 'M')) %>%
      dplyr::mutate(error = 'mwu is not 1 or M'),
    annotations %>%
      dplyr::filter(is_not_empty(xds),
                    !xds %in% c("A", "C", "U", "O", "B", "P")) %>%
      dplyr::mutate(error = 'xds is not A, C, U, O, B, or P'),
    )

  # Check that there is just one error column added in all the chains above
  assertthat::are_equal(colnames(annotations),
                        colnames(errors %>% dplyr::select(-error))) %>%
    invisible()

  errors_wide <- errors %>%
    dplyr::select(eaf_filename, transcription_id, error) %>%
    dplyr::mutate(error_number = dplyr::row_number(),
                  .by = c(eaf_filename, transcription_id)) %>%
    tidyr::pivot_wider(values_from = error, names_from = error_number,
                       names_prefix = 'error_')

  return(errors_wide)
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
#' - Annotation are checked for errors for the standard ACLEW tiers only.
#'   Interval-level checks aren't currently checked at all.
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
#'
#' @param table Which table to return - `annotations` (the default) or
#' `intervals`. If `merged`, returns the `annotations` table with the interval
#' information merged in. Intervals without annotations won't be included. If
#' `all`, returns a named list of both tables.#'
#'
#' @param subset Which pre-defined subset of the data should be loaded?
#' - 'random' (the default) loads the annotations from the 15 randomly sampled
#' intervals from all recordings in the corpus.
#' - 'VI+TD-VI' loads the annotations from the random and the top-5
#' high-volubility intervals from VI recordings and their TD matches.
#' - 'everything' loads all annotations from all tiers. Exercise caution with
#' this option: the data will include incomplete and unchecked annotations.
#'
#' @param include_all_tier_types Should all tier types be included in the
#' output? If `FALSE` (the default), only tiers that are relevant to the subset
#' are returned. For the 'random' and 'VI+TD-VI' subsets, the relevant tier
#' types are: transcription, vcm, lex, mwu, xds. For the 'everything' subset,
#' this parameter is ignored as all tier types are returned.
#'
#' @param allow_annotation_errors In case errors are found in the annotations,
#' should the function throw an error (`FALSE`, the default) or add `error_n`
#' columns to the `annotations` table? Use only as a way to inspect the errors,
#' not as a way to ignore them.
#'
#' @param include_pi Should annotations marked as PI be included in the output?
#' If `FALSE` (the default), they are filtered out.
#'
#' @return A table or a list of tables depending on the `table` parameter.
#' @export
#'
#' @examples
#' vitd_annotations <- get_vihi_annotations(version='0.0.0.9006-dev.5',
#'                                          subset='VI+TD-VI')
#'
#' vitd <- get_vihi_annotations(version='0.0.0.9006-dev.5', subset='VI+TD-VI',
#'                              table='all')
#' vitd$annotations %>% head()
#' vitd$intervals %>% head()
#'
get_vihi_annotations <- function(
    version = NULL,
    subset = c('random', 'everything', 'VI+TD-VI'),
    table = c('annotations', 'intervals', 'merged', 'all'),
    include_all_tier_types = FALSE,
    allow_annotation_errors = FALSE,
    include_pi = FALSE) {

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

  if (!include_pi) {
    tables$annotations <- tables$annotations %>%
      dplyr::filter(is.na(PI))
  }

  # Run checks on annotations
  annotation_errors <- find_errors_in_vihi_annotations(tables$annotations)
  if (nrow(annotation_errors) > 0) {
    if (isTRUE(allow_annotation_errors)) {
      tables$annotations <- tables$annotations %>%
        dplyr::left_join(annotation_errors,
                         by=c('eaf_filename', 'transcription_id'))
      warning(glue::glue('
        Errors found in {nrow(annotation_errors)} annotations - check the \\
        `error` column in the `annotations` table.

        You set `allow_annotation_errors` to TRUE. This setting should only \\
        be used to inspect the errors. Do not use it to ignore and forget.'))
    } else {
      stop(glue::glue("
        Errors found in {nrow(annotation_errors)} annotations. \\
        Rerun with allow_annotation_errors=TRUE and `table` set to anything \\
        but `intervals` and check the `error` column in the `annotations` \\
        table. For example:

        library(dplyr)

        tables <- get_vihi_annotations(version=\'{version}\',
                                       table = \'all\',
                                       allow_annotation_errors=TRUE)
        tables$annotations %>%
           filter(if_any(starts_with('error_'), ~ !is.na(.)))
        "))
    }
  }

  result <- switch(table,
                   'annotations' = tables$annotations,
                   'intervals' = tables$intervals,
                   'merged' = dplyr::left_join(tables$annotations,
                                               tables$intervals,
                                               by=c('eaf_filename', 'code_num'),
                                               relationship = 'many-to-one'),
                   'all' = tables[c('annotations', 'intervals')])

  if (rlang::is_null(result)) {
    stop(glue::glue('Unknown values of the `table` argument: `table`'))
  }

  return(result)
}
