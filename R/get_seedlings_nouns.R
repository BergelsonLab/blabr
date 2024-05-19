sn_factor_levels <- list(
  audio_video = c('video', 'audio'),
  children = sprintf('%02d', c(1:23, 25:46)),
  months = sprintf('%02d', 6:17),
  speakers = c('AF3', 'AFA', 'AFB', 'AFC', 'AFL', 'AMC', 'AU2', 'AUN', 'BR1',
               'BR2', 'BRO', 'BSS', 'CFS', 'CHI', 'CME', 'EFA', 'EFB', 'EFE',
               'EFS', 'EMM', 'FAT', 'FCO', 'FTV', 'FTY', 'GP2', 'GRA', 'GRM',
               'GRP', 'GTY', 'MBR', 'MFT', 'MIS', 'MOT', 'MT2', 'MTV', 'MTY',
               'SI1', 'SI2', 'SIS', 'STY', 'TOY', 'TVS', 'UN2', 'UNC', 'AFN',
               'AFR', 'AFS', 'AM1', 'ATV', 'BSK', 'BTY', 'CFA', 'CFE', 'FTS',
               'GTV', 'MC2', 'MCO', 'MCU', 'MGM', 'NOT', 'STV', 'AF8', 'AFD',
               'AMR', 'BSE', 'BTV', 'CFR', 'CMD', 'MFU', 'MFV', 'MGP', 'MOY',
               'SCU', 'AF1', 'AF2', 'AFH', 'AFM', 'AFP', 'AM2', 'AM3', 'AMA',
               'AMI', 'BSJ', 'CF1', 'CFC', 'CFD', 'CFK', 'CFZ', 'CMH', 'CML',
               'CMO', 'FBR', 'FC2', 'MTT', 'AF4', 'AF5', 'AFE', 'AM4', 'AM5',
               'AMM', 'AU3', 'AU4', 'CFL', 'CM1', 'GRO', 'MMT', 'UN4', 'AF6',
               'AF7', 'AF9', 'AFT', 'AMB', 'AME', 'AMJ', 'CCU', 'CFP', 'CH1',
               'GGM', 'GUN', 'SST', 'AFG', 'AFK', 'AMS', 'AMT', 'BSD', 'CFH',
               'CM2', 'CMJ', 'GGP', 'GMS', 'MC3', 'UAT', 'UAU', 'UTV', 'X10',
               'X11', 'AFJ', 'BSC', 'BSL', 'CFB', 'CFM', 'CMM', 'UN3', 'X12',
               'AMG', 'AMK', 'BSB', 'COU', 'GR2', 'GRF', 'MGG', 'SIU', 'UMT',
               'ADM', 'AFY', 'AM6', 'BIS', 'CMT', 'FC3', 'FCU', 'GRY', 'MST',
               'MTO', 'SGP', 'BBT', 'CTY', 'FGA', 'MBT', 'X13'),
  utterance_types = c('d', 'i', 'n', 'q', 'r', 's', 'u'),
  object_present_values = c('n', 'u', 'y'),
  region_types = c('subregion', 'top_3', 'top_4', 'surplus'),
  # Column data_type in the codebooks.
  data_types = c('integer', 'boolean', 'categorical', 'string', 'datetime')
)

seedlings_nouns_col_types <- list(
  `seedlings-nouns` = readr::cols(
    audio_video =
      readr::col_factor(levels = sn_factor_levels$audio_video),
    recording_id = readr::col_character(),
    child = readr::col_factor(levels = sn_factor_levels$children),
    month = readr::col_factor(levels = sn_factor_levels$months),
    subject_month = readr::col_character(),
    onset = readr::col_integer(),
    offset = readr::col_integer(),
    annotid = readr::col_character(),
    ordinal = readr::col_integer(),
    speaker = readr::col_factor(levels = sn_factor_levels$speakers),
    object = readr::col_character(),
    basic_level = readr::col_character(),
    global_basic_level = readr::col_character(),
    transcription = readr::col_character(),
    utterance_type =
      readr::col_factor(levels = sn_factor_levels$utterance_types),
    object_present =
      readr::col_factor(levels = sn_factor_levels$object_present_values),
    is_subregion = readr::col_logical(),
    is_top_3_hours = readr::col_logical(),
    is_top_4_hours = readr::col_logical(),
    is_surplus = readr::col_logical(),
    position = readr::col_integer(),
    subregion_rank = readr::col_integer(),
  ),
  regions = readr::cols(
    recording_id = readr::col_character(),
    start = readr::col_integer(),
    end = readr::col_integer(),
    is_subregion = readr::col_logical(),
    is_top_3_hours = readr::col_logical(),
    is_top_4_hours = readr::col_logical(),
    is_surplus = readr::col_logical(),
    position = readr::col_integer(),
    subregion_rank = readr::col_integer()
  ),
  recordings = readr::cols(
    recording_id = readr::col_character(),
    duration_ms = readr::col_integer(),
    listened_ms = readr::col_integer(),
    surplus_ms = readr::col_integer()
  ),
  `sub-recordings` = readr::cols(
    recording_id = readr::col_character(),
    start_dt = readr::col_datetime(format = ""),
    end_dt = readr::col_datetime(format = ""),
    start_ms = readr::col_integer(),
    end_ms = readr::col_integer()
  ),
  codebook = readr::cols(
    column = readr::col_character(),
    data_type = readr::col_factor(levels =
                                    sn_factor_levels$data_types),
    values = readr::col_character(),
    description = readr::col_character(),
  ),
  # The codebook for seedlings-nouns has two extra columns
  `seedlings-nouns-codebook-extra` = readr::cols(
    additional_info = readr::col_character(),
    additional_info_2 = readr::col_character()
  )
)

version_2_dev <- 'v2.0.0-dev'

#' Starting with v2.0.0-dev, all extra tables got audio_video, child, and month
#' columns added. Regions additionally received region_id, sub-recordings -
#' sub_recording_id.
#' @noRd
build_v2_extra_col_types <- function() {
  composite_key_col_types <- subset_col_types(
    seedlings_nouns_col_types[['seedlings-nouns']],
    c('audio_video', 'child', 'month'))

  # List added columns for each table.
  v2_extra_col_types <-
    list(
      `regions` = composite_key_col_types %>%
        add_col_types(readr::cols(region_id = readr::col_character())),
      `recordings` = composite_key_col_types,
      `sub-recordings` = composite_key_col_types %>%
        add_col_types(readr::cols(sub_recording_id = readr::col_character()))
    )

  return(v2_extra_col_types)
}


is_public_version <- function(version) {
  if (startsWith(version, '0') | endsWith(version, '-dev')) {
    return(FALSE)
  } else if (grepl('v?\\d+\\.\\d+\\.\\d+', version)) {
    return(TRUE)
  } else {
    stop(glue::glue('Unrecognized version {version}'))
  }
}

build_seedlings_nouns_col_types <- function(table, get_codebook, version) {
  if (isTRUE(get_codebook)) {
    col_types <- seedlings_nouns_col_types$codebook
    if (table == 'seedlings-nouns') {
      # The codebook for seedlings-nouns has two extra columns
      col_types <- add_col_types(
        col_types,
        seedlings_nouns_col_types[['seedlings-nouns-codebook-extra']])
    }
  } else {
    col_types <- seedlings_nouns_col_types[[table]]

    if (parse_version(version) >= parse_version(version_2_dev)) {
      v2_extra_col_types <- build_v2_extra_col_types()
      if (table %in% names(v2_extra_col_types)) {
        col_types <- add_col_types(col_types,
                                   v2_extra_col_types[[table]])
      }}
  }

  return(col_types)
}


#' Loads all seedlings-nouns csv tables and their codebooks (which are also
#' stored as csvs - hence the name)
#' @keywords internal
#' @noRd
get_seedlings_nouns_csv <- function(
    version = NULL,
    table = c('seedlings-nouns', 'regions', 'recordings', 'sub-recordings'),
    get_codebook = FALSE) {

  # We need to know the version here because
  if (is.null(version) || is_public_version(version)) {
    repository <- 'seedlings-nouns'
  } else {
    repository <- 'seedlings-nouns_private'
  }

  version <- handle_dataset_version(repo = repository,
                                    version = version,
                                    tags_already_updated = FALSE,
                                    check_for_updates = TRUE)
  # In the version 0.0.0.9000, the files were in the root folder and then they
  # were moved to "public/".
  if (version == '0.0.0.9000' || is_public_version(version)) {
    folder <- '.'
  } else {
    folder <- 'public'
  }

  # Determine the name of the requested file
  table <- match.arg(table)
  suffix <- ifelse(isTRUE(get_codebook), '.codebook', '')
  filename <- glue::glue('{table}{suffix}.csv')

  col_types <- build_seedlings_nouns_col_types(
    table = table, get_codebook = get_codebook, version = version)

  file_path = file.path(folder, filename)
  seedlings_nouns <- get_df_file(repo = repository, filename = file_path,
                                 version = version, col_types = col_types,
                                 version_already_handled = TRUE)

  if (isFALSE(get_codebook)) {
    message(glue::glue(
      'To get the codebook for the table, run `get_seedlings_nouns_codebook`',
      ' with `table = \'{table}\'`.'))
  }

  return(seedlings_nouns)
}

#' Load data from the SEEDLingS - Nouns dataset
#'
#' For the functions to work, clone [seedlings-nouns](https://github.com/BergelsonLab/seedlings-nouns) to `~/BLAB_DATA/seedlings-nouns/` first.
#'
#' - `get_seedlings_nouns()` loads the main "seedlings-nouns" table with the annotated nouns.
#' - `get_seedlings_nouns_extra()` function allows for loading additional tables: "regions", "recordings", and "sub-recordings".
#' - `get_seedlings_nouns_codebook()` function loads codebooks for any of the four tables mentioned above.
#'
#' To get the same data every time you run the script, always supply the version argument.
#' To get the latest version number, run `get_latest_version('seedlings-nouns')` and then set the version parameter to the output number, e.g., `get_seedlings_nouns(version = 'v1.0.0')`.
#'
#' Alternatively, don't set the version parameter, run the function, look for the version number in the issued warning, and then set `version` to that number.
#' You don't need to run the function again after that.
#'
#' If you are a Bergelson Lab member and you need to use a version that isn't public, clone [seedlings-nouns_private](https://github.com/Bergel sonLab/seedlings-nouns_private) to `~/BLAB_DATA/seedlings-nouns_private/`.
#' The function will look in the private repository only if you supply a corresponding private version - one starting with "0." or ending with "-dev".
#' To get the latest private version, use `get_latest_version('seedlings-nouns_private')`.
#' Otherwise, look in the releases section on GitHub.
#' There are no version descriptions though so you basically select the version by date.
#'
#' @inheritParams get_all_basiclevel
#' @param table For `get_seedlings_nouns_extra`, the extra table to load. One of: "regions", "recordings", "sub-recordings". For `get_seedlings_nouns_codebook`, the table can also be "seedlings-nouns" which is also the default for that function.
#' See ["README.md"](https://github.com/BergelsonLab/seedlings-nouns/blob/main/README.md) for details.
#'
#' @return
#' - For `get_seedlings_nouns`, a tibble with one annotated noun per row.
#' - For `get_seedlings_nouns_extra`, a tibble with one row per region, recording, or sub-recording depending on which table was requested.
#' - For `get_seedlings_nouns_codebook`, a tibble with ono row per column of the requested table.
#'
#' @export
#'
#' @examples
#' version <- 'v1.0.0'
#' seedlings_nouns <- get_seedlings_nouns(version)
#' seedlings_nouns_codebook <- get_seedlings_nouns_codebook(version)
#' seedlings_regions <- get_seedlings_nouns_extra(version, 'regions')
#' seedlings_regions_codebook <- get_seedlings_nouns_codebook(version,
#'                                                            'recordings')
#'
get_seedlings_nouns <- function(version = NULL) {
  get_seedlings_nouns_csv(version = version,
                          table = 'seedlings-nouns',
                          get_codebook = FALSE)
}

#' @rdname get_seedlings_nouns
#' @export
get_seedlings_nouns_extra <- function(
    version = NULL,
    table) {
  stopifnot(table %in% c('regions', 'recordings', 'sub-recordings'))

  df <- get_seedlings_nouns_csv(version = version,
                                table = table,
                                get_codebook = FALSE)

  if (table == 'sub-recordings') {
    message(glue::glue(
      'For anonymization purposes, the date of the first sub-recording of each',
      ' recording was set to Jan 1, 1920. If you need the actual dates for',
      ' your analysis, please contact the Bergelson Lab.'))

    more_than_one_count <- df %>%
      dplyr::count(recording_id) %>%
      dplyr::filter(n > 1) %>%
      nrow
    message(glue::glue(
      'Only {more_than_one_count} audio recordings were paused at any time and',
      ' therefore have more than one sub-recording. The rest of the audio',
      ' recordings and all video recordings have just one. We included them',
      ' in this table to provide time of day when those single uninterrupted',
      ' recordings started and ended.'
    ))
  }

  if (table == 'recordings') {
    message(glue::glue(
      'See table \'sub-recordings\' if you are interested in the time of day',
      ' the recordings were made. We couldn\'t add this information here',
      ' because of a small subset of recordings had been inerrupted one or',
      ' more times.'))
  }

  if (table == 'regions') {
    message(glue::glue(
      'Regions are only defined for audio recordings in this dataset. Be aware',
      ' that the video recordings are not included in this table, e.g., use',
      ' left/right/outer join when merging with the other table on',
      ' `recording_id`.'))
  }

  return(df)
}

#' @rdname get_seedlings_nouns
#' @export
get_seedlings_nouns_codebook <- function(
    version = NULL,
    table = c('seedlings-nouns', 'regions', 'recordings', 'sub-recordings')) {
  table <- match.arg(table)
  get_seedlings_nouns_csv(version = version,
                          table = table,
                          get_codebook = TRUE)
}
