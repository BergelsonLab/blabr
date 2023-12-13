
#' Get the all_basiclevel data
#'
#' @param version version tag to checkout
#' @param drop_basic_level_na whether to use the "*_NA" version that has all the
#' nouns, including those whose "basic_level" is set to "NA"
#' (drop_basic_level_na = FALSE) or the standard version that does not include
#' them.
#'
#' @return a dataframe containing the all_basiclevel data
#' @export
#'
#' @examples
#'
#' # get version with a specific version tag
#' all_bl <- get_all_basiclevel(version='0.3.2')
get_all_basiclevel <- function(version = NULL,
                               drop_basic_level_na = TRUE) {
  filename <- "all_basiclevel_na.csv"

  # The reason there are so many factors is for backwards compatibility with the
  # .feather versions of all_basiclevel which we used to keep. They were loaded
  # by default so some scripts might expect factors and they should get them.
  col_types <- readr::cols(
    ordinal = readr::col_integer(),
    onset = readr::col_integer(),
    offset = readr::col_integer(),
    object = readr::col_factor(),
    utterance_type = readr::col_factor(),
    object_present = readr::col_factor(),
    speaker = readr::col_factor(),
    basic_level = readr::col_factor(),
    annotid = readr::col_character(),
    id = readr::col_factor(),
    subj = readr::col_factor(),
    month = readr::col_factor(),
    SubjectNumber = readr::col_factor(),
    audio_video = readr::col_factor(),
    # With include_na = FALSE, NA values get coded as belonging to a NA level,
    # and with include_na = TRUE - to a "NA" level. The latter was easier to
    # remove in the mutate below, so that we have NA values, not a NA/"NA"
    # level.
    tier = readr::col_factor(include_na = TRUE),
    pho = readr::col_character()
  )

  # global_bl column wasn't there until version 0.5.0
  if (is.null(version) || (compareVersion(version, '0.5.0') >= 0)) {
    col_types$cols[['global_bl']] = readr::col_factor()
  }

  all_bl <- get_df_file(repo = 'all_basiclevel', filename = filename,
                        version = version, col_types = col_types)

  # Drop the "NA" level, converting the corresponding values to NA. Done for
  # the sake of consistency with the older versions of all_basiclevel when
  # we had feather files too there.
  all_bl <- all_bl %>%
    dplyr::mutate(tier = dplyr::recode_factor(all_bl$tier, "NA" = NA_character_))

  # There should only be one csv file now - the one that has rows where
  # basic_level is NA - that is the full file. Let's check that that we are
  # loading that file in case someone uses an older version.
  assertthat::assert_that(sum(is.na(all_bl$basic_level)) > 0)

  if (isTRUE(drop_basic_level_na)) {
    all_bl <- all_bl %>%
      tidyr::drop_na(basic_level)
  }

  return(all_bl)
}


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
    onset = readr::col_integer(),
    offset = readr::col_integer(),
    annotid = readr::col_character(),
    ordinal = readr::col_integer(),
    speaker = readr::col_factor(levels = sn_factor_levels$speakers),
    object = readr::col_character(),
    basic_level = readr::col_character(),
    global_basic_level = readr::col_character(),
    utterance_type =
      readr::col_factor(levels = sn_factor_levels$utterance_types),
    object_present =
      readr::col_factor(levels = sn_factor_levels$object_present_values),
    is_top_3_hours = readr::col_logical(),
    is_top_4_hours = readr::col_logical(),
    is_surplus = readr::col_logical()
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
    total_recorded_time_ms = readr::col_integer(),
    total_listened_time_ms = readr::col_integer()
  ),
  `sub-recordings` = readr::cols(
    recording_id = readr::col_character(),
    start = readr::col_datetime(format = ""),
    end = readr::col_datetime(format = ""),
    start_position_ms = readr::col_integer()
  ),
  codebook = readr::cols(
    column = readr::col_character(),
    data_type = readr::col_factor(levels =
                                    sn_factor_levels$data_types),
    values = readr::col_character(),
    description = readr::col_character(),
  ),
  `seedlings-nouns-codebok-extra` = readr::cols(
    additional_info = readr::col_character(),
    additional_info_2 = readr::col_character()
  )
)


is_public_version <- function(version) {
  if (startsWith(version, '0') | endsWith(version, '-dev')) {
    return(FALSE)
  } else if (grepl('v?\\d+\\.\\d+\\.\\d+', version)) {
    return(TRUE)
  } else {
    stop(glue::glue('Unrecognized version {version}'))
  }
}


#' Load data from the SEEDLingS - Nouns dataset
#'
#' Loads a requested table from the SEEDLingS - Nouns dataset.
#' By default, loads the main "seedlings-nouns" table with all the annotated nouns in the SEEDLingS corpus.
#' For the function to work, clone [seedlings-nouns](https://github.com/BergelsonLab/seedlings-nouns_private) to `~/BLAB_DATA/seedlings-nouns/` first.
#'
#' To get the same data every time you run the script, always supply the version argument.
#' To get the latest version number, run `get_latest_version('seedlings-nouns')` and then set the version parameter to the output number, e.g., `get_seedlings_nouns(version = 'v1.0.2')`.
#'
#' Alternatively, don't set the version parameter, run the function, look for the version number in the issued warning, and then set `version` to that number.
#' You don't need to run the function again after that.
#'
#' If you are a Bergelson Lab member and you need to use a version that hasn't been made public yet, clone [seedlings-nouns_private](https://github.com/Bergel sonLab/seedlings-nouns_private) to `~/BLAB_DATA/seedlings-nouns_private/`.
#' If you don't know the version number and want to get the most current one, get it with `get_latest_version('seedlings-nouns_private')`.
#'
#' @inheritParams get_all_basiclevel
#' @param table Apart from the main "seedlings-nouns" table, the dataset contains three more: regions, recordings, and sub-recordings.
#' See "public/README.md" of the [seedlings-nouns_private](https://github.com/BergelsonLab/seedlings-nouns_private) for details.
#' @param get_codebook Set to `TRUE` to get the requested table's codebook instead of the table itself.
#'
#' @return By default, returns a dataframe with one row per annotated object.
#'
#' If `table` and or `get_codebook` are changed from their default values, returns the requested table/codebook.
#'
#' @export
#'
#' @examples
#' seedlings_nouns <- get_seedlings_nouns('0.0.0.9000')
#'
get_seedlings_nouns <- function(
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
  if (isTRUE(get_codebook)) {table <- glue::glue('{table}.codebook')}
  filename <- glue::glue('{table}.csv')

  if (isTRUE(get_codebook)) {
    col_types <- seedlings_nouns_col_types$codebook
  } else {
    col_types <- seedlings_nouns_col_types[[table]]
    if (table == 'seedlings-nouns') {
      extra_cols <- seedlings_nouns_col_types['seedlings-nouns-codebok-extra']
      col_types$cols <- c(col_types$cols, extra_cols$cols)
    }
  }

  file_path = file.path(folder, filename)
  seedlings_nouns <- get_df_file(repo = repository, filename = file_path,
                                 version = version, col_types = col_types)

  return(seedlings_nouns)
}


#' Get the CDI spreadsheet for SEEDLingS babies
#'
#' @param version version tag to checkout
#' @param type "feather" or "csv". defaults to "feather"
#'
#' @return a tibble containing the SEEDLingS CDI spreadsheet
#' @export
#'
#' @examples
#' cdi <- get_cdi_spreadsheet(version='0.0.7')
get_cdi_spreadsheet <- function(version = NULL, type="feather") {
  col_types <- readr::cols(
    .default = readr::col_character(),
    month = readr::col_double(),
    CDIcomp = readr::col_double(),
    CDIprod = readr::col_double()
  )
  switch(type,
         "csv" = get_df_file('cdi_spreadsheet', "cdi.csv", version = version,
                             col_types = col_types),
         "feather" = get_df_file('cdi_spreadsheet', "cdi.feather",
                                 version = version)
         )
}


#' The the motor questionaire spreadsheet for the SEEDLingS babies
#'
#' @param version version tag to checkout
#' @param type "feather" or "csv". defaults to "feather"
#'
#' @return a tibble contaiing the SEEDLingS Motor Questionaire spreadsheet
#' @export
#'
#' @examples
#' motor <- get_motor_spreadsheet(version = '0.1.0')
get_motor_spreadsheet <- function(version = NULL, type="feather") {
  col_types <- readr::cols(
    .default = readr::col_character(),
    weight = readr::col_double(),
    Q8_FirstClick = readr::col_double(),
    Q8_LastClick = readr::col_double(),
    Q8_PageSubmit = readr::col_double(),
    Q8_ClickCount = readr::col_double(),
    Q26_FirstClick = readr::col_double(),
    Q26_LastClick = readr::col_double(),
    Q26_PageSubmit = readr::col_double(),
    Q26_ClickCount = readr::col_double(),
    walking_month = readr::col_double(),
    walking_day = readr::col_double(),
    walking_year = readr::col_double()
  )
  switch(type,
         "csv" = get_df_file('motor_spreadsheet', "motor.csv",
                             version = version, col_types = col_types),
         "feather" = get_df_file('motor_spreadsheet', "motor.feather",
                                 version = version)
         )
}


#' Get the reliability spreadsheets
#'
#' @param av either "audio" or "video"
#' @param month the month as a string, e.g. "06"
#' @param version version tag to checkout
#'
#' @return dataframe with data for the requested modality and month
#' @export
#'
#' @examples
#' audio_06_rel <- get_reliability("audio", "06")
get_reliability <- function(av, month, version = NULL) {
  fname <- paste0(av, "_", month, ".csv")
  get_df_file('reliability', fname, version)
}


#' Get the global basic level spreadsheets
#'
#' They are used to map every token in all_basiclevel_na to its global basic
#' level, see `map_global_basic_level` and `update_global_basic_levels`
#'
#' @param version version tag to checkout
#'
#' @return list of object_dict and
#' @export
#'
#' @examples
#' global_bl_mapping <- get_global_bl_mappings(version = '0.3.2')
get_global_bl_mappings <- function(version = NULL) {

  get_mapping <- function(filename, col_types) {
    return(
      get_df_file(
        repo = 'all_basiclevel',
        filename = glue::glue('global_basic_level_dicts/{filename}'),
        version = version,
        col_types = col_types))
  }

  object_dict <- get_mapping(
    filename = 'global_bl_dictionary.csv',
    col_types = readr::cols(
      object = readr::col_character(),
      disambiguate = readr::col_character(),
      global_bl = readr::col_character()))
  check_object_dict(object_dict)

  annotid_disambiguation <- get_mapping(
    filename = 'disambiguated_rows.csv',
    col_types = readr::cols(
      object = readr::col_character(),
      annotid = readr::col_character(),
      disambiguate = readr::col_character()))
  check_annotid_disambiguation(annotid_disambiguation)

  return(list(object_dict = object_dict,
              annotid_disambiguation = annotid_disambiguation))
}

#' Find latest version available for downloading?
#'
#' @inheritParams get_latest_tag
#'
#' @return the version string
#' @export
#'
#' @examples
#' get_latest_version('all_basiclevel')
get_latest_version <- function(repo, tags_already_updated = FALSE) {
  get_latest_tag(repo, tags_already_updated = tags_already_updated)
}


#' Handles the version.
#'
#' If the version isn't specified, finds the newest version and warns that not
#' specifying the version might not be a good idea.
#' If it is specified, notifies if there is a newer version available.
#'
#' @inheritParams get_all_basiclevel
#' @return version string
handle_dataset_version <- function(repo, version = NULL,
                                   tags_already_updated = FALSE,
                                   check_for_updates = TRUE) {
  # We will only need to get the latest version in these two cases.
  if (is.null(version) | isTRUE(check_for_updates)) {
    latest_version <- get_latest_version(
      repo = repo,
      tags_already_updated = tags_already_updated)

    if (is.null(version)) {
      version <- latest_version
      warning(glue::glue(
        'Getting a dataset without specifying a version is highly discouraged.\n',
        'Add ", version = \'{version}\'" to the `get_*` function call.'))
    }

    if (isTRUE(check_for_updates) && (latest_version != version)) {
      message(glue::glue(
        "You've requested version {version} of {repo}.\n",
        "The latest available version is {latest_version}.\n",
        "Consider updating."))
    }
  }

  return(version)
}


#' Downloads and then loads a csv/feather file from a specified version of a
#' dataset
#'
#' @inheritParams get_all_basiclevel
#' @inheritParams get_latest_tag
#' @param filename name of a csv/feather file
#' @param col_types Passed to `readr::read_csv` when filename ends with ".csv".
#'
#' @return tibble for feather files, data.frame for csv files
#'
#' @examples
#'
#' \dontrun{
#' get_df_file('all_basiclevel', 'all_basiclevel.csv', version = '0.1.0')
#' }
get_df_file <- function(repo, filename, version = NULL, col_types = NULL) {
  version <- handle_dataset_version(repo = repo, version = version,
                                    tags_already_updated = FALSE,
                                    check_for_updates = TRUE)

  # Download the file
  checkout_tag(repo, tag = version)

  # Load the file
  file_path <- file.path(blab_data, repo, filename)
  if (endsWith(file_path, ".csv")) {
    result <- readr::read_csv(file_path, col_types = col_types)
  } else if (endsWith(file_path, ".feather")) {
    result <- arrow::read_feather(file_path)
  }
  message("reading file: ", file_path)
  return(result)
}


#' Get the version tag of the last downloaded version of a dataset
#'
#' For a given dataset, returns the version tag of the version that was last
#' downloaded - the one that is on your computer right now.
#'
#' The main assumed usage is switching from using `get_all_basiclevel()` without
#' the version argument.
#'
#' Use interactively only and put the actual version string literal in your
#' code.
#'
#' @param dataset dataset name: 'all_basiclevel', 'reliability', etc.
#'
#' @return list with two keys: version and date
#' @export
#'
#' @examples
#' all_bl_version <- get_dataset_version('all_basiclevel')
#' print(all_bl_version$version)
#' print(all_bl_version$date)
get_dataset_version <- function(dataset) {
  version <- get_current_tag(repo = dataset, tags_already_updated = FALSE)
  commit_date <- get_current_commit_date(repo = dataset)
  return(list(version=version, date = commit_date))
}


#' Load VIHI annotation data
#'
#' Clone BLAB-private [vihi_annotations](https://github.com/bergelsonlab/vihi_annotations.git)
#' repo to `~/BLAB_DATA` once before using this function.
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
#' vihi_annotaitons <- get_vihi_annotations(version='0.0.0.9000', merge=True)
get_vihi_annotations <- function(
    version = NULL,
    table = c('annotations', 'intervals', 'merged')) {

  table <- match.arg(table)

  col_types <- list(
    annotations = cols(
      eaf_filename = col_character(),
      participant = col_character(),
      onset = col_integer(),
      offset = col_integer(),
      annotation = col_character(),
      participant_annotation_id = col_character(),
      mwu = col_character(),
      lex = col_character(),
      vcm = col_character(),
      cds = col_character(),
      xds = col_character(),
      # The type of `code_num` has to match (a) blabpy, (b) intervals. Change
      # all three together if necessary.
      code_num = col_character(),
      PI = col_character(),
      inq = col_character(),
      utt = col_character()),
    intervals= cols(
      eaf_filename = col_character(),
      # See not before code_num in `annotations` above
      code_num = col_character(),
      sampling_type = col_character(),
      onset = col_integer(),
      offset = col_integer(),
      context_onset = col_integer(),
      context_offset = col_integer())
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

