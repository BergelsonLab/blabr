
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
    object = col_factor(),
    utterance_type = col_factor(),
    object_present = col_factor(),
    speaker = col_factor(),
    basic_level = col_factor(),
    annotid = readr::col_character(),
    id = col_factor(),
    subj = col_factor(),
    month = col_factor(),
    SubjectNumber = col_factor(),
    audio_video = col_factor(),
    # With include_na = FALSE, NA values get coded as belonging to a NA level,
    # and with include_na = TRUE - to a "NA" level. The latter was easier to
    # remove in the mutate below, so that we have NA values, not a NA/"NA"
    # level.
    tier = readr::col_factor(include_na = TRUE),
    pho = readr::col_character()
  )

  # global_bl column wasn't there until version 0.5.0
  if (is.null(version) || (compareVersion(version, '0.5.0') >= 0)) {
    col_types[['global_bl']] = readr::col_factor()
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


#' Load seedlings_nouns from the lab-private repository
#'
#' For the function to work, clone [seedlings-nouns_private](https://github.com/BergelsonLab/seedlings-nouns_private) to `~/BLAB_DATA/seedlings-nouns_private/` if you haven't already.
#'
#' To make your analysis reproducible, set the `version` argument.
#' To get the latest version, omit `version`, look for the version number in the output, and then set the `version` argument to that version.
#'
#'
#'
#' @inheritParams get_all_basiclevel
#'
#' @return A dataframe with one row per annotated object.
#' @export
#'
#' @examples
#' seedlings_nouns <- get_seedlings_nouns('0.0.0.9000')
get_seedlings_nouns <- function(version = NULL) {
  repository = 'seedlings-nouns_private'

  # We need to know the version here because in the version 0.0.0.9000, the
  # files were in the root folder and then they were moved to "public/".
  version <- handle_dataset_version(repo = repository,
                                    version = version,
                                    tags_already_updated = FALSE,
                                    check_for_updates = FALSE)

  filename <- 'seedlings-nouns.csv'
  if (version == '0.0.0.9000') {
    # Files are in the root, nothing to do.
  } else {
    filename <- file.path('public', filename)
  }

  children <- sprintf('%02d', c(1:23, 25:46))
  months <- sprintf('%02d', 6:17)
  speakers <- c('AF3', 'AFA', 'AFB', 'AFC', 'AFL', 'AMC', 'AU2', 'AUN', 'BR1',
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
                'MTO', 'SGP', 'BBT', 'CTY', 'FGA', 'MBT', 'X13')
  utterance_types <- c('d', 'i', 'n', 'q', 'r', 's', 'u')
  object_present_values <- c('n', 'u', 'y')

  col_types = readr::cols(
    audio_video = readr::col_factor(levels = c('video', 'audio')),
    recording_id = readr::col_character(),
    child = readr::col_factor(levels = children),
    month = readr::col_factor(levels = months),
    onset = readr::col_integer(),
    offset = readr::col_integer(),
    annotid = readr::col_character(),
    ordinal = readr::col_integer(),
    speaker = readr::col_factor(levels = speakers),
    object = readr::col_character(),
    basic_level = readr::col_character(),
    global_basic_level = readr::col_character(),
    utterance_type = readr::col_factor(levels = utterance_types),
    object_present = readr::col_factor(levels = object_present_values),
    is_top_3_hours = readr::col_logical(),
    is_top_4_hours = readr::col_logical(),
    is_surplus = readr::col_logical()
  )
  seedlings_nouns <- get_df_file(repo = repository, filename = filename,
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
#' level, see `map_global_basic_level` and `make_new_global_basic_level`
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
#' get_latest_version('all_basiclvel')
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
