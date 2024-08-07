#' Read a csv file enforcing col_types if provided
#' @noRd
read_csv_strict <- function(file_path, col_types = NULL,
                            show_col_types = FALSE) {

  if (is.null(col_types)) {
    warning(glue::glue(
      "No column types specified for {file_path} when blabr loaded it.",
      " This might lead to unexpected behavior so please check that",
      " the column types match your expectations."))
  }

  result <- readr::read_csv(file_path, col_types = col_types,
                            show_col_types = FALSE)

  # Ensure that column in the file fully match col_types
  if (!is.null(col_types)) {
    tryCatch({
      assert_df_matches_col_types(result, col_types)},
      error = function(e) {
        message <- paste0(
          "The file ", file_path, " does not match the expected column types. ",
          "This could be due to a change in the dataset or a bug in blabr. ",
          "Please report this issue to the lab staff.\n\n",
          e$message)
        stop(message)})
  }

  return(result)
}

#' Read all_basiclevel_NA.csv
#' @noRd
read_all_basiclevel_na <- function(file_path, has_global_bl = TRUE) {
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
  if (isTRUE(has_global_bl)) {
    col_types$cols[['global_bl']] = readr::col_factor()
  }

  read_csv_strict(file_path, col_types = col_types, show_col_types = FALSE) %>%
    # Drop the "NA" level, converting the corresponding values to NA. Done for
    # the sake of consistency with the older versions of all_basiclevel when
    # we had feather files in the repo.
    dplyr::mutate(tier = dplyr::recode_factor(.data$tier,
                                              "NA" = NA_character_))
}

#' Get the all_basiclevel data from the all_basiclevel repo
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

  all_bl_path <- get_df_file(repo = 'all_basiclevel', filename = filename,
                             version = version, read = FALSE)

  has_global_bl <- is.null(version) || (compareVersion(version, '0.5.0') >= 0)
  all_bl <- read_all_basiclevel_na(all_bl_path, has_global_bl)

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
#' motor <- get_motor_spreadsheet(version = '0.0.2')
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
  # todo:
  # - check that month is one of '06', '07', ..., '17' for versions starting
  #   from 0.0.4
  # - check that av is in c('audio', 'video')
  # - for version prior to 0.0.4, check for specific av-month combinations that
  #   were present there
  # - add col_types (might be version-dependent)

  if (is.null(version) || (compareVersion(version, '0.0.4') >= 0)) {
    fname <- glue::glue('{month}_{av}_reliability.csv')
  } else {
    fname <- paste0(av, "_", month, ".csv")
  }

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


#' List available versions
#'
#' @inheritParams get_latest_tag
#'
#' @return the version string
#' @export
#'
#' @examples
#' get_versions('seedlings-nouns')
get_versions <- function(repo, tags_already_updated = FALSE) {
  get_tags(repo, tags_already_updated = tags_already_updated)
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
  if (!tags_already_updated) {update_tags(repo)}

  if (!is.null(version) && !version %in% get_versions(repo)) {
    stop(glue::glue("
      Couldn't find version {version} in {repo}.

      Please check the available versions with

      `get_versions('{repo}')`
      "))
  }

  # We will only need to get the latest version in these two cases.
  if (is.null(version) | isTRUE(check_for_updates)) {
    latest_version <- get_latest_version(
      repo = repo,
      tags_already_updated = TRUE)

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


#' Downloads and optionally loads a csv/feather file from a specified version of a dataset
#'
#' @inheritParams get_all_basiclevel
#' @inheritParams get_latest_tag
#' @param filename name of a csv/feather file
#' @param read If TRUE (default), loads the file. Otherwise, returns the path.
#' @param col_types Passed to `readr::read_csv` when filename ends with ".csv".
#'
#' @return tibble for feather files, data.frame for csv files
#'
#' @examples
#'
#' \dontrun{
#' get_df_file('all_basiclevel', 'all_basiclevel_NA.csv', version = '0.6.4')
#' }
get_df_file <- function(repo, filename, version = NULL, read = TRUE,
                        col_types = NULL, version_already_handled = FALSE) {
  if (!version_already_handled) {
    version <- handle_dataset_version(repo = repo, version = version,
                                      tags_already_updated = FALSE,
                                      check_for_updates = TRUE)}

  # Check out the version tag if it's not the current one
  # note:
  if (get_current_tag(repo) != version) {
    checkout_tag(repo, version)
  }


  file_path <- file.path(blab_data, repo, filename)

  if (!isTRUE(read)) {
    return(file_path)
  }

  # Load the file
  if (endsWith(file_path, ".csv")) {
    result <- read_csv_strict(file_path, col_types = col_types,
                              show_col_types = FALSE)
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
