
#' Get the all_basiclevel data
#'
#' @param version version tag to checkout
#' @param type "feather" or "csv". defaults to "feather"
#'
#' @return a dataframe containing the all_basicalevel data
#' @export
#'
#' @examples
#'
#' # get version with a specific version tag
#' all_bl <- get_all_basiclevel(version='0.1.0')
get_all_basiclevel <- function(version = NULL, type="feather") {
  switch(type,
         "csv" = get_df_file('all_basiclevel', "all_basiclevel.csv", version = version),
         "feather" = get_df_file('all_basiclevel', "all_basiclevel.feather", version = version)
         )
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
  switch(type,
         "csv" = get_df_file('cdi_spreadsheet', "cdi.csv", version = version),
         "feather" = get_df_file('cdi_spreadsheet', "cdi.feather", version = version)
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
  switch(type,
         "csv" = get_df_file('motor_spreadsheet', "motor.csv", version = version),
         "feather" = get_df_file('motor_spreadsheet', "motor.feather", version = version)
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


#' Find latest version available for downloading?
#'
#' @inheritParams get_latest_tag
#' @inheritParams
#'
#' @return the version string
#' @export
#'
#' @examples
#' get_latest_version('all_basiclvel')
get_latest_version <- function(repo, tags_already_updated = FALSE) {
  get_latest_tag(repo, tags_already_updated = tags_already_updated)
}


#' Check whether there is a newer version than one requested
#'
#' @inheritParams get_latest_tag
#' @param requested_version - the version user requested
#'
#' @examples
#' check_for_updates('all_basiclevel', '0.0.2')
check_for_updates <- function(repo, requested_version,
                              tags_already_updated = FALSE) {
  latest_version <- get_latest_version(
    repo, tags_already_updated = tags_already_updated)
  if (latest_version != requested_version) {
    message(glue::glue(
      "You've requested version {requested_version} of {repo}.\n",
      "The latest available version is {latest_version}.\n",
      "Consider updating."))
  }
}


#' Downloads and then loads a csv/feather file from a specified version of a
#' dataset
#'
#' @inheritParams get_all_basiclevel
#' @inheritParams get_latest_tag
#' @param filename name of a csv/feather file
#'
#' @return tibble for feather files, data.frame for csv files
#'
#' @examples
#' get_df_file('all_basiclevel', 'all_basiclevel.csv', version = '0.1.0')
get_df_file <- function(repo, filename, version = NULL) {
  # Get the up-to-date set of tags
  update_tags(repo)

  # Find out the latest version if none was specified
  if (is.null(version)) {
    requested_version <- get_latest_version(repo, tags_already_updated = TRUE)
    warning(glue::glue(
      'Getting a dataset without specifying a version is highly discouraged.\n',
      'Add ", version = {requested_version}" to the `get_*` function call.'))
  } else {
    requested_version <- version
    # Check if there is a newer version
    check_for_updates(repo = repo, requested_version = requested_version,
                      tags_already_updated = TRUE)
  }

  # Download the file
  checkout_tag(repo, tag = requested_version)

  # Load the file
  file_path <- file.path(blab_data, repo, filename)
  if (endsWith(file_path, ".csv")) {
    result <- read.csv(file_path)
  } else if (endsWith(file_path, ".feather")) {
    result <- arrow::read_feather(file_path)
  }
  print(paste("reading file: ", file_path))
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
