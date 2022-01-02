
#' Get the all_basiclevel data
#'
#' @param branch git branch to pull from
#' @param commit git commit to pull
#' @param type "feather" or "csv". defaults to "feather"
#'
#' @return a tibble containing the all_basicalevel data
#' @export
#'
#' @examples
#' # get the latest
#' all_bl <- get_all_basiclevel()
#'
#' # get version at a specific commit
#' all_bl <- get_all_basiclevel(commit='833356d604fa2ea020d7984a7f6be612ffea862c')
get_all_basiclevel <- function(branch = NULL, commit = NULL, type="feather") {
  switch(type,
         "csv" = get_df_file('all_basiclevel', "all_basiclevel.csv", branch, commit),
         "feather" = get_df_file('all_basiclevel', "all_basiclevel.feather", branch, commit)
         )
}


#' Get the CDI spreadsheet for SEEDLingS babies
#'
#' @param branch git branch to pull from
#' @param commit git commit to pull
#' @param type "feather" or "csv". defaults to "feather"
#'
#' @return a tibble containing the SEEDLingS CDI spreadsheet
#' @export
#'
#' @examples
#' cdi <- get_cdi_spreadsheet()
get_cdi_spreadsheet <- function(branch = NULL, commit = NULL, type="feather") {
  switch(type,
         "csv" = get_df_file('cdi_spreadsheet', "cdi.csv", branch, commit),
         "feather" = get_df_file('cdi_spreadsheet', "cdi.feather", branch, commit)
         )
}


#' The the motor questionaire spreadsheet for the SEEDLingS babies
#'
#' @param branch git branch to pull from
#' @param commit git commit to pull
#' @param type "feather" or "csv". defaults to "feather"
#'
#' @return a tibble contaiing the SEEDLingS Motor Questionaire spreadsheet
#' @export
#'
#' @examples
get_motor_spreadsheet <- function(branch = NULL, commit = NULL, type="feather") {
  switch(type,
         "csv" = get_df_file('motor_spreadsheet', "motor.csv", branch, commit),
         "feather" = get_df_file('motor_spreadsheet', "motor.feather", branch, commit)
         )
}


#' Get the reliability spreadsheets
#'
#' @param av either "audio" or "video"
#' @param month the month as a string, e.g. "06"
#' @param branch git branch to pull from
#' @param commit git commit to pull
#'
#' @return
#' @export
#'
#' @examples
#' audio_06_rel <- get_reliability("audio", "06")
get_reliability <- function(av, month, branch = NULL, commit = NULL) {
  fname <- paste0(av, "_", month, ".csv")
  get_df_file('reliability', fname, branch, commit)
}

get_df_file <- function(repo, filename, branch, commit) {
  sync_repo(repo, branch, commit)
  file_path <- file.path(blab_data, repo, filename)
  if (endsWith(file_path, ".csv")) {
    result <- read.csv(file_path)
  } else if (endsWith(file_path, ".feather")) {
    result <- arrow::read_feather(file_path)
  }
  print(paste("reading file: ", file_path))
  return(result)
}
