#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions were removed from blabr. If there's a known replacement,
#' calling the function will tell you about it.
#'
#' @keywords internal
#' @name defunct
NULL

#' @usage # Deprecated in 0.22.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
load_tsv <- function(fixrep_path, val_guess_max = 100000) {
  lifecycle::deprecate_stop(
    "0.22.0", "load_tsv()",
    details = c(
      i = glue::glue(
        "If you are reading a fixation or a message report, use",
        "`read_fixations_report()` or `read_messages_report()` instead."),
      i = "If you are reading a .tsv file, use `readr::read_tsv()` instead."))
}

#' @export
#' @rdname defunct
fixations_report <- function(fixrep_path, val_guess_max = 100000,
                             remove_unfinished = TRUE, remove_practice = TRUE) {
  lifecycle::deprecate_stop(
    "0.22.0", "fixations_report()", "read_fixations_report()")
}

#' @export
#' @rdname defunct
binifyFixations <- function(
    gaze, binSize = 20, keepCols=c("Subject", "TrialNumber", "Target", "T"),
    maxTime = NULL) {
  lifecycle::deprecate_stop(
    "0.22.0", "binifyFixations()", "read_fixation_report()")
}

#' @export
#' @rdname defunct
get_windows <- function(fix_mes_age,
                        bin_size = 20,
                        nb_1 = 18,
                        short_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$short,
                        med_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$med,
                        long_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$long) {
  lifecycle::deprecate_stop("0.22.0", "get_windows()",
                            "assign_time_windows()")
}

#' @export
#' @rdname defunct
FindLowData <- function(gazeData,
                        subsetWin,
                        window_size = NULL,
                        nb_2 = 0,
                        binSize = 20) {
  lifecycle::deprecate_stop("0.22.0", "FindLowData()",
                            "tag_low_data_trials()")
}


#' @export
#' @rdname defunct
RemoveLowData <- function(gazeData,
                          subsetWin,
                          # maxBins = NULL,
                          # maxMissing = NULL,
                          window_size = NULL,
                          nb_2 = 0,
                          binSize = 20,
                          propt = "propt",
                          timeBin = "timeBin",
                          Trial = "Trial",
                          SubjectNumber = "SubjectNumber") {
  lifecycle::deprecate_stop("0.22.0", "RemoveLowData()",
                            "tag_low_data_trials()")
}


#' @export
#' @rdname defunct
RemoveFrozenTrials <- function(gazeData,
                               Trial,
                               SubjectNumber,
                               gaze) {
  lifecycle::deprecate_stop("0.22.0", "RemoveFrozenTrials()",
                            "FindFrozenTrials()")
}
