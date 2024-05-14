object2string <- function(obj){
  # Gets string from name of the object, ex object2string(blop) returns "blop"
  deparse(substitue(obj))
}


string2object <- function(string_name, val){
  assign(string_name, val)
  # how to return it?
  # eval(parse(text = string_name))
}


#' Convert all character columns to factors
#'
#' @param df Dataframe whose columns need conversion.
#'
#' @return Dataframe with all character columns converted to factors.
#' @export
characters_to_factors <- function(df){
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],
                                         as.factor)
  df
}


DEFAULT_WINDOWS_UPPER_BOUNDS <- list(short = 2000,
                                     med = 3500,
                                     long = 5000)


#' Read EyeLink fixation/message report file
#'
#' @param report_path Report file path.
#  issue: Do not use this parameter by default. Instead, raise an error if there
#    were problems during reading and suggest setting `guess_max` to the
#    smallest number that doesn't lead to problems. The current version is
#    unnecessarily slow.
#' @param guess_max Passed to `readr::read_tsv`. Default is 100000. Decreasing
#'   this number can speed up reading, just make sure there were no problems
#'   during reading by looking out for warning from `readr` or by passing the
#'   output dataframe to `readr::problems`.
#' @param remove_unfinished Removes lines where the `order` column is NA. Does
#'   not work in the current version because no value are interpreted as NA.
#' @param remove_practice Only keeps rows where the `practice` column is "n".
#'
#' @return
#' @keywords internal
# @noRd
read_report <- function(report_path,
                        guess_max = 100000,
                        remove_unfinished = TRUE,
                        remove_practice = TRUE){
  # load tsv file
  # issue: Switch to na = '.'. na = character() implicitly makes readr treat
  #   any column with missing values as character. There is no need to do this.
  report <- readr::read_tsv(report_path,
                            na = character(),
                            guess_max = guess_max)

  # remove incomplete studies
  # note: I don't know why this'd work or what "studies" means here. Zhenya.
  # issue: Because we are doing na = character() in read_tsv, `order` is never
  #   NA so this isn't doing anything at all.
  if (remove_unfinished){
    report <- report %>%
      dplyr::filter(!is.na(order))
  }

  # remove practice rows
  # issue: Assert that practice is "y", "n" or NA/'.'.
  if (remove_practice){
    report <- report %>%
      dplyr::filter(practice == "n")
  }

  return(report)
}

#' Read EyeLink fixation report file
#'
#' @inherit read_report
#' @export
read_fixation_report <- function(report_path,
                                 guess_max = 100000,
                                 remove_unfinished = TRUE,
                                 remove_practice = TRUE) {
  read_report(report_path = report_path,
              guess_max = guess_max,
              remove_unfinished = remove_unfinished,
              remove_practice = remove_practice)
}

#' Read EyeLink message report file
#'
#' @inherit read_report
#' @export
#'
# note: The parameters are copied here instead of using `@inheritDotParams`
#   and function(...) read_report(...) in order th have parameter autocomplete.
read_message_report <- function(report_path,
                                guess_max = 100000,
                                remove_unfinished = TRUE,
                                remove_practice = TRUE) {

  read_report(report_path = report_path,
              guess_max = guess_max,
              remove_unfinished = remove_unfinished,
              remove_practice = remove_practice)
}

#' Convert a dataframe of fixation intervals to an evenly spaced timeseries
#'
#' - The fixations are first dropped/trimmed if `t_max` is set.
#' - The fixation interval boundaries are then rounded up to the nearest
#'   multiples of `t_step`.
#' - Each fixation row is then duplicated as many time as there are time points
#'   (also multiples of `t_step`) falling within the rounded up intervals. The
#'   timepoins are stored in a new column `time`.
#' - If a timepoint falls within multiple fixations, only the row with the
#'   later fixation is kept.
#' - If a timepoint doesn't fall within any fixations in a given trial, it won't
#'   appear in the output at all.
#'
#' @param fixations A dataframe that must minimally contain the following
#'   columns:
#'   - `recording_id` and `trial_index` that uniquely identify trials.
#'   - `t_start`, `t_end` - the start and end times of the
#'     fixations in ms. Use `read_*_report`, `split_*_report`, and
#'     `merge_split_reports` to get such a dataframe.
#' @param t_step The time step in ms.
#' @param t_max Optional. The maximum time in ms. Fixations that start after
#'  this time will be removed and the end time of fixations that end after this
#'  time will be set to this time.
#'
#' @return A dataframe with similar columns as the input dataframe but with
#'   many more rows. Here are the differences in columns:0
#' - A new column `time` was added that contains the time points that are
#'   multiples of `t_step` and are within the fixation interval after it was
#'   rounded up to the nearest multiple of `t_step`.
#' - The `t_start` and `t_end` columns were renamed to `fixation_start` and
#'   `fixation_end`.
#' - Some fixations may have been dropped if they started after `t_max` or if
#'   they were all in one time bin and it was shared with another fixation.
#'
#' @export
#'
#' @examples
#' fixations <- data.frame(
#'   recording_id = seq(1, 3),
#'   trial_index = seq(1, 3),
#'   t_start = c(105, 202, 256),
#'   t_end = c(155, 241, 560),
#'   x = c(0, -50, 50),
#'   y = rep(0, 3))
#' t_step <- 20
#' t_max <- 400
#' fixations_to_timeseries(fixations, t_step, t_max)
#'
fixations_to_timeseries <- function(
  fixations,
  t_step = 20,
  t_max = NULL) {

  assertthat::assert_that(has_columns(fixations, c(
    "recording_id",
    "trial_index",
    "t_start",
    "t_end")))

  if (!is.null(t_max)) {
    # Drop all fixations that start after the t_max
    fixations <- subset(fixations, t_start < t_max)
    # Trim fixation end times to be less than t_max
    fixations$t_end[fixations$t_end > t_max] <- t_max
  }

  # Match fixations to a dataframe with timepoints from the start of the
  # earliest fixation to the end of the latest fixation
  fixation_timeseries <- fixations %>%
    dplyr::mutate(
      dplyr::across(c(t_start, t_end),
                    ~ ceiling(.x / t_step) * t_step,
                    .names = '{.col}_rounded')) %>%
    dplyr::inner_join(
      tibble(time = seq(min(.$t_start_rounded),
                        max(.$t_end_rounded),
                        by = t_step)),
      by = dplyr::join_by(between(y$time,
                                  x$t_start_rounded, x$t_end_rounded))) %>%
    dplyr::select(-t_start_rounded, -t_end_rounded) %>%
    dplyr::rename(
      fixation_start = t_start,
      fixation_end = t_end)

  # There is a border case in which two fixations share a time point resulting
  # in two rows with the same `time`. Ex.:
  # fixation intervals (1, 41) and (49, 69) will be rounded to (20, 60) and
  # (60, 80) and then expanded to time points [20, 40, 60] and [60, 80]. We will
  # use data from the second fixation at time point 60 in such cases.
  #
  # issue: This a fragile way to handle this because it relies on the fixations
  #   being sorted by `t_start` and `t_end`. Either sort the fixations or find
  #   (recording_id, trial_index, time) duplicates and keep the row with the
  #   highest fixation_start. In practice, this is probably fine because the
  #   fixation report *is* sorted and so should the fixations table be, unless
  #   someone intentionally reorders at some step in the pipeline.
  fixation_timeseries <- subset(
    fixation_timeseries,
    c(time[2:length(time)] != time[1:(length(time)-1)], TRUE))

  return(fixation_timeseries)
}


#' (no docs yet) Find key press issues and create a doc to check and potentially correct them
#'
#' @param data
#' @param study
#' @param practice_trials
#' @param output_dir
#' @param out_csv
#'
#' @return
#' @export
#'
#' @examples
keypress_issues <- function(data, study = "eye_tracking_study", practice_trials = c("p1", "p2", "p3", "p4"), output_dir = "../data/", out_csv = FALSE){ # or study = NULL and take data[:-4]
  keypress_issues <- data %>%
    dplyr::filter(RT == -1 & !Trial %in% practice_trials) %>%
    group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget)%>%
    distinct(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, RT, AudioTarget)
  if (out_csv){
    write_csv(keypress_issues, paste(output_dir, "keypress_issues_", study, "_", str_replace_all(Sys.time(), ' ', '_'), ".csv", sep=''))
  }
  keypress_issues
}


# retrieve corrected kp OR retrieve correct late target onset // separate functions?
# TODO: zh: Update/delete the comment. I think it is out of date and the functions have already been separated


#' (no docs yet) Load file with manually corrected key presses or late target onsets
#'
#' @param filename
#' @param drop_list
#'
#' @return
#' @export
#'
#' @examples
keypress_retrieved <- function(filename, drop_list = c("video_pop_time", "video_targetonset", "notes")){
  retrieved_kp <- read_excel(filename) %>%
    dplyr::select(-one_of(drop_list))
  retrieved_kp #return?
}


#' (no docs yet) Create message report with corrected key presses
#'
#' @param mesrep_all
#' @param fixed_kp
#' @param final_columns
#'
#' @return
#' @export
#'
#' @examples
get_mesrep <- function(mesrep_all, fixed_kp, final_columns = c("RECORDING_SESSION_LABEL", "CURRENT_MSG_TIME", "TRIAL_INDEX", "AudioTarget", "Trial")){

  mesrep_temp <- mesrep_all %>% #subset of mesrep_all
    dplyr::select(one_of(final_columns, "CURRENT_MSG_TEXT", "RT")) %>%
    mutate(RT = as.numeric(as.character(RT)),
           Trial=as.numeric(Trial))

  good_kp_mesrep <- mesrep_temp %>% # message reports corresponding to good key presses
    dplyr::filter(CURRENT_MSG_TEXT == "EL_BUTTON_CRIT_WORD") %>%
    dplyr::select(one_of(final_columns))

  fixed_kp_mesrep <- mesrep_temp %>% # message reports corresponding to fixed key presses
    dplyr::filter(CURRENT_MSG_TEXT=="PLAY_POP" & RT=="-1") %>%
    left_join(fixed_kp %>% dplyr::filter(outcome=="FIX")) %>%
    dplyr::rename(PLAY_POP=CURRENT_MSG_TIME) %>%
    mutate(CURRENT_MSG_TIME = PLAY_POP+ms_diff) %>%
    dplyr::select(one_of(final_columns))

  mesrep <- fixed_kp_mesrep %>% # merge both reports
    bind_rows(good_kp_mesrep)

  mesrep # return?
}

#' (no docs yet) Find late target onsets and create a doc to check and potentially correct them
#'
#' @param data
#' @param max_time
#' @param study
#' @param output_dir
#' @param out_csv
#'
#' @return
#' @export
#'
#' @examples
get_late_target_onset <- function(data, max_time = 6000, study = "eye_tracking_study", output_dir = "../data/", out_csv = FALSE){
  late_target_onset <- data %>%
    dplyr::filter(CURRENT_MSG_TIME>max_time) %>%
    group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget) %>%
    distinct(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget)
  if(out_csv){
    write_csv(late_target_onset, paste(output_dir, "late_target_onset_", study, "_", str_replace_all(Sys.time(), ' ', '_'), ".csv", sep=''))
  }
  late_target_onset
}

# retrieve corrected kp OR retrieve correct late target onset // separate functions? ## IDENTICAL TO KP FUNCTION
# TODO: zh: Update/delete the comment. I think it is out of date and the functions have already been separated

#' (no docs yet) Load file with manually corrected late target onsets
#'
#' @param filename
#' @param drop_list
#'
#' @return
#' @export
#'
#' @examples
late_target_retrieved <- function(filename, drop_list = c("video_pop_time", "video_targetonset", "notes")){
  retrieved_late_target <- read_excel(filename) %>%
    dplyr::select(-one_of(drop_list))
  retrieved_late_target #return?
}


#' Assigns binned fixations to a short, medium, and long time windows
#'
#' @param fixation_timeseries A fixation timeseries dataframe that is required to minimally contain these columns:
#'  - `target_onset` (numeric): Time of target onset in milliseconds.
#'  - `time` (numeric): Time in ms from the start of the trial.
#' @param t_step The time step in ms. MUST match the one used in `fixations_to_timeseries`.
#' @param t_start Time when the windows starts, defaults to 360. Must be a multiple of `bin_size`.
#' @param short_window_time,med_window_time,long_window_time End timepoints of the short, medium, and long windows in milliseconds from the target word onset. Default to `r DEFAULT_WINDOWS_UPPER_BOUNDS$short`, `r DEFAULT_WINDOWS_UPPER_BOUNDS$med`, and `r DEFAULT_WINDOWS_UPPER_BOUNDS$long`, respectively.
#'
#' @details
#' Parameter `t_start` defaults to 360, because it is the closest bin to 367 ms, which is the magic window onset from Fernald et al. (2008).
#'
#' A note on the window and bin boundaries. Let's use `nb_1 = 18` and the long window as an example. Both bins located exactly 360 ms and 5000 ms after the target onset will be counted as belonging to the long window (`longwin == "Y"`). In the case of the lower bound, this creates a slight inconsistency with the `Nonset` column: in most cases, the bin with `Nonset` equal to 360 ms will not belong to any windows, but in ~1/20 of the cases where the bin is exactly at 360 ms from the target onset, it will belong to all windows.
#'
#' @return The input dataframe with the following columns added:
#' - `prewin` (factor): Whether a time bin comes before the target onset. Level labels are "Y" and "N".
#' - `shortwin`, `medwin`, `longwin` (factor): Whether a time bin is in the short, medium, or long window, respectively. Level labels are "Y" and "N".
#' - `whichwin_short`, `whichwin_med`, `whichwin_long` (factor): Whether a time bin
#'   - is in the short, medium, or long window (level label "short", "medium", or "long", respectively),
#'   - comes before the target onset (level "pre"), or
#'   - neither (level "neither").
#' - `t_onset` (numeric): Time (in ms) from the target onset rounded up to the nearest multiple of `bin_size`.
#'
#' @export
assign_time_windows <- function(
  fixation_timeseries,
  t_step = 20,
  t_start = 360,  # floor(367 / 20)
  short_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$short,
  med_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$med,
  long_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$long) {

    original_columns <- colnames(fixation_timeseries)

    # Assert that t_start is a multiple of bin_size
    assertthat::assert_that(
      t_start %% t_step == 0,
      msg = "t_start must be a multiple of bin_size")

    # Check that the time and target_onset columns are present and are numeric
    assertthat::assert_that(
      all(c("time", "target_onset") %in% colnames(fixation_timeseries)),
      msg = "fixation_timeseries must contain columns 'time' and 'target_onset'")

    assertr::assert(fixation_timeseries, is.numeric, time, target_onset)

    # Check that target_onset is not NA and is a positive number
    assertthat::assert_that(
      !any(is.na(fixation_timeseries$target_onset)) &&
      all(fixation_timeseries$target_onset > 0),
      msg = "target_onset must be a non-empty positive number")

    # Adds '{size}win' and 'whichwin_{size}' columns where size is "short",
    # "med", or "long"
    add_window_columns <- function(df, size, window_end_ms) {
      df %>%
        dplyr::mutate(
          sizewin = dplyr::between(.data$time_shifted_ms,
                                   .env$t_start, .env$window_end_ms),
          whichwin_size = dplyr::case_when(
            # Both prewin and sizewin are boolean at this point
            .data$prewin ~ "pre",
            .data$sizewin ~ .env$size,  # e.g., "short"
            TRUE ~ "neither")) %>%
        dplyr::rename(
          '{size}win' := .data$sizewin,
          'whichwin_{size}' := .data$whichwin_size)
    }

    fixation_timeseries %>%
      dplyr::mutate(
        time_shifted_ms = time - target_onset,
        prewin = time_shifted_ms <= 0) %>%
      add_window_columns("short", short_window_time) %>%
      add_window_columns("med", med_window_time) %>%
      add_window_columns("long", long_window_time) %>%
      dplyr::mutate(
        dplyr::across(
          c(prewin, shortwin, medwin, longwin),
          ~ as.factor(ifelse(.x, "Y", "N")))) %>%
      dplyr::mutate(t_onset = ceiling(time_shifted_ms / t_step) * t_step) %>%
      dplyr::select(dplyr::all_of(original_columns),
                    prewin,
                    shortwin, medwin, longwin,
                    whichwin_short, whichwin_med, whichwin_long,
                    t_onset)}


#' Mark "low-data" trials
#'
#' Trials are considered to be "low-data" if less than one third of the window of interest contains fixations on either area of interest. Bins that won't count toward that one third are:
#'
#' - Bins that don't have a fixation due to being in a saccade or in a period with lost track. We are assuming that bins in `gazeData` were created by `binifyFixation` which skips such bins completely - there won't be rows with them.
#' - Bins with fixations outside of the areas of interest, including off-screen fixations. We will identify such fixations by NA values in the `propt` column.
#'
#' Or looking at it from the opposite perspective, bins that do count toward the one third are bins in `gazeData` that:
#' - Are present in `gazeData`.
#' - Are in the window of interest ("Y" in the column specified by `subsetWin`).
#' - Have fixations on either are of interest (non-NA values in the `propt` column).
#'
#' @param fixation_timeseries  A fixations dataframe that is required to minimally contain these columns:
#' - `recording_id` and `trial_index` to uniquely trials.
#' - `is_good_timepoint` - only rows with TRUE in this column will be counted as having data.
#' - Column specified by `window_column`.
#' @param window_column Name of the column that indicates (using factor label "Y") bins that belong to the window which will be tested for insufficient data.
#' @param t_start Lower bound of the window of interest in milliseconds from the target onset.
#' @param t_end Upper bound of the window of interest in milliseconds from the target onset. If a non-default upper bound was used during the assignment of bins to windows in the `get_windows` call, then that value MUST be supplied here. If NULL (default), the default corresponding to the `subsetWin` will be used.
#' @inheritParams assign_time_windows
#'
#' @return The input dataframe with boolean `is_trial_low_data` column added.
#' @export

tag_low_data_trials <- function(
    fixation_timeseries,
    window_column,
    t_start,
    t_end = NULL,
    t_step = 20,
    min_fraction = 1/3) {

  assertthat::assert_that(has_columns(
    fixation_timeseries,
    c("recording_id", "trial_index", "is_good_timepoint", window_column)))

  original_columns <- colnames(fixation_timeseries)

  # If the window size is not provided, use the default for the window indicator column
  if (is.null(t_end)) {
    assertthat::assert_that(
      window_column %in% c("shortwin", "medwin", "longwin"),
      msg = glue::glue("If `t_end` is NULL, `window_column` must be one of",
                       " 'shortwin', 'medwin', or 'longwin'."))

    # Look up t_end for short/med/long window
    window_size_label <- stringr::str_remove(window_column, "win$")
    t_end <- DEFAULT_WINDOWS_UPPER_BOUNDS[[window_size_label]]
  }

  min_points_with_data <- floor((t_end - t_start) / t_step * min_fraction)

  tagged <- fixation_timeseries %>%
    assertr::verify(!!sym(window_column) %in% c('Y', 'N')) %>%
    dplyr::mutate(
      # issue: check that `window_column` only takes "Y" or "N" values
      in_time_window = !!sym(window_column) == "Y",
      has_data = in_time_window & is_good_timepoint) %>%
    dplyr::group_by(recording_id, trial_index) %>%
    dplyr::mutate(
      bins_with_data_count = sum(has_data),
      is_trial_low_data = bins_with_data_count < min_points_with_data) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(original_columns), is_trial_low_data)

  return(tagged)

}


# Zhenya: issue: Deprecate this function, tell to use tag_low_data_trials
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
  # this function is for making sure there's at least X amount of data in a trial; there are two potential sources of missing data: 1) off screen 2) elsewhere, not in an interest area
  #gazeData is the dataset, subsetWin is the column name that contains "Y" indicating that's the part in which we are making sure there's enough data,
  #maxBins is how many bins there could have been in the trial,
  #minLength is how much data is the minimum to keep the trial, (not arg)
  #maxMissing= in real time, how many ms of data need to be there

  #binSize is what size of bins the fixations were turned into, this will usually be 20ms,
  #propt is proportion of target looking,

  #timeBin is the (20 ms) bin the trial that each line is

  if (is.null(window_size)){
    if (subsetWin=="longwin"){
      window_size <- DEFAULT_WINDOWS_UPPER_BOUNDS$long
    } else if (subsetWin=="medwin"){
      window_size <- DEFAULT_WINDOWS_UPPER_BOUNDS$med
    } else if (subsetWin=="shortwin"){
      window_size <- DEFAULT_WINDOWS_UPPER_BOUNDS$short
    }
  }

  maxBins <- as.integer((window_size - nb_2)/binSize)
  maxMissing <- as.integer((window_size - nb_2) - ((window_size - nb_2)/3))

  gazeData2 <- gazeData %>%
    dplyr::filter(gazeData[,subsetWin] == "Y")

  print(dim(gazeData2))
  #1) offscreen: those timebins don't exist with my version of binifyFixations so how many timebins
  # are there in relation to the maximum given the trial length?

  number_timebins <- gazeData2 %>%
    group_by(Trial, SubjectNumber) %>%
    tally() %>%
    mutate(bins = n) %>%
    dplyr::select(-n)%>%
    mutate(missing_bins = maxBins - bins)

  #2)elsewhere: let's see how many NAs we have for propt, our proportion of target looking

  elsewhere_bins <- gazeData2 %>%
    group_by(Trial, SubjectNumber) %>%
    tally(is.na(propt)) %>%
    mutate(elsewhere_bins = n) %>%
    dplyr::select(-n)

  # This is all the low data from either source

  lowdata_bins <- left_join(number_timebins, elsewhere_bins) %>%
    mutate(lowdata = missing_bins + elsewhere_bins) %>%
    mutate(missing_TF = lowdata >floor(maxMissing/binSize)) %>%
    dplyr::select(Trial, SubjectNumber, missing_TF)

  gazeData <- left_join(gazeData, lowdata_bins) %>%
    dplyr::filter(missing_TF == F) %>%
    dplyr::select(-missing_TF)

  message("Low data rows have been removed. To identify them in a new column without removing them, use blabr::FindLowData.")
  return(gazeData)
}


get_pairs <- function(data, study = "eye_tracking", output_dir = '../data/', out_csv = FALSE){
  res <- data %>%
    group_by(SubjectNumber) %>%
    distinct(Pair)
  if (out_csv){
    name <- paste(output_dir, "pairs_", study, "_", str_replace_all(Sys.time(), ' ', '_'), ".csv", sep='')
    print(name)
    write_csv(res, name)
  }
  res
}


outlier <- function(cross_item_mean_proptcorrTT, num_sd=3) {
  (cross_item_mean_proptcorrTT >
     (mean(cross_item_mean_proptcorrTT) +
        num_sd*(sd(cross_item_mean_proptcorrTT))) |
     cross_item_mean_proptcorrTT <
     (mean(cross_item_mean_proptcorrTT) -
        num_sd*(sd(cross_item_mean_proptcorrTT))))
}


FindFrozenTrials <- function(gazeData,
                             Trial,
                             SubjectNumber,
                             gaze) {

 gazeData <-  gazeData %>%
    group_by(SubjectNumber, Trial) %>%
    mutate(frozen = ifelse(length(levels(fct_explicit_na(gaze, na_level = "NA"))) == 1, T, F))

 message("Column added identifying trials where gaze stayed in one interest area for whole trial (frozen = T).")
 return(gazeData)
}


# Zhenya: issue: Deprecate this functions, tell users to use FindFrozenTrials
RemoveFrozenTrials <- function(gazeData,
                             Trial,
                             SubjectNumber,
                             gaze) {

  gazeData <-  gazeData %>%
    group_by(SubjectNumber, Trial) %>%
    mutate(frozen = ifelse(length(levels(fct_explicit_na(gaze, na_level = "NA"))) == 1, T, F)) %>%
    dplyr::filter(frozen == F) %>%
    dplyr::select(-frozen)

  message("frozen trials have been removed. To identify them in a new column without removing them, use blabr::FindFrozenTrials.")
  return(gazeData)
}
