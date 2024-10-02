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


#' @export
DEFAULT_WINDOWS_UPPER_BOUNDS <- list(short = 2000,
                                     med = 3500,
                                     long = 5000)


#' Split a fixation or message report into hierarchical dataframes
#'
#' @param drop_empty_columns Logical. If TRUE, columns that are all NA will be
#'   dropped.
#'
#' @keywords internal
split_report <- function(report,
                         drop_empty_columns = TRUE) {

  # Rename the key columns
  rename_map <- c()
  recording_key <- 'recording_id'
  trial_key <- 'trial_index'
  rename_map[recording_key] <- 'RECORDING_SESSION_LABEL'
  rename_map[trial_key] <- 'TRIAL_INDEX'
  report <- report %>% dplyr::rename(dplyr::all_of(rename_map))


  if (isTRUE(drop_empty_columns)) {
    report <- report %>%
      dplyr::select_if(~ !all(is.na(.x)))
  }

  find_constant_columns <- function(df, .by = NULL) {
    constant_columns <- df %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(),
                      ~ length(unique(.x)) == 1),
        .by = .by) %>%
      dplyr::select(-dplyr::all_of(.by)) %>%
      dplyr::select(dplyr::where(~ all(.x))) %>%
      colnames

    return(constant_columns)
  }

  # Experiment-level data
  experiment_data_columns <- report %>%
    find_constant_columns()

  experiment_data <- report %>%
    dplyr::select(dplyr::all_of(experiment_data_columns)) %>%
    dplyr::distinct()

  report <- report %>%
    dplyr::select(-dplyr::all_of(colnames(experiment_data)))

  # Recording-level data
  recording_data_columns <- report %>%
    find_constant_columns(.by = recording_key)

  recording_data <- report %>%
    dplyr::select(dplyr::all_of(c(recording_key,
                                  recording_data_columns))) %>%
    dplyr::distinct()

  report <- report %>%
    dplyr::select(-dplyr::all_of(recording_data_columns))

  # Trial-level data
  trial_keys <- c(recording_key, trial_key)

  trial_data_columns <- report %>%
    find_constant_columns(.by = trial_keys)

  trial_data <- report %>%
    dplyr::select(dplyr::all_of(c(trial_keys,
                                  trial_data_columns))) %>%
    dplyr::distinct()

  # The rest
  report <- report %>%
    dplyr::select(-dplyr::all_of(trial_data_columns)) %>%
    dplyr::select(dplyr::all_of(trial_keys),
                  dplyr::everything())

  return(list(experiment = experiment_data,
              recordings = recording_data,
              trials = trial_data,
              the_rest = report))

}



#' Split a fixation report into a list of hierarchical dataframes
#'
#' @param report Fixation report dataframe as created by `read_fixation_report`.
#' @inheritParams split_report
#' @param drop_saccades,drop_prev_and_next Logical. If TRUE, columns related to
#'   saccades and previous/next fixations/saccades will be dropped.
#'
#' @export
split_fixation_report <- function(report,
                                  drop_empty_columns = TRUE,
                                  drop_saccades = TRUE,
                                  drop_prev_and_next = TRUE) {

  if (isTRUE(drop_saccades)) {
    report <- report %>%
      # ignore.case is TRUE by default, added here for transparency
      dplyr::select(
        -dplyr::starts_with('previous_sac_', ignore.case = TRUE),
        # there are no current saccades in fixation reports
        -dplyr::starts_with('next_sac_', ignore.case = TRUE))
  }

  if (isTRUE(drop_prev_and_next)) {
    report <- report %>%
      dplyr::select(-dplyr::starts_with('previous_', ignore.case = TRUE),
                    -dplyr::starts_with('next_', ignore.case = TRUE))
  }

  data <- split_report(report,
                       drop_empty_columns = drop_empty_columns) %>%
    list_rename(fixations = "the_rest")

  # Rename a few columns
  data$fixations <- data$fixations %>%
    assertr::verify(
      assertr::has_all_names(
        'CURRENT_FIX_START',
        'CURRENT_FIX_END',
        'CURRENT_FIX_X',
        'CURRENT_FIX_Y')) %>%
    dplyr::rename(
      t_start = .data$CURRENT_FIX_START,
      t_end = .data$CURRENT_FIX_END,
      x = .data$CURRENT_FIX_X,
      y = .data$CURRENT_FIX_Y) %>%
    dplyr::select(
      dplyr::all_of(c('recording_id', 'trial_index',
                      't_start', 't_end', 'x', 'y')),
      dplyr::everything())

  return(data)
}

#' Split a message report into a list of hierarchical dataframes
#'
#' @param report Message report dataframe as created by `read_message_report`.
#' @inheritParams split_report
#'
#' @export
split_message_report <- function(report,
                                 drop_empty_columns = TRUE
                                 # ek2ek: implement for videos, like in VNA
                                 # compress_repeating = TRUE
) {
  data <- split_report(report,
                       drop_empty_columns = drop_empty_columns) %>%
    list_rename(messages = "the_rest")

  return(data)
}

#' Checks that common columns in tables derived from the message report are
#' identical (experiment and recording tables) or a subset of (trials table)
#' to the ones derived from the fixations report.
#'
#' @noRd
check_split_data_consistency <- function(mes_rep_data, fix_rep_data) {

  convert_all_to_character <- function(df) {
    df %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  }

  assert_common_cols_identical <- function(df1, df2, msg) {
    common_cols <- intersect(colnames(df1), colnames(df2))
    assertthat::assert_that(
      assertthat::are_equal(
        df1[, common_cols] %>% convert_all_to_character,
        df2[, common_cols] %>% convert_all_to_character),
      msg = msg)
  }

  assert_common_cols_is_subset <- function(df1, df2, msg) {
    common_cols <- intersect(colnames(df1), colnames(df2))
    assertthat::assert_that(assertthat::are_equal(
      dplyr::anti_join(
        df1[, common_cols] %>% convert_all_to_character,
        df2[, common_cols] %>% convert_all_to_character,
        by = common_cols
      ) %>%
        nrow,
      0),
      msg = msg)
  }

  non_identical_error_msg <- glue::glue("
      The {{level}}-level info differs between the fixation and message \\
      reports. Update the `${{level}}` table in the split version of either \\
      or both so that the information in the common columns is identical \\
      between the two if as.character is applied -- then try merging again.")

  assert_common_cols_identical(
    fix_rep_data$experiment,
    mes_rep_data$experiment,
    msg = glue::glue(non_identical_error_msg, level = "experiment"))

  assert_common_cols_identical(
    fix_rep_data$recordings,
    mes_rep_data$recordings,
    glue::glue(non_identical_error_msg, level = "recordings"))

  not_subset_error_msg <- glue::glue("
      The trial-level info in the fixation report is not a subset of that in \\
      the message report. Update the `trials` table in the split version of \\
      either or both so that the information in the common columns is a \\
      subset of the other if as.character is applied -- then try merging again.")

  assert_common_cols_is_subset(
    fix_rep_data$trials,
    mes_rep_data$trials,
    msg = not_subset_error_msg)

}


#' Merge tables from split fixation and message reports
#'
#' The main point of merging is to have one list and no repeating data. For
#' example, we don't have two "trials" tables in memory where the one coming
#' from the message report has more trials - we want them merged.
#'
#' @param fix_rep_data List of tables as returned by `split_fixation_report`.
#' @param mes_rep_data List of tables as returned by `split_message_report`.
#'
#' @export
#'
merge_split_reports <- function(fix_rep_data, mes_rep_data) {

  check_split_data_consistency(mes_rep_data, fix_rep_data)

  join_tables <- function(df1, df2, how, key_columns) {
    # Joins tables, first removing redundant columns from the second table.
    # We a

    redundant_cols <- setdiff(intersect(colnames(df1),
                                        colnames(df2)),
                              key_columns)
    df2 <- df2 %>%
      dplyr::select(-dplyr::all_of(redundant_cols))

    how <- match.arg(how, c('cross', 'inner', 'left'))

    if (how == 'cross') {
      assertthat::assert_that(assertthat::are_equal(key_columns, c()))
      return(dplyr::cross_join(df1, df2))
    }

    join_fun <- switch(how,
                       "inner" = dplyr::inner_join,
                       "left" = dplyr::left_join
    )

    joined <- join_fun(
      df1,
      df2,
      by = key_columns,
      relationship = 'one-to-one',
      unmatched = 'error')

    return(joined)
  }

  recording_key <- 'recording_id'
  trial_key <- 'trial_index'

  merged <- list(
    experiment = join_tables(
      mes_rep_data$experiment,
      fix_rep_data$experiment,
      how = "cross",
      key_columns = c()),
    recordings = join_tables(
      mes_rep_data$recordings,
      fix_rep_data$recordings,
      how = "inner",
      key_columns = recording_key),
    trials = join_tables(
      mes_rep_data$trials,
      fix_rep_data$trials,
      how = "left",
      key_columns = c(recording_key, trial_key)),
    fixations = fix_rep_data$fixations,
    messages = mes_rep_data$messages)

  return(merged)

}

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
#' @return A dataframe with the report data.
#' @keywords internal
read_report <- function(report_path,
                        guess_max = 100000,
                        remove_unfinished = TRUE,
                        remove_practice = TRUE){

  report <- readr::read_tsv(report_path,
                            na = '.',
                            guess_max = guess_max)

  # remove incomplete studies
  # note: I don't know why this'd work or what "studies" means here. Zhenya.
  # note: Previously, we had na = character() in read_tsv and `order` was never
  #   NA so this didn't do anything at all. Now, we have na = '.' so this might
  #   change things for existing analyses.
  if (remove_unfinished){
    report <- report %>%
      dplyr::filter(!is.na(.data$order))
  }

  # remove practice rows
  # issue: Assert that practice is "y", "n" or NA.
  if (remove_practice){
    report <- report %>%
      dplyr::filter(.data$practice == "n")
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


#' @noRd
verify_no_overlapping_fixations <- function(fixations_df) {
  fixations_df <- fixations_df %>%
    # I don't want to sort the table because of this so I am going to restore
    # the original order at the end
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    dplyr::group_by(.data$recording_id, .data$trial_index) %>%
    dplyr::arrange(.data$t_start, .data$t_end, .by_group = TRUE) %>%
    dplyr::mutate(overlapping =
                  .data$t_start <= dplyr::lag(.data$t_end, default = -Inf)) %>%
    dplyr::ungroup() %>%
    # Restore original row order
    dplyr::arrange(.data$row_number) %>%
    dplyr::select(-.data$row_number)

  assertthat::assert_that(
    !any(fixations_df$overlapping),
    msg = glue::glue("Overlapping fixations found in the data.",
                     " You'll need to drop the extra ones.",
                     " Search on gitbook for more info."))

  fixations_df %>% dplyr::select(-.data$overlapping)
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

  verify_no_overlapping_fixations(fixations)

  if (!is.null(t_max)) {
    # Drop all fixations that start after the t_max
    fixations <- fixations[fixations$t_start < t_max, ]
    # Trim fixation end times to be less than t_max
    fixations$t_end[fixations$t_end > t_max] <- t_max
  }

  # Match fixations to a dataframe with timepoints from the start of the
  # earliest fixation to the end of the latest fixation
  fixation_timeseries <- fixations %>%
    dplyr::mutate(
      dplyr::across(c(.data$t_start, .data$t_end),
                    ~ ceiling(.x / .env$t_step) * .env$t_step,
                    .names = '{.col}_rounded')) %>%
    dplyr::inner_join(
      dplyr::tibble(time = seq(min(.$t_start_rounded),
                               max(.$t_end_rounded),
                               by = t_step)),
      by = dplyr::join_by(dplyr::between(
        y$time,
        x$t_start_rounded, x$t_end_rounded))) %>%
    dplyr::select(-.data$t_start_rounded, -.data$t_end_rounded) %>%
    dplyr::rename(
      fixation_start = .data$t_start,
      fixation_end = .data$t_end)

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
    dplyr::group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget)%>%
    dplyr::distinct(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, RT, AudioTarget)
  if (out_csv){
    readr::write_csv(
      keypress_issues,
      paste(output_dir, "keypress_issues_", study, "_",
            stringr::str_replace_all(Sys.time(), ' ', '_'), ".csv", sep=''))
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
  retrieved_kp <- readxl::read_excel(filename) %>%
    dplyr::select(-dplyr::one_of(drop_list))
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
    dplyr::select(dplyr::one_of(final_columns, "CURRENT_MSG_TEXT", "RT")) %>%
    dplyr::mutate(RT = as.numeric(as.character(RT)),
           Trial=as.numeric(Trial))

  good_kp_mesrep <- mesrep_temp %>% # message reports corresponding to good key presses
    dplyr::filter(CURRENT_MSG_TEXT == "EL_BUTTON_CRIT_WORD") %>%
    dplyr::select(dplyr::one_of(final_columns))

  fixed_kp_mesrep <- mesrep_temp %>% # message reports corresponding to fixed key presses
    dplyr::filter(CURRENT_MSG_TEXT=="PLAY_POP" & RT=="-1") %>%
    dplyr::left_join(fixed_kp %>% dplyr::filter(outcome=="FIX")) %>%
    dplyr::rename(PLAY_POP=CURRENT_MSG_TIME) %>%
    dplyr::mutate(CURRENT_MSG_TIME = PLAY_POP+ms_diff) %>%
    dplyr::select(dplyr::one_of(final_columns))

  mesrep <- fixed_kp_mesrep %>% # merge both reports
    dplyr::bind_rows(good_kp_mesrep)

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
    dplyr::group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget) %>%
    dplyr::distinct(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget)
  if(out_csv){
    readr::write_csv(
      late_target_onset,
      paste(output_dir, "late_target_onset_", study, "_",
            stringr::str_replace_all(Sys.time(), ' ', '_'), ".csv", sep=''))
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
  retrieved_late_target <- readxl::read_excel(filename) %>%
    dplyr::select(-dplyr::one_of(drop_list))
  retrieved_late_target #return?
}


#' Assigns binned fixations to specified time windows
#'
#' @param fixation_timeseries A fixation timeseries dataframe that must contain at least the following columns:
#'   - `target_onset` (numeric): Time of target onset in milliseconds.
#'   - `time` (numeric): Time in milliseconds from the start of the trial.
#' @param t_step The time step in milliseconds. Must match the one used in `fixations_to_timeseries`.
#' @param t_starts A numeric vector of start times (in milliseconds) for the time windows, relative to the target onset. Each value must be a multiple of `t_step`. If of length 1, it will be recycled to match the length of `t_ends`.
#' @param t_ends A numeric vector of end times (in milliseconds) for the time windows, relative to the target onset. Each value must be a multiple of `t_step`. If of length 1, it will be recycled to match the length of `t_starts`.
#'
#' @details
#' The function assigns each time bin in `fixation_timeseries` to specified time windows defined by `t_starts` and `t_ends` relative to the target onset. For each window, it creates two new columns:
#' - `window_{start}_{end}ms`: Indicates whether the time bin falls within the window from `start` to `end` milliseconds after target onset. Values are "Y" or "N".
#' - `which_window_{start}_{end}ms`: Indicates whether the time bin is in the window (labeled as "{start}_{end}ms"), comes before the target onset ("pre"), or neither ("neither").
#'
#' Time bins located exactly at `t_starts[i]` or `t_ends[i]` are included in the corresponding window (i.e., the intervals are inclusive of the endpoints).
#'
#' The function also adds a `prewin` column indicating whether the time bin comes before the target onset.
#'
#' The `t_onset` column is computed as the time from the target onset, rounded up to the nearest multiple of `t_step`.
#'
#' If you want to use the default BLab time windows, set `t_starts` to `360` and `t_ends` to `DEFAULT_WINDOWS_UPPER_BOUNDS`.
#'
#' @return The input dataframe with the following columns added:
#' - `prewin` (factor): Whether a time bin comes before the target onset. Values are "Y" and "N".
#' - For each window defined by `t_starts[i]` and `t_ends[i]`, the following columns are added:
#'   - `window_{t_starts[i]}_{t_ends[i]}ms` (factor): Whether a time bin is in the window from `t_starts[i]` to `t_ends[i]` milliseconds after target onset. Values are "Y" and "N".
#'   - `which_window_{t_starts[i]}_{t_ends[i]}ms` (factor): Indicates whether the time bin is in the window (labeled as "{t_starts[i]}_{t_ends[i]}ms"), comes before the target onset ("pre"), or neither ("neither").
#' - `t_onset` (numeric): Time (in milliseconds) from the target onset rounded up to the nearest multiple of `t_step`.
#'
#' @export
assign_time_windows <- function(
  fixation_timeseries,
  t_step = 20,
  t_starts, t_ends) {

  # Assert that t_starts and t_ends are multiples of t_step
  check_bounds <- function(bounds, var_name) {
    bounds <- unlist(bounds)

    assertthat::assert_that(
      is.numeric(bounds),
      all(bounds %% t_step == 0),
      msg = glue::glue("{var_name} must be numeric and multiples of t_step"))
  }
  check_bounds(t_starts, "t_starts")
  check_bounds(t_ends, "t_ends")

  # Check that the required columns are present and numeric
  assertthat::assert_that(
    all(c("time", "target_onset") %in% colnames(fixation_timeseries)),
    msg = "fixation_timeseries must contain columns 'time' and 'target_onset'")

  with(fixation_timeseries,
       assert_that(is.numeric(time), is.numeric(target_onset)),
       msg = "Columns 'time' and 'target_onset' must be numeric")

  # Check that target_onset is not NA and is positive
  assertthat::assert_that(
    !any(is.na(fixation_timeseries$target_onset)),
    all(fixation_timeseries$target_onset > 0),
    msg = "Column 'target_onset' must contain non-NA positive values")

  # Keep the list of original columns
  original_columns <- colnames(fixation_timeseries)

  # Recycle t_starts/t_ends so that the user can do smth like (t_start = 360,
  # t_end = c(5000, 10000))
  t_bounds <- data.frame(start = unlist(t_starts),
                         end = unlist(t_ends))

  add_one_window_columns <- function(df, start_ms, end_ms) {
    # as.character is needed to avoid which_window having the "glue" class
    start_end_ms <- as.character(glue::glue('{start_ms}_{end_ms}ms'))
    df %>%
      dplyr::mutate(
        window = dplyr::between(.data$time_shifted_ms,
                                .env$start_ms, .env$end_ms),
        which_window = dplyr::case_when(
          # Both prewin and window are boolean at this point
          .data$prewin ~ "pre",
          .data$window ~ start_end_ms,
          TRUE ~ "neither")) %>%
      dplyr::rename(
        'window_{start_end_ms}' := "window",
        'which_window_{start_end_ms}' := "which_window")
  }

  fixation_timeseries %>%
    dplyr::mutate(
      time_shifted_ms = time - target_onset,
      prewin = time_shifted_ms <= 0) %>%
    {
      purrr::reduce2(
        t_bounds$start, t_bounds$end,
        \(df, t_start, t_end) add_one_window_columns(df, t_start, t_end),
        .init = .)
    } %>%
    dplyr::mutate(
      dplyr::across(
        c(prewin, starts_with('window_')),
        ~ as.factor(ifelse(.x, "Y", "N")))) %>%
    dplyr::mutate(t_onset = ceiling(time_shifted_ms / t_step) * t_step) %>%
    dplyr::select(dplyr::all_of(original_columns),
                  prewin,
                  starts_with('window_'),
                  starts_with('which_window_'),
                  t_onset)
}


#' Tag low-data trials based on fixation timeseries
#'
#' Identifies and marks trials with insufficient data based on the proportion of time points within a specified time window that contain valid data. A trial is considered "low-data" if less than `min_fraction` of the time window contains valid data.
#'
#' Time points counted as having data meet both of the following criteria:
#' - `is_good_timepoint` is `TRUE`. This column is typically created with a condition like `mutate(is_good_timepoint = some_condition)`. The definition can vary between studies.
#' - The time bin is within the window of interest, indicated by the `window_column` or specified by `t_start` and `t_end`.
#'
#' @param fixation_timeseries A dataframe containing fixation timeseries data. It must minimally contain the following columns:
#'   - `recording_id`: Identifier for the recording session.
#'   - `trial_index`: Index or identifier for the trial.
#'   - `is_good_timepoint`: Logical vector indicating valid time points (`TRUE` or `FALSE`).
#'   - If `window_column` is provided: a column with that name indicating time bins within the window of interest. Values should be `"Y"` or `"N"`.
#' @param window_column (Optional) A string specifying the name of the column that indicates (using the factor label `"Y"`) the time bins that belong to the window being tested for insufficient data. Either `window_column` or both `t_start` and `t_end` must be supplied.
#' @param t_start (Optional) Numeric value specifying the lower bound of the window of interest in milliseconds from the target onset. Must be provided along with `t_end` if `window_column` is not supplied.
#' @param t_end (Optional) Numeric value specifying the upper bound of the window of interest in milliseconds from the target onset. Must be provided along with `t_start` if `window_column` is not supplied.
#' @param t_step The time step in milliseconds. Must match the one used in `fixations_to_timeseries`.
#' @param min_fraction Numeric value between `0` and `1` indicating the minimum fraction of the window that must contain valid data for the trial to be considered "high-data". For example, `min_fraction = 1/3` requires at least one-third of the window to have valid data. **This parameter must be specified.**
#'
#' @details
#' **Usage Requirements:**
#' - **Either** `window_column` **or both** `t_start` **and** `t_end` **must be supplied.**
#'   - If `window_column` is provided, the function uses it to identify time bins within the window of interest.
#'   - If `t_start` and `t_end` are provided:
#'     - The function checks if a column named `window_{t_start}_{t_end}ms` exists in `fixation_timeseries`.
#'       - If it exists, the function stops and suggests using `window_column = "window_{t_start}_{t_end}ms"` instead.
#'       - If it does not exist, the function calls `assign_time_windows()` to create the required window column and proceeds.
#'     - The temporary `which_window_{t_start}_{t_end}ms` column created by `assign_time_windows()` is dropped after use.
#'
#' The function calculates the minimum number of time points with valid data required for a trial to be considered "high-data", based on the `min_fraction` and the duration of the time window (`t_end - t_start`). It then tags each trial by adding a new column `is_trial_low_data`, which is `TRUE` for low-data trials and `FALSE` otherwise.
#'
#' @return The input dataframe with an additional logical column `is_trial_low_data`, indicating whether each trial is considered low-data (`TRUE`) or not (`FALSE`).
#'
#' @export
tag_low_data_trials <- function(
    fixation_timeseries,
    window_column = NULL,
    t_start = NULL,
    t_end = NULL,
    t_step = 20,
    min_fraction) {

  original_columns <- colnames(fixation_timeseries)

  # Ensure that either window_column is supplied, or both t_start and t_end are supplied
  if (is.null(window_column)) {
    if (is.null(t_start) || is.null(t_end)) {
      stop("Either window_column or both t_start and t_end must be supplied.")
    }
    # Create window_column name
    window_column <- glue::glue('window_{t_start}_{t_end}ms')

    # Check if window_column exists in the input dataframe
    if (window_column %in% colnames(fixation_timeseries)) {
      stop(glue::glue("{window_column} is already present in the data. Please use that column instead of supplying t_start and t_end."))
    } else {
      # Apply assign_time_windows to create the window_column
      fixation_timeseries <- assign_time_windows(
        fixation_timeseries,
        t_step = t_step,
        t_starts = t_start,
        t_ends = t_end
      )
      # Remove which_window_ column(s)
      which_window_column <- glue::glue('which_window_{t_start}_{t_end}ms')
      fixation_timeseries <- fixation_timeseries %>%
        dplyr::select(-dplyr::all_of(which_window_column))
    }
  } else {
    if (!is.null(t_start) || !is.null(t_end)) {
      stop("Specify either window_column or both t_start and t_end, not both.")
    }

    # Check that window_column is present in the data
    assertthat::assert_that(
      window_column %in% colnames(fixation_timeseries),
      msg = glue("{window_column} not found in fixation_timeseries")
    )

    # Check that window_column is in the correct format
    window_pattern <- '^window_(\\d+)_(-?\\d+)ms$'
    assertthat::assert_that(
      stringr::str_detect(window_column, window_pattern),
      msg = "window_column does not match the required format 'window_N_Mms'"
    )

    # Extract start and end from window_column
    matches <- stringr::str_match(window_column, window_pattern)
    t_start <- as.integer(matches[, 2])
    t_end <- as.integer(matches[, 3])
  }

  # Check that min_fraction is set
  if (missing(min_fraction)) {
    stop("min_fraction must be set. The often-used value is 1/3.")
  }

  min_points_with_data <- floor((t_end - t_start) / t_step * min_fraction)

  # Check required columns
  required_columns <- c("recording_id", "trial_index", "is_good_timepoint", window_column)
  assertthat::assert_that(
    all(required_columns %in% colnames(fixation_timeseries)),
    msg = glue("fixation_timeseries must contain columns: {paste(required_columns, collapse = ', ')}")
  )

  tagged <- fixation_timeseries %>%
    assertr::verify(!!rlang::sym(window_column) %in% c('Y', 'N')) %>%
    dplyr::mutate(
      in_time_window = !!rlang::sym(window_column) == "Y",
      has_data = .data$in_time_window & .data$is_good_timepoint
    ) %>%
    dplyr::group_by(.data$recording_id, .data$trial_index) %>%
    dplyr::mutate(
      bins_with_data_count = sum(.data$has_data),
      is_trial_low_data = .data$bins_with_data_count < .env$min_points_with_data
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(original_columns), .data$is_trial_low_data)

  return(tagged)
}


get_pairs <- function(data, study = "eye_tracking", output_dir = '../data/', out_csv = FALSE){
  res <- data %>%
    dplyr::group_by(SubjectNumber) %>%
    dplyr::distinct(Pair)
  if (out_csv){
    name <- paste(output_dir, "pairs_", study, "_", stringr::str_replace_all(Sys.time(), ' ', '_'), ".csv", sep='')
    print(name)
    readr::write_csv(res, name)
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
    dplyr::group_by(SubjectNumber, Trial) %>%
    dplyr::mutate(
      frozen = ifelse(
        length(levels(forcats::fct_explicit_na(gaze, na_level = "NA"))) == 1,
        T,
        F))

 message("Column added identifying trials where gaze stayed in one interest area for whole trial (frozen = T).")
 return(gazeData)
}
