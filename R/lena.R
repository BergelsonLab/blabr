#' Prepare intervals to potentially be annotated later
#'
#' Various metrics can then be calculated for each intervals and the "best"
#' can then be annotated.
#'
#' @param its_xml XML object created by `rlena::read_its_file`.
#' @param duration Interval duration supported by `lubridate::period`, e.g.,
#' '2 mins'.
#'
#' @return A tibble with
prepare_intervals <- function(its_xml, duration) {
  # For each recording, we'll find when the first duration-long interval would
  # have started and the last interval would have ended if they were full.
  # For example, if duration is '5 mins' and one of the recordings was on
  # from 9:22 to 9:37, these "pseudo" starts and ends would be 9:20 and 9:40.
  # We'll then create a sequence of individual interval starts and ends, i.e.,
  # 9:20, 9:25, 9:30, 9:35 (starts) and 9:25, 9:30, 9:35, 9:40 (ends).
  # Finally we'll trim the first and last interval to when the recording was on,
  # i.e., making the following sequence of intervals: 9:22-9:25, 9:25-9:30,
  # 9:30-9:35, 9:35-9:37.
  recordings <- rlena::gather_recordings(its_xml)
  intervals <- recordings %>%
    # Create interval starts and ends. Except for the first and the last ones
    # in each recording, the starts and ends should already be correct.
    dplyr::mutate(
      first_interval_pseudo_start
      = lubridate::floor_date(startClockTimeLocal, duration),
      last_interval_pseudo_end
      = lubridate::ceiling_date(endClockTimeLocal, duration),
      n_intervals = (last_interval_pseudo_end - first_interval_pseudo_start)
                    / lubridate::duration(duration),
      interval_offsets
      = purrr::map(n_intervals, ~ lubridate::period(duration) * seq(0, .x - 1)),
      interval_pseudo_start
      = purrr::map2(first_interval_pseudo_start, interval_offsets, ~ .x + .y),
      interval_pseudo_end
      = purrr::map2(last_interval_pseudo_end, interval_offsets,
                    ~ .x - rev(.y))) %>%
    # We don't need most of the columns
    dplyr::select(recId, startClockTimeLocal, endClockTimeLocal,
                  interval_pseudo_start, interval_pseudo_end, startTime) %>%
    dplyr::rename(recording_id = recId,
                  recording_start = startClockTimeLocal,
                  recording_end = endClockTimeLocal,
                  recording_start_wav = startTime) %>%
    # interval_start and interval_end are lists at this point, let's unnest them
    # into rows
    tidyr::unnest_longer(col = c(interval_pseudo_start,
                                 interval_pseudo_end)) %>%
    # Finally, let's
    # (1) trim them to when the recording was on
    dplyr::mutate(
      interval_start = pmax(interval_pseudo_start, recording_start),
      interval_end = pmin(interval_pseudo_end, recording_end)) %>%
    # (2) add within-wav start time
    dplyr::mutate(
      # When does interval start within the recording, in milliseconds
      interval_start_rec = lubridate::interval(recording_start,
                                               interval_start)
                           / lubridate::milliseconds(1),
      # And within the wav file
      interval_start_wav = recording_start_wav * 1000 + interval_start_rec) %>%
    # ... and remove unnecessary columns. We will still need
    # `interval_pseudo_start` and `interval_pseudo_end` to identify intervals
    # from different recordings that are part of the same pseudo-interval. E.g.,
    # if there are two recordings: 9:00-9:31 and 9:33-9:46, then we'll have
    # the 9:30-9:31 from recording 1 and the 9:33-9:35 interval from recording
    # 2 which both belong to the 9:30-9:35 interval. For now, however, we will
    # keep them separately to know that it is not actually a full interval and
    # account for that.
    dplyr::select(interval_start, interval_end, interval_start_wav,
                  recording_id)

  return(intervals)
}


#' Calculate stats similar to those in LENA's 5min.csv files
#'
#' @inheritParams prepare_intervals
#'
#' @return a tibble with at least these four columns: interval_start,
#' interval_end, AWC.Actual, CTC.Actual, CWC.Actual
#' @export
calculate_lena_like_stats <- function(its_xml, duration) {
  intervals <- prepare_intervals(its_xml, duration)
  add_lena_stats(its_xml, intervals)
}


#' Add LENA stats to each interval in a dataframe
#'
#' @inheritParams prepare_intervals
#' @param time_type Either `wall` or `wav`. If `wall`, `start` and `end` must
#' contain timestamps with local time. If `wav`, `start` and `end` must contain
#' the number of milliseconds since the beginning of the wav file. `wall` is not
#' yet implemented.
#' @param intervals A tibble with columns `interval_start`, `interval_end`,
#' `interval_start_wav`, and `recording_id` as output by `prepare_intervals`.
#' Can contain other columns.
#'
#' @note Any its segment overlapping with multiple intervals will count fully
#' towards all of them.
#'
#' @note The difference between this function and `calculate_lena_like_stats`
#' is that the latter calculates exactly the same stats but does it for every
#' five minute period of wall time (e.g., 13:00-13:05). It should be rewritten
#' to first create these intervals and then use `add_lena_stats` to avoid code
#' repetition (see issue #).
#'
#' @return Same as `intervals` but with three new columns: `cvc`, `ctc`, and
#' `awc`.
#' @export
add_lena_stats <- function(its_xml, intervals, time_type = c('wav', 'wall')) {
  time_type <- match.arg(time_type)
  if (time_type == 'wall') {
    stop('Using wall time has not been implemented.')
  }

  segments <- rlena::gather_segments(its_xml) %>%
    dplyr::select(endTime, startTime, convTurnCount, childUttCnt, maleAdultWordCnt,
           femaleAdultWordCnt) %>%
    # convert time to milliseconds
    dplyr::mutate(dplyr::across(c(startTime, endTime), ~ as.integer(.x * 1000)))

  intervals <- intervals %>% add_interval_end_wav

  interval_stats <- intervals %>%
    dplyr::cross_join(segments) %>%
    dplyr::filter(interval_start_wav < endTime
                  & startTime < interval_end_wav) %>%
    # Adjust stats for segments with only partial overlaps
    dplyr::mutate(
      tmp.overlap_duration = pmin(endTime, interval_end_wav)
                         - pmax(startTime, interval_start_wav),
      tmp.segment_duration = endTime - startTime,
      dplyr::across(
        c(childUttCnt, maleAdultWordCnt, femaleAdultWordCnt),
        ~ .x * (tmp.overlap_duration / tmp.segment_duration))
    ) %>%
    dplyr::select(-starts_with('tmp.')) %>%
    # Aggregate over intervals
    dplyr::group_by(interval_start_wav, interval_end_wav) %>%
    dplyr::summarise(
      # If there were no conversational turns, use NA, not -Inf
      cumulative_ctc = ifelse(
        !all(is.na(convTurnCount)),
        max(convTurnCount, na.rm=T),
        NA_integer_),
      cvc = round(sum(childUttCnt, na.rm = T)),
      fwc = sum(maleAdultWordCnt, na.rm = T),
      mwc = sum(femaleAdultWordCnt, na.rm = T),
      awc = round(fwc + mwc),
      .groups = 'drop') %>%
    # For segments that don't have any conversational turns, use the previous value
    tidyr::fill(cumulative_ctc, .direction = 'down') %>%
    dplyr::mutate(cumulative_ctc = tidyr::replace_na(cumulative_ctc, 0)) %>%
    dplyr::mutate(ctc = cumulative_ctc
                        - dplyr::lag(cumulative_ctc, default = 0)) %>%
    # Remove auxiliary columns
    dplyr::select(-c(cumulative_ctc, fwc, mwc))

  # Match intervals to stats, substituting NAs for zero for intervals without
  # segments
  new_columns <- setdiff(names(interval_stats), names(intervals))
  intervals_with_stats <- intervals %>%
    dplyr::left_join(interval_stats,
                     by = c('interval_start_wav', 'interval_end_wav')) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(new_columns),
                                ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::select(-interval_end_wav)

  return(intervals_with_stats)
}


#' Approximate LENA's 5min.csv output
#'
#' @inherit calculate_lena_like_stats
#' @export
#'
#' @examples
#' # Download the example ITS file (code from rlena's README)
#' url <- paste0("https://cdn.rawgit.com/HomeBankCode/lena-its-tools/",
#'               "master/Example/e20160420_165405_010572.its")
#' tmp <- tempfile()
#' download.file(url, tmp)
#' its <- rlena::read_its_file(tmp)
#' make_five_min_approximation(its_xml = its)
make_five_min_approximation <- function(its_xml) {
  calculate_lena_like_stats(its_xml, '5 mins')
}

#' Calculate per-speaker statistics based on the .its file
#'
#' @inheritParams add_lena_stats
#'
#' @return a tibble with the following columns:
#' - interval_start, interval_end: same as in the intervals input tibble,
#' - adult_word_count: non-zero for MAN and FAN only
#' - utterance_count: for CH* - the sum of childUttCnt, for everyone else - the
#'   number of conversation segments
#' @export
get_lena_speaker_stats <- function(its_xml, intervals) {
  # Extract several segment stats (a single utterance or a CHN/CHF
  # multi-utterance)
  segment_stats <- rlena::gather_segments(its_xml) %>%
    filter(blkType == 'Conversation') %>%
    mutate(
      adult_word_count = case_when(
        stringr::str_starts(spkr, 'FA') ~ femaleAdultWordCnt,
        stringr::str_starts(spkr, 'MA') ~ maleAdultWordCnt,
        TRUE ~ 0),
      # Only CHN and CHF get to have multiple utterances per segment
      utterance_count = case_when(
        stringr::str_starts(spkr, 'CH') ~ childUttCnt,
        TRUE ~ 1
      ),
      segment_duration = endTime - startTime) %>%
    dplyr::select(startClockTimeLocal, spkr, adult_word_count, utterance_count,
           segment_duration)

  # Match intervals to segments and summarize the stats from above
  intervals <- intervals %>% dplyr::select(interval_start, interval_end)
  intervals %>%
    # start: conditional left join: startClockTimeLocal within interval
    dplyr::cross_join(segment_stats) %>%
    dplyr::filter(interval_start <= startClockTimeLocal
                  & startClockTimeLocal < interval_end) %>%
    dplyr::right_join(intervals, by = c('interval_start', 'interval_end')) %>%
    # end: conditional left join
    group_by(interval_start, interval_end, spkr) %>%
    summarise(across(c(adult_word_count, utterance_count, segment_duration),
                     sum),
              .groups = 'drop')
}



#' Adds wav-anchored interval boundaries
#'
#' @inheritParams add_lena_stats
#' @param unit The time unit of the added boundaries. Either 'ms' or 's' for
#' milliseconds or seconds respectively. The add column(s)' names will reflect
#' the choice of this parameter.
#'
#' @return If `unit` is 'ms' then `intervals` with one additional column -
#'  `interval_end_wav`. If unit is 's' - two additional columns:
#'  `interval_start_wav_s` and `interval_end_wav_s`.
#'
#' @noRd
add_interval_end_wav <- function(intervals, unit = c('ms', 's')) {
  unit <- match.arg(unit)
  intervals <- intervals %>%
    dplyr::mutate(duration = lubridate::interval(interval_start, interval_end)
                  / lubridate::milliseconds(1),
                  interval_end_wav = interval_start_wav + duration) %>%
    dplyr::select(-duration)

  if (unit == 's') {
    intervals <- intervals %>%
      mutate(across(c(interval_start_wav, interval_end_wav),
                    ~ .x / 1000,
                    .names = '{.col}_s')) %>%
      select(-interval_end_wav)
  }

  return(intervals)
}

#' Match intervals to VTC annotations
#'
#' Does a brute-force interval join to match intervals to all annotations that
#' overlap with them.
#'
#' @inheritParams add_lena_stats
#' @inheritParams add_vtc_stats
#'
#' @keywords internal
match_intervals_to_vtc_annotations <- function(intervals, all_rttm) {
  # Add wav-anchored interval boundaries in seconds (VTC uses seconds, we could
  # have converted it to milliseconds instead).
  intervals <- add_interval_end_wav(intervals, unit = 's')

  all_rttm <- all_rttm %>%
    dplyr::mutate(offset = onset + duration)

  intervals %>%
    # cross-join
    dplyr::cross_join(all_rttm) %>%
    # keep only rows
    dplyr::filter(
      onset < interval_end_wav_s
      & interval_start_wav_s < offset
    ) %>%
    dplyr::right_join(intervals, by = colnames(intervals)) %>%
    mutate(overlap_duration =
             pmin(interval_end_wav_s, offset)
             - pmax(interval_start_wav_s, onset)) %>%
    select(-c(interval_start_wav_s, interval_end_wav_s))
}

#' Add ctc calculated based on the vtc annotations
#'
#' @inheritParams add_lena_stats
#' @param all_rttm An `all.rttm` file from the VTC output loaded with
#' `read_rttm`.
#'
#' @return Same as `intervals` but with additional `vtc_ctc` column.
#'
#' @note Ported from childrpoject's high-volubility sampler code.
#'
#' @export
add_vtc_stats <- function(intervals, all_rttm) {
  # Conversational turn count: adult and child speaking withing 1 s from each
  # other
  child_voice_type <- 'KCHI'
  other_voice_types <- c('FEM', 'MAL')
  all_voice_types <- c(child_voice_type, other_voice_types)
  distance <- 1  # second

  all_rttm_is_ct <- all_rttm %>%
    filter(voice_type %in% all_voice_types) %>%
    # The order of voice types is arbitrary, the point is just to have a
    # reproducible result whatever the order was before that.
    arrange(onset, match(voice_type, all_voice_types)) %>%
    mutate(
      offset = onset + duration,
      iti = onset - lag(offset),
      prev_voice_type = lag(voice_type),
      chi_and_other = ((voice_type == child_voice_type
                        & prev_voice_type %in% other_voice_types)
                       | (voice_type %in% other_voice_types
                          & prev_voice_type == child_voice_type)),
      is_ct = (iti < distance) & chi_and_other) %>%
    select(onset, duration, is_ct)

  match_intervals_to_vtc_annotations(intervals = intervals,
                                     all_rttm = all_rttm_is_ct) %>%
    mutate(is_ct = tidyr::replace_na(is_ct, FALSE)) %>%
    group_by(across(colnames(intervals))) %>%
    summarize(vtc_ctc = sum(is_ct), .groups = 'drop')
}


#' Calculate per-speaker statistics based on the VTC output
#'
#' @inheritParams add_lena_stats
#' @inheritParams add_vtc_stats
#'
#' @return A tibble with `interval_start` and `interval_end` columns for each
#' interval in `intervals` and three new columns: `voice_type` and `duration`
#' and `count` of VTC annotations. The `duration` is in seconds.
#' @export
get_vtc_speaker_stats <- function(intervals, all_rttm) {
  match_intervals_to_vtc_annotations(intervals = intervals,
                                     all_rttm = all_rttm) %>%
    group_by(interval_start, interval_end, voice_type) %>%
    summarise(duration = sum(overlap_duration),
              count = n(),
              .groups = 'drop')
}


#' Calculates the number of Seedlings annotations in each interval (by speaker)
#'
#' @inheritParams add_lena_stats
#' @param annotations A tibble loaded from a csv annotations file from the
#'   Seedlings project or similar
#'
#' @return A tibble with `interval_start` and `interval_end` columns for each
#' interval in `intervals` and two new columns: `speaker` and `n_annotations`.
#'
#' @export
get_seedlings_speaker_stats <- function(intervals, annotations) {
  intervals <- intervals %>%
    add_interval_end_wav(unit = 's') %>%
    dplyr::select(interval_start, interval_end,
           interval_start_wav_s, interval_end_wav_s)
  annotations <- annotations %>%
    dplyr::select(speaker, onset, offset) %>%
    dplyr::mutate(dplyr::across(c(onset, offset), ~ .x / 1000))

  intervals %>%
    # start: conditional left join: intervals overlap
    dplyr::cross_join(annotations) %>%
    dplyr::filter(
      onset < interval_end_wav_s
      & interval_start_wav_s < offset
    ) %>%
    dplyr::right_join(intervals, by = colnames(intervals)) %>%
    dplyr::arrange(interval_start, interval_end, onset, offset) %>%
    # end: conditional left join
    group_by(interval_start, interval_end, speaker) %>%
    summarise(n_annotations = n(),
              .groups = 'drop') %>%
    mutate(n_annotations = if_else(is.na(speaker), 0L, n_annotations))
}


#' Prepare intervals for sampling
#'
#' - remove incomplete intervals,
#' - check that there are still some left after that,
#' - check that there are at least `size` intervals, unless `allow_fewer` is
#'   `TRUE`
#'
#' @inheritParams add_lena_stats
#' @inheritParams calculate_lena_like_stats
#' @param size required sample size
#' @param allow_fewer Is it OK if there are fewer than size intervals?
#'
#' @return intervals with incomplete intervals removed
#'
#' @keywords internal
prepare_intervals_for_sampling <- function(intervals, size, duration,
                                           allow_fewer) {
  intervals %>%
    # Keep only full intervals (the first and last interval of each recording
    # are usually shorter)
    dplyr::filter(interval_end - interval_start
                  == lubridate::period(duration)) %T>%
    # Check that there are at least some full intervals
    {assertthat::assert_that(nrow(.) > 0)} %T>%
    # Check that there are enough rows left
    {assertthat::assert_that(allow_fewer | (nrow(.) >= size))}
}

#' Sample intervals randomly
#'
#' @inheritParams prepare_intervals_for_sampling
#' @inheritParams add_lena_stats
#' @param seed Seed for the random number generator.
#'
#' @return A subsample of intervals.
#' @export
sample_intervals_randomly <- function(intervals, size, duration,
                                      allow_fewer = FALSE, seed = NULL) {
  if (!is.null(seed))
    {withr::local_seed(seed)}
  else
    {withr::local_preserve_seed()}

  intervals %>%
    select(interval_start, interval_end) %>%
    prepare_intervals_for_sampling(size = size, duration = duration,
                                   allow_fewer = allow_fewer) %>%
    dplyr::sample_n(min(size, nrow(.))) %>%
    arrange(interval_start)
}

#' Sample intervals that are highest on a given metric
#'
#' @inheritParams prepare_intervals_for_sampling
#' @param column Name of the column whose value is to be maximized.
#'
#' @inherit sample_intervals_randomly return
#' @export
sample_intervals_with_highest <- function(intervals, column, size, duration,
                                          allow_fewer = FALSE) {
  intervals %>%
    select(interval_start, interval_end, {{column}}) %>%
    prepare_intervals_for_sampling(size = size, duration = duration,
                                   allow_fewer = allow_fewer) %>%
    dplyr::arrange({{ column }}) %>%
    tail(min(size, nrow(.))) %>%
    dplyr::arrange(interval_start)
}

#' Sample intervals periodically, e.g. every hour
#'
#' @inheritParams prepare_intervals_for_sampling
#' @param period Period at the end of which we should sample intervals,
#' e.g., '1 hour'
#'
#' @inherit sample_intervals_randomly return
#' @export
sample_intervals_periodically <- function(intervals, duration, period){
  intervals %>%
    select(interval_start, interval_end) %>%
    prepare_intervals_for_sampling(size = 0, duration = duration,
                                   allow_fewer = TRUE) %>%
    dplyr::filter(lubridate::ceiling_date(interval_end, unit = period)
                  == interval_end) %>%
    dplyr::arrange(interval_start)
}
