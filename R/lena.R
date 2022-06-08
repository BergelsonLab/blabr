#' Calculate stats similar to those in LENA's 5min.csv files
#'
#' @param its_xml xml object created by rlena::read_its_file
#' @param period interval duration supported by lubridate::period, e.g.,
#' '2 mins'
#'
#' @return a tibble with at least these four columns: interval_start,
#' interval_end, AWC.Actual, CTC.Actual, CWC.Actual
#' @export
calculate_lena_like_stats <- function(its_xml, period) {
  rlena::gather_segments(its_xml) %>%
    # To keep the intervals to when the recording was on, we'll need recording-
    # level starts and ends.
    # Also, all recordings are in a single wav file, so to be able to find
    # intervals within the wav file, we'll also add the interval start as time
    # since the wav start.
    dplyr::inner_join(
      rlena::gather_recordings(its_xml) %>%
        dplyr::select(recId,
               recStartClockTimeLocal = startClockTimeLocal,
               recEndClockTimeLocal = endClockTimeLocal,
               recStartTimeWav = startTime),
      by = 'recId') %>%
    dplyr::mutate(
      interval_start = pmax(lubridate::floor_date(startClockTimeLocal, period),
                            recStartClockTimeLocal),
      interval_end = pmin(lubridate::ceiling_date(startClockTimeLocal, period),
                          recEndClockTimeLocal),
      # When does interval start within the recording, in seconds
      interval_start_rec = lubridate::interval(recStartClockTimeLocal,
                                               interval_start)
                           / lubridate::milliseconds(1),
      # And within the wav file
      interval_start_wav = recStartTimeWav * 1000 + interval_start_rec) %>%
    dplyr::select(-interval_start_rec) %>%
    dplyr::group_by(interval_start, interval_end, interval_start_wav) %>%
    dplyr::summarise(
      # If there were not conversational turns, use NA, not -Inf
      cumulative_ctc = ifelse(
        !all(is.na(convTurnCount)),
        max(convTurnCount, na.rm=T),
        NA_integer_),
      CVC.Actual = round(sum(childUttCnt, na.rm = T)),
      FWC = sum(maleAdultWordCnt, na.rm = T),
      MWC = sum(femaleAdultWordCnt, na.rm = T),
      AWC.Actual = round(FWC + MWC)) %>%
    dplyr::ungroup() %>%
    # For segments that don't have any conversational turns, use the previous value
    tidyr::fill(cumulative_ctc, .direction = 'down') %>%
    dplyr::mutate(cumulative_ctc = tidyr::replace_na(cumulative_ctc, 0)) %>%
    dplyr::mutate(CTC.Actual = cumulative_ctc - dplyr::lag(cumulative_ctc, default = 0)) %>%
    dplyr::select(interval_start, interval_end, interval_start_wav, CVC.Actual, CTC.Actual, AWC.Actual)
}


#' Add LENA stats to each interval in a dataframe
#'
#' @inherit add_lena_stats
#' @param intervals Tibble that contain columns `start` and `end`
#' @param time_type Either `wall` or `wav`. If `wall`, `start` and `end` must
#' contain timestamps with local time. If `wav`, `start` and `end` must contain
#' the number of milliseconds since the beginning of the wav file. `wall` is not
#' yet implemented.
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
add_lena_stats <- function(its_xml, intervals, time_type) {
  assertthat::assert_that(time_type %in% c('wav', 'wall'))
  if (time_type == 'wall') {
    stop('Using wall time has not been implemented.')
  }

  segments <- rlena::gather_segments(its_xml) %>%
    select(endTime, startTime, convTurnCount, childUttCnt, maleAdultWordCnt,
           femaleAdultWordCnt) %>%
    mutate(across(c(startTime, endTime), ~ as.integer(endTime * 1000)))
  interval_stats <- intervals %>%
    dplyr::inner_join(segments, by = character()) %>%
    filter(start < endTime & startTime < end) %>%
    dplyr::group_by(start, end) %>%
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
    dplyr::select(start, end, cvc, ctc, awc)

  # Match intervals to stats, substituting NAs for zero for intervals without
  # segments
  intervals %>%
    dplyr::left_join(interval_stats, by = c('start', 'end')) %>%
    dplyr::mutate(dplyr::across(c(-start, -end), ~ tidyr::replace_na(.x, 0)))
}


#' Approximate LENA's 5min.csv output
#'
#' @inherit calculate_lena_like_stats
#' @export
#'
#' @examples
#' # Download the example ITS file (code from rlena's README)
#' url <- "https://cdn.rawgit.com/HomeBankCode/lena-its-tools/master/Example/e20160420_165405_010572.its"
#' tmp <- tempfile()
#' download.file(url, tmp)
#' its <- rlena::read_its_file(tmp)
#' make_five_min_approximation(its_xml = its)
make_five_min_approximation <- function(its_xml) {
  calculate_lena_like_stats(its_xml, '5 mins')
}

#' Calculate per-speaker statistics based on the .its file
#'
#' @inheritParams calculate_lena_like_stats
#' @param intervals a tibble with interval_start and interval_end datetime
#' columns
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
    select(startClockTimeLocal, spkr, adult_word_count, utterance_count,
           segment_duration)

  # Match intervals to segments and summarize the stats from above
  intervals <- intervals %>% select(interval_start, interval_end)
  intervals %>%
    # start: conditional left join: startClockTimeLocal within interval
    dplyr::inner_join(segment_stats, by = character()) %>%
    dplyr::filter(interval_start <= startClockTimeLocal
                  & startClockTimeLocal < interval_end) %>%
    dplyr::right_join(intervals, by = c('interval_start', 'interval_end')) %>%
    # end: conditional left join
    group_by(interval_start, interval_end, spkr) %>%
    summarise(across(c(adult_word_count, utterance_count, segment_duration),
                     sum),
              .groups = 'drop')
}



#' Adds wav-anchored interval boundaries in addition to local-time ones
#'
#' @inheritParams calculate_lena_like_stats
#'
#' @return `intervals` with two additional columns
#' @noRd
add_wav_anchored_interval_boundaries <- function(intervals) {
  intervals %>%
    dplyr::mutate(duration_s = lubridate::interval(interval_start, interval_end)
                  / lubridate::seconds(1),
                  interval_start_wav_s = interval_start_wav / 1000,
                  interval_end_wav_s = interval_start_wav_s + duration_s) %>%
    dplyr::select(-duration_s)
}


#' Calculate per-speaker statistics based on the VTC output
#'
#' @inheritParams get_lena_speaker_stats
#' @param all_rttm An `all.rttm` file from the VTC output loaded with
#'   `read_rttm`.
#' @return A tibble with `interval_start` and `interval_end` columns for each
#' interval in `intervals` and three new columns: `voice_type` and `duration`
#' and `count` of VTC annotations.
#' @export
get_vtc_speaker_stats <- function(all_rttm, intervals) {
  intervals <- intervals %>%
    add_wav_anchored_interval_boundaries %>%
    select(interval_start, interval_end,
           interval_start_wav_s, interval_end_wav_s)

  all_rttm <- all_rttm %>%
    # Keep the necessary columns only
    dplyr::select(onset, duration, voice_type) %>%
    dplyr::rename(interval_start_wav_rttm = onset) %>%
    dplyr::mutate(interval_end_wav_rttm = interval_start_wav_rttm + duration)

  intervals %>%
    # start: conditional left join: intervals overlap
    dplyr::inner_join(all_rttm, by = character()) %>%
    dplyr::filter(
      interval_start_wav_rttm < interval_end_wav_s
      & interval_start_wav_s < interval_end_wav_rttm
    ) %>%
    dplyr::right_join(intervals, by = colnames(intervals)) %>%
    # end: conditional left join
    # clip voice duration to the interval boundaries
    dplyr::mutate(
      duration = pmin(interval_end_wav_s, interval_end_wav_rttm)
      - pmax(interval_start_wav_s, interval_start_wav_rttm)
    ) %>%
    group_by(interval_start, interval_end, voice_type) %>%
    summarise(duration = sum(duration),
              count = n(),
              .groups = 'drop')
}


#' Calculates the number of Seedlings annotations in each interval (by speaker)
#'
#' @inheritParams get_lena_speaker_stats
#' @param annotations A tible loaded from a csv annotations file from the
#'   Seedlings project or similar
#'
#' @return A tibble with `interval_start` and `interval_end` columns for each
#' interval in `intervals` and two new columns: `speaker` and `n_annotations`.
#' @export
get_seedlings_speaker_stats <- function(intervals, annotations) {
  intervals <- intervals %>%
    add_wav_anchored_interval_boundaries %>%
    select(interval_start, interval_end,
           interval_start_wav_s, interval_end_wav_s)
  annotations <- annotations %>%
    select(speaker, onset, offset) %>%
    mutate(across(c(onset, offset), ~ .x / 1000))

  intervals %>%
    # start: conditional left join: intervals overlap
    dplyr::inner_join(annotations, by = character()) %>%
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
#' @inheritParams get_lena_speaker_stats
#' @param size required sample size
#' @param period What period (e.g., '5 mins') is the main interval size? Only
#' intervals of this duration will be sampled.
#' @param allow_fewer Is it OK if there are fewer than size intervals?
#'
#' @return intervals with incomplete intervals removed
#'
#' @keywords internal
prepare_intervals_for_sampling <- function(intervals, size, period,
                                           allow_fewer) {
  intervals %>%
    # Keep only full intervals (the first and last interval of each recording
    # are usually shorter)
    dplyr::filter(interval_end - interval_start == lubridate::period(period)) %T>%
    # Check that there are at least some full intervals
    {assertthat::assert_that(nrow(.) > 0)} %T>%
    # Check that there are enough rows left
    {assertthat::assert_that(allow_fewer | (nrow(.) >= size))}
}

#' Sample intervals randomly
#'
#' @inheritParams prepare_intervals_for_sampling
#' @param seed Seed for the random number generator.
#'
#' @return A subsample of intervals.
#' @export
sample_intervals_randomly <- function(intervals, size, period,
                                      allow_fewer = FALSE, seed = NULL) {
  if (!is.null(seed))
    {withr::local_seed(seed)}
  else
    {withr::local_preserve_seed()}

  intervals %>%
    select(interval_start, interval_end) %>%
    prepare_intervals_for_sampling(size = size, period = period,
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
sample_intervals_with_highest <- function(intervals, column, size, period,
                                          allow_fewer = FALSE) {
  intervals %>%
    select(interval_start, interval_end, {{column}}) %>%
    prepare_intervals_for_sampling(size = size, period = period,
                                   allow_fewer = allow_fewer) %>%
    dplyr::arrange({{ column }}) %>%
    tail(min(size, nrow(.))) %>%
    dplyr::arrange(interval_start)
}

#' Sample intervals periodically, e.g. every hour
#'
#' @inheritParams prepare_intervals_for_sampling
#' @param interval_period period used to create the intervals
#' @param sampling_period period at the end of which we should sample intervals,
#' e.g., '1 hour'
#'
#' @inherit sample_intervals_randomly return
#' @export
sample_intervals_periodically <- function(intervals, interval_period,
                                          sampling_period){
  intervals %>%
    select(interval_start, interval_end) %>%
    prepare_intervals_for_sampling(size = 0, period = interval_period,
                                   allow_fewer = TRUE) %>%
    dplyr::filter(lubridate::ceiling_date(interval_end,
                                          unit = sampling_period)
                  == interval_end) %>%
    dplyr::arrange(interval_start)
}
