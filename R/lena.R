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
    # intervals within the
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

#' Calculate per-speaker
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
get_speaker_stats <- function(its_xml, intervals) {
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

#' Prepare intervals for sampling
#'
#' - remove incomplete intervals,
#' - check that there are still some left after that,
#' - check that there are at least `size` intervals, unless `allow_fewer` is
#'   `TRUE`
#'
#' @inheritParams get_speaker_stats
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
