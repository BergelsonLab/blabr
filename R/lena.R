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
    # level starts and ends
    dplyr::inner_join(
      rlena::gather_recordings(its_xml) %>%
        dplyr::select(recId,
               recStartClockTimeLocal = startClockTimeLocal,
               recEndClockTimeLocal = endClockTimeLocal),
      by = 'recId') %>%
    dplyr::mutate(
      interval_start = pmax(lubridate::floor_date(startClockTimeLocal, period),
                            recStartClockTimeLocal),
      interval_end = pmin(lubridate::ceiling_date(startClockTimeLocal, period),
                          recEndClockTimeLocal)) %>%
    dplyr::group_by(interval_start, interval_end) %>%
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
    dplyr::select(interval_start, interval_end, CVC.Actual, CTC.Actual, AWC.Actual)
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
#'
#' @examples
get_speaker_stats <- function(its_xml, intervals) {
  intervals %>%
    select(interval_start, interval_end) %>%
    fuzzyjoin::interval_left_join(
      rlena::gather_segments(its_xml) %>%
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
               segment_duration),
      by = c("interval_start" = "startClockTimeLocal",
             "interval_end" = "startClockTimeLocal")) %>%
    group_by(interval_start, interval_end, spkr) %>%
    summarise(across(c(adult_word_count, utterance_count, segment_duration),
                     sum),
              .groups = 'drop')
}

