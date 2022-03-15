#' Calculate stats similar to those in LENA's 5min.csv files
#'
#' @param its_xml xml object created by rlena::read_its_file
#' @param period interval duration supported by lubridate::period, e.g.,
#' '2 mins'
#'
#' @return a tibble with at least these four columns: duration, AWC.Actual,
#' CTC.Actual, CWC.Actual
#' @export
calculate_lena_like_stats <- function(its_xml, period) {
  rlena::gather_segments(its_xml) %>%
    dplyr::mutate(five_min_time = lubridate::floor_date(startClockTimeLocal, period)) %>%
    dplyr::group_by(five_min_time) %>%
    dplyr::summarise(
      duration = diff(range(startClockTimeLocal)),
      # If there were not conversational turns, use NA, not -Inf
      cumulative_ctc = ifelse(
        !all(is.na(convTurnCount)),
        max(convTurnCount, na.rm=T),
        NA_integer_),
      CVC.Actual = round(sum(childUttCnt, na.rm = T)),
      FWC = sum(maleAdultWordCnt, na.rm = T),
      MWC = sum(femaleAdultWordCnt, na.rm = T),
      AWC.Actual = round(FWC + MWC)) %>%
    # For segments that don't have any conversational turns, use the previous value
    tidyr::fill(cumulative_ctc, .direction = 'down') %>%
    dplyr::mutate(cumulative_ctc = tidyr::replace_na(cumulative_ctc, 0)) %>%
    dplyr::mutate(CTC.Actual = cumulative_ctc - dplyr::lag(cumulative_ctc, default = 0)) %>%
    dplyr::select(five_min_time, CVC.Actual, CTC.Actual, AWC.Actual, duration)
}


#' Approxmiate LENA's 5min.csv output
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
