#' Find PN-OPUS
#'
#' @return Path to the PN-OPUS shared drive
#' @export
#'
#' @examples
#' pn_opus_path <- get_pn_opus_path()
get_pn_opus_path <- function(check_exists = TRUE) {
  pn_opus_path <- Sys.getenv('PN_OPUS_PATH')
  if (is.null(pn_opus_path)) {
    pn_opus_path <- '/Volumes/pn-opus'
  }
  if (check_exists & !dir.exists(pn_opus_path)) {
    stop('VPN connected? Location other than /Volumes/pn-opus?')
  }
}
