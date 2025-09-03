#' Find BLab share
#'
#' @param check_exists Should the function check that the path exists
#' (accessible)
#'
#' @return Path to the BLab share
#' @export
#'
#' @examples
#' \dontrun{
#' blab_share <- get_blab_share_path()
#' }
get_blab_share_path <- function(check_exists = TRUE) {
  blab_share_path <- Sys.getenv('BLAB_SHARE_PATH')
  if (is.null(blab_share_path) | blab_share_path == "" ) {
    blab_share_path <- '/Volumes/Fas-Phyc-PEB-Lab'
  }
  if (check_exists & !dir.exists(blab_share_path)) {
    stop(glue::glue('VPN connected? Location other than {blab_share_path}?'))
  }
  return(blab_share_path)
}
