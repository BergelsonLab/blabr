SNAPSHOT_DIR_NAME <- ".environment_snapshot"
SESSION_INFO_TXT <- 'session-info.txt'

#' Capture environment information
#'
#' Captures two types of information:
#' - Versions of packages used in scripts and documents in a folder.
#' - Session info - platform and external software information about an R
#'   session. The session info will we be captured for the session from which the
#'   function is called and for each RMarkdown document discovered. For the
#'   latter, the document will be rendered in a separate session and the
#'  information will be
#'   captured at the moment when all the code has already been run.
#'
#' Run this function in the root of your project before any collaborator
#' (whether internal or external) will work on the project.
#'
#' # RMarkdown
#'
#' Don't use this function in RMarkdown document because it will discover the
#' document from which is called and render it resulting in an infinite loop.
#'
#'
#' @param root_dir Root folder - the folder in which files will be serached for.
#'   If `NULL` (default), an RStudio project will be searched for.
#' @param output_dir The folder to which the output files will be written. If
#'   `NULL` (default), a folder named "`r SNAPSHOT_DIR_NAME`" fill be crated in
#'   the root folder.
#'
#'
#' @return
#' @export
#'
#' @examples
#'
capture_environment <- function(root_dir = NULL, snapshot_dir = NULL) {
  # Deal with paths
  root_dir <- root_dir %??% rstudioapi::getActiveProject()
  snapshot_dir <- snapshot_dir %??% file.path(root_dir, SNAPSHOT_DIR_NAME)
  dir_create(snapshot_dir, exist_ok = TRUE)

  # Save package versions and the session info
  capture_package_versions(root_dir = root_dir, snapshot_dir = snapshot_dir)
  project_session_info <- capture_session_info(to_file =
                                                 file.path(snapshot_dir,
                                                           SESSION_INFO_TXT))

  # Save when-knitted session info of Rmd documents in root_dir
  rmd_files <- find_rmd_files(root_dir = root_dir)
  rmd_session_info_paths <- shorten_file_paths(rmd_files) %>%
    stringr::str_replace(.Platform$file.sep, '===') %>%
    stringr::str_replace(rmd_extension, glue::glue('_{SESSION_INFO_TXT}')) %>%
    purrr::map_chr(~ file.path(snapshot_dir, .x))
  no_return <- purrr::map2(
    rmd_files, rmd_session_info_paths,
    ~ capture_rmd_sessioninfo(rmd_file = .x, to_file = .y))
}

#' Capture RMarkdown session info
#'
#' Renders an RMarkdown in a separate R session and captures platform and
#' external information
capture_rmd_sessioninfo <- function(rmd_file, to_file = NULL) {
  # Run `render` and `capture_sessioninfo` in a new session.
  info <- callr::r(
    function(rmd_file, capture_session_info, to_file) {
      # Render
      rmarkdown::render(rmd_file, output_format = NULL, run_pandoc = FALSE,
                        quiet = TRUE)
      capture_session_info(to_file = to_file)},
    args = list(rmd_file, capture_session_info, to_file))
  return(info)
}

remove_ignored <- function(root_dir, file_paths) {
  blabr_env_ignore_name <- '.blabr-env-ignore'
  blabr_env_ignore_path <- file.path(root_dir, blabr_env_ignore_name)
  if (!file.exists(blabr_env_ignore_path)) {
    return(file_paths)
  }
  relative_ignored_paths <- readLines(blabr_env_ignore_path)
  normalized_ignored_paths <-
    purrr::map_chr(
      relative_ignored_paths,
      function(relative_path)
        file.path(root_dir, relative_path) %>%
        normalizePath %>%
        stringr::str_to_upper())
  is_ignored <-
    purrr::map_lgl(
      file_paths,
      function(file_path) {
        stringr::str_to_upper(normalizePath(file_path)) %in%
          normalized_ignored_paths})

  n_ignored <- sum(is_ignored)
  if (n_ignored > 0) {
    message(glue::glue('
      The following files were listed in the `{blabr_env_ignore_name}` \\
      ignore file and were ignored:
      {glue::glue_collapse(file_paths[is_ignored])}

      The ignore file was found at the following path:
      {blabr_env_ignore_path}
      '))}
  return(file_paths[!is_ignored])
}


find_rmd_files <- function(root_dir) {
  rmd_extension <- "\\.Rmd$"
  rmd_files <- list.files(root_dir,
                          pattern = rmd_extension,
                          recursive = TRUE,
                          ignore.case = TRUE,
                          full.names = TRUE)
  remove_ignored(root_dir, rmd_files)
}

#' Capture current session info
#'
#' Uses `sessioninfo::session_info` to capture platform information and
#' information about external software and optionally save output to a file.
#'
#' @param to_file Set to TRUE or a filename to save information. See
#' `?sessioninfo::session_info` for details.
#'
#' @return A session_info object, see `?sessioninfo::session_info` for details.
capture_session_info <- function(to_file = FALSE) {
  info <- sessioninfo::session_info(
    info = c('platform', 'external'),
    to_file = to_file,
    # These only relate to the package info which we aren't requesting. But if
    # we decide to request it later, we should start with these defaults.
    pkgs = 'installed',
    include_base = FALSE,
    dependencies = TRUE)
  return(info)
}


#' Capture package versions
#'
#' Captures version of packages used in the project and all their dependencies
#' using `renv`, and then saves the output to a `renv.lock` file in the
#' specified output folder.
#'
#' @inheritParams capture_environment
#'
#' @export
capture_package_versions <- function(root_dir = NULL, snapshot_dir = NULL) {
  # TODO: Factor both of these out to DRY
  # Deal with paths
  root_dir <- root_dir %??% rstudioapi::getActiveProject()
  snapshot_dir <- snapshot_dir %??% file.path(root_dir, SNAPSHOT_DIR_NAME)

  # Take a snapshot suppressing the output of renv::snapshot
  snapshot_path <- file.path(snapshot_dir, "renv.lock")
  log <- capture.output(
    renv::snapshot(project = root_dir,
      type = 'implicit',
      lockfile = snapshot_path,
      # Otherwise complains if renv project isn't initiated.
      prompt = FALSE))
  stopifnot(file.exists(snapshot_path))
}
