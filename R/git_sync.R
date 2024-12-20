#' Absolute path to a dataset folder
#'
#' @param repo dataset name: 'all_basiclevel', 'reliability', etc.
#'
#' @return absolute path as a string
#'
#' @examples
#' \dontrun{
#' get_repo_path('all_basiclevel')
#' }
get_repo_path <- function(repo) {
  fs::path_home() / "BLAB_DATA" / repo
}


#' Run a git command in a dataset repository
#'
#' @inheritParams get_repo_path
#' @param command what you would have after `git` on the command line except
#' for the -C \<folder\> part - the function will do that part for you.
#' @param return_output boolean, whether to return the printed output of the
#' command as string
#'
#' @return string with output if return_output is TRUE, else NULL
#'
#' @examples
#' \dontrun{
#' run_git_command('all_basiclevel', 'status')
#' }
run_git_command <- function(repo, command, return_output = FALSE) {
  repo_root <- get_repo_path(repo)
  if (!dir.exists(repo_root)) {
    stop(glue::glue(
      'Expected to find the "{repo}" repository at the following location: {repo_root}. Please clone it.'))
  }

  process <- processx::process

  cmd_args <- strsplit(command, '\\s+')[[1]]
  git_bin <- Sys.which('git')
  process <- process$new(git_bin, c('-C', repo_root, cmd_args),
                         stdout = "|", stderr = "|")
  process$wait()

  exit_status <- process$get_exit_status()

  if (exit_status != 0) {
    error_message <- paste("Error executing git command:\n\n",
                           command,
                           "\n\nError message:\n\n",
                           process$read_error())
    stop(error_message)
  }

  if (return_output) {
    # Split the output into lines for consistency with the previous version that
    # used system2
    output_lines <- strsplit(process$read_output(), "\n")[[1]]
    return(output_lines)
  }
}



#' Fetches dataset tags from GitHub
#'
#' @inheritParams get_repo_path
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' update_tags('all_basiclevel')
#' }
update_tags <- function(repo) {
  run_git_command(repo, 'fetch --tags --prune --prune-tags')
}


#' Download the dataset version specified by a tag
#'
#' @inheritParams get_repo_path
#' @param tag tag label as a string
#'
#' @return NULL
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' checkout_tag('all_basiclevel', '0.1.0')
#' }
checkout_tag <- function(repo, tag) {
  # --quiet suppresses message about where HEAD was/is
  run_git_command(repo, glue::glue('checkout tags/{tag} --quiet'))
}



#' Finds the latest version tag
#'
#' @inheritParams get_repo_path
#' @param tags_already_updated boolean, have the repository tags been updated
#' recently? Avoids unnecessary fetching of the tags when multiple functions
#' that use tags are called in succession.
#'
#' @return The latest version tag as a string
#'
#' @examples
#' \dontrun{
#' get_latest_tag('all_basiclevel')
#' }
get_latest_tag <- function(repo, tags_already_updated = FALSE) {
  if (!tags_already_updated) {update_tags(repo)}
  versions <- run_git_command(repo, 'tag --sort version:refname',
                              return_output = TRUE)
  last_version <- tail(versions, 1)
  return(last_version)
}

#' List tags in the dataset repository
#' @noRd
get_tags <- function(repo, tags_already_updated = tags_already_updated) {
  if (!tags_already_updated) {update_tags(repo)}
  run_git_command(repo, 'tag --list', return_output = TRUE)
}


#' Find tag label of the currently checked out commit.
#'
#' @inherit get_latest_tag params return
#'
#' @examples
#' \dontrun{
#' all_bl <- get_all_basiclevel(version = '0.0.7')
#' current_tag <- get_current_tag('all_basiclevel')
#' stopifnot(current_tag == '0.0.7')
#' }
get_current_tag <- function(repo, tags_already_updated = FALSE) {
  if (!tags_already_updated) {update_tags(repo)}
  run_git_command(repo, 'describe --tags', return_output = TRUE)
}


#' Get the date of the currently checked out commit
#'
#' @inheritParams get_latest_tag
#'
#' @return A string with the date.
#'
#' @examples
#'
#' \dontrun{
#' get_current_commit_date('all_basiclevel')
#' }
get_current_commit_date <- function(repo) {
  current_commit <- run_git_command(repo = repo, command = 'rev-parse HEAD',
                                    return_output = TRUE)
  commit_date <- run_git_command(
    repo = repo,
    command = glue::glue('show -s --format=%ci {current_commit}'),
    return_output = TRUE)
  return(commit_date)
}
