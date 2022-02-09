#this is the script that syncs data, or something.
git_bin <- Sys.which("git")

home_dir <- path.expand('~')
blab_data <- file.path(home_dir, "BLAB_DATA")


#' Absolute path to a dataset folder
#'
#' @param repo dataset name: 'all_basiclevel', 'reliability', etc.
#'
#' @return absolute path as a string
#'
#' @examples
#' get_repo_path('all_basiclevel')
get_repo_path <- function(repo) {
  file.path(blab_data, repo)
}


#' Run a git command in a dataset repository
#'
#' @inheritParams get_repo_path
#' @param command what you would have after `git` on the command line except
#' for the -C <folder> part - the function will do that part for you.
#' @param return_output boolean, whether to return the printed output of the
#' command as string
#'
#' @return string with output if return_output is TRUE, else NULL
#'
#' @examples
#' run_git_command('all_basiclevel', 'status')
run_git_command <- function(repo, command, return_output = FALSE) {
  arguments <- c('-C', get_repo_path(repo), strsplit(command, '\\s+')[[1]])
  result <- system2(git_bin, arguments, wait = TRUE, stdout = TRUE)
  if (return_output) {return(result)}
}


#' Fetches dataset tags from GitHub
#'
#' @inheritParams get_repo_path
#'
#' @return NULL
#'
#' @examples
#' update_tags('all_basiclevel')
update_tags <- function(repo) {
  run_git_command(repo, 'fetch --tags --prune')
}


#' Download the dataset version specified by a tag
#'
#' @inheritParams get_repo_path
#' @param tag tag label as a string
#'
#' @return NULL
#'
#' @examples
#' checkout_tag('all_basiclevel', '0.1.0')
checkout_tag <- function(repo, tag) {
  run_git_command(repo, glue::glue('checkout tags/{tag}'))
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
#' get_latest_tag('all_basiclevel')
get_latest_tag <- function(repo, tags_already_updated = FALSE) {
  if (!tags_already_updated) {update_tags(repo)}
  run_git_command(repo, 'tag --sort version:refname | tail -1',
                  return_output = TRUE)
}


#' Find tag label of the currently checked out commit.
#'
#' @inherit get_latest_tag params return
#'
#' @examples
#' all_bl <- get_all_basiclevel(version = '0.0.7')
#' current_tag <- get_current_tag('all_basiclevel')
#' stopifnot(current_tag == '0.0.7')
get_current_tag <- function(repo, tags_already_updated = FALSE) {
  if (!tags_already_updated) {update_tags(repo)}
  run_git_command(repo, 'describe --tags', return_output = TRUE)
}


#' Get the date of the currently checked out commit
#'
#' @inheritParams get_latest_tag
#'
#' @return
#'
#' @examples
#' get_current_commit_date('all_basiclevel')
get_current_commit_date <- function(repo) {
  current_commit <- run_git_command(repo = repo, command = 'rev-parse HEAD',
                                    return_output = TRUE)
  commit_date <- run_git_command(
    repo = repo,
    command = glue::glue('show -s --format=%ci {current_commit}'),
    return_output = TRUE)
  return(commit_date)
}
