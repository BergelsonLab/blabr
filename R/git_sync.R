#this is the script that syncs data, or something.
git_bin <- Sys.which("git")

home_dir <- path.expand('~')
blab_data <- file.path(home_dir, "BLAB_DATA")

sync_to_upstream <- function(repo, branch) {
  print(git_bin)
  repo_path <- file.path(blab_data, repo)
  handle <- system2(git_bin, c('-C', repo_path, 'pull', 'origin', branch), wait = TRUE)
  print(handle)
}

checkout_commit <- function(repo, commit) {
  print(git_bin)
  repo_path <- file.path(blab_data, repo)
  handle <- system2(git_bin, c('-C', repo_path, 'checkout', commit), wait = TRUE)
  print(handle)
}

checkout_branch <- function(repo, branch) {
  print(git_bin)
  repo_path <- file.path(blab_data, repo)
  handle <- system2(git_bin, c('-C', repo_path, 'checkout', branch), wait = TRUE)
  print(handle)
}


sync_repo <- function(repo= "" , branch = NULL, commit = NULL) {
  if (!is.null(branch)) {
    checkout_branch(repo, branch)
  } else {
    checkout_branch(repo, 'master')
  }

  if(!is.null(commit)) {
    checkout_commit(repo, commit)
  } else {
    sync_to_upstream(repo, branch)
  }
}


