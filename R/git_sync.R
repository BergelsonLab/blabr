git_bin <- Sys.which("git")

home_dir <- path.expand('~')
blab_data <- file.path(home_dir, "BLAB_DATA")

sync_to_upstream <- function(repo, branch) {
  print(git_bin)
  repo_path <- file.path(blab_data, repo)
  handle <- subprocess::spawn_process(git_bin, c('-C', repo_path, 'pull', 'origin', branch))
  subprocess::process_wait(handle, subprocess::TIMEOUT_INFINITE)
  print(handle)
  subprocess::process_read(handle, subprocess::PIPE_STDOUT, timeout = 1000)
}

checkout_commit <- function(repo, commit) {
  print(git_bin)
  repo_path <- file.path(blab_data, repo)
  handle <- subprocess::spawn_process(git_bin, c('-C', repo_path, 'checkout', commit))
  subprocess::process_wait(handle, subprocess::TIMEOUT_INFINITE)
  print(handle)
  subprocess::process_read(handle, subprocess::PIPE_STDOUT, timeout = 1000)
}

checkout_branch <- function(repo, branch) {
  print(git_bin)
  repo_path <- file.path(blab_data, repo)
  handle <- subprocess::spawn_process(git_bin, c('-C', repo_path, 'checkout', branch))
  subprocess::process_wait(handle, subprocess::TIMEOUT_INFINITE)
  print(handle)
  subprocess::process_read(handle, subprocess::PIPE_STDOUT, timeout = 1000)
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

### commenterrific ###
