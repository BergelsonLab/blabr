
get_all_basiclevel <- function(branch = NULL, commit = NULL, feather= FALSE) {
  get_csv_df('all_basiclevel', branch, commit, "all_basiclevel.csv")
}

get_cdi_spreadsheet <- function(branch = NULL, commit = NULL, feather= FALSE) {
  get_csv_df('cdi_spreadsheet', branch, commit, "cdi.csv")
}

get_motor_spreadsheet <- function(branch = NULL, commit = NULL, feather= FALSE) {
  if (feather) {
    get_feather_df('motor_spreadsheet', branch, commit, "motor.csv")
  } else {
    get_csv_df('motor_spreadsheet', branch, commit, "motor.csv")
  }
}

get_csv_df <- function(repo = "", branch = NULL, commit = NULL, filename = "") {
  sync_repo(repo, branch, commit)
  file_path <- file.path(blab_data, repo, filename)
  result <- read.csv(file_path)
  return(result)
}

get_feather_df <- function(repo = "", branch = NULL, commit = NULL, filename = "") {
  sync_repo(repo, branch, commit)
  file_path <- file.path(blab_data, repo, filename)
  result <- feather::read_feather(file_path)
  return(result)
}


