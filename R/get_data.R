
get_all_basiclevel <- function(branch = NULL, commit = NULL, type="feather") {
  switch(type,
         "csv" = get_df_file('all_basiclevel', "all_basiclevel.csv", branch, commit),
         "feather" = get_df_file('all_basiclevel', "all_basiclevel.feather", branch, commit))
}

get_cdi_spreadsheet <- function(branch = NULL, commit = NULL, type="feather") {
  switch(type,
         "csv" = get_df_file('cdi_spreadsheet', "cdi.csv", branch, commit),
         "feather" = get_df_file('motor_spreadsheet', "cdi.feather", branch, commit))
}

get_motor_spreadsheet <- function(branch = NULL, commit = NULL, type="feather") {
  switch(type,
         "csv" = get_df_file('motor_spreadsheet', "motor.csv", branch, commit),
         "feather" = get_df_file('motor_spreadsheet', "motor.feather", branch, commit))
}


get_df_file <- function(repo, filename, branch, commit) {
  sync_repo(repo, branch, commit)
  file_path <- file.path(blab_data, repo, filename)
  if (endsWith(file_path, ".csv")) {
    result <- read.csv(file_path)
    print("reading csv")
  } else if (endsWith(file_path, ".feather")) {
    result <- feather::read_feather(file_path)
    print("reading feather")
  }
  return(result)
}
