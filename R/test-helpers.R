# I have no idea why I (Zhenya) decided to put this here instead of next to the
# test that uses it. It should probably be moved there.
test_rttm_contents <- c(
  "SPEAKER filename 1 13.993 1.065 <NA> <NA> MAL <NA> <NA>",
  "SPEAKER filename 1 13.993 1.166 <NA> <NA> SPEECH <NA> <NA>",
  "SPEAKER filename 1 16.511 5.840 <NA> <NA> SPEECH <NA> <NA>",
  "SPEAKER filename 1 16.590 0.106 <NA> <NA> MAL <NA> <NA>",
  "SPEAKER filename 1 16.991 3.520 <NA> <NA> KCHI <NA> <NA>",
  "SPEAKER filename 1 19.992 0.246 <NA> <NA> CHI <NA> <NA>",
  "SPEAKER filename 1 20.363 1.556 <NA> <NA> MAL <NA> <NA>",
  "SPEAKER filename 1 21.011 0.959 <NA> <NA> FEM <NA> <NA>"
)

expect_non_empty_dataframe <- function(object) {
  expect_s3_class(object, 'data.frame')
  expect_gt(nrow(object), 0)
}

without_warning_or_messages <- function(expr) {
  suppressWarnings(suppressMessages(eval(expr)))
}

assert_df_matches_col_types <- function(df, col_types) {
  # Assert that the column names in the data frame completely match the column
  # names in the col_types specification. This is different from read_csv's
  # behavior, which issues a warning when col_types contains columns that aren't
  # in the file and does nothing when the file contains columns that aren't in
  # col_types.

  # Extract column names from the dataframe
  df_col_names <- sort(colnames(df))

  # Extract column names from col_types specification
  col_types_col_names <- sort(names(col_types$cols))

  # Check if the sets of column names are the same, ignoring order
  if (!setequal(df_col_names, col_types_col_names)) {
    missing_cols <- setdiff(col_types_col_names, df_col_names)
    extra_cols <- setdiff(df_col_names, col_types_col_names)

    error_message <- "Column names do not match.\n"

    if (length(missing_cols) > 0) {
      error_message <- paste0(error_message,
                              "Missing columns in DataFrame: ",
                              paste(shQuote(missing_cols), collapse = ", "),
                              ".\n")}

    if (length(extra_cols) > 0) {
      error_message <- paste0(error_message,
                              "Extra columns in DataFrame: ",
                              paste(shQuote(extra_cols), collapse = ", "),
                              ".\n")}

    stop(error_message)
  }
}

calculate_column_hashes <- function(df) {
  df %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), digest::digest)) %>%
    as.list
}

#' Checks that each column in a data frame has the same data as before
#'
#' To use, run `dput(calculate_column_hashes(<your-df>))` in the console and
#' paste the output into `expect_column_contents(<your-df>, <paste-here>)`.
#' @noRd
expect_column_contents <- function(df, expected_hashes) {
  actual_hashes <- calculate_column_hashes(df)
  expect_mapequal(actual_hashes, expected_hashes)
}
