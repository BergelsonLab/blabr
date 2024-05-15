# Custom predicate function to check for specific columns with assertthat
has_columns <- function(df, required_cols) {
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.character(required_cols),
                          length(required_cols) > 0)
  all(required_cols %in% colnames(df))
}

# Custom failure message for the assertion
assertthat::on_failure(has_columns) <- function(call, env) {
  df <- eval(call$df, env)
  required_cols <- eval(call$required_cols, env)
  missing_cols <- setdiff(required_cols, colnames(df))
  paste0("The following required columns are missing: ",
         paste(missing_cols, collapse = ", "))
}
