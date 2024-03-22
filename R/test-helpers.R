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
