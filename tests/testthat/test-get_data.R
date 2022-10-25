library(blabr)

# get_functions issue a warning when called without version specified
suppressWarnings({datasets <- list(
  all_basiclevel = get_all_basiclevel(),
  seedslings_nouns = get_seedlings_nouns(),
  cdi = get_cdi_spreadsheet(),
  motor = get_motor_spreadsheet(),
  reliability = get_reliability("audio", "06")
)})


test_that("all datasets can be downloaded", {
  for (dataset in datasets) {
    expect_s3_class(dataset, 'data.frame')
    expect_gt(nrow(dataset), 0)
  }
})


# laod the csv versions
suppressWarnings({datasets_csv <- list(
  all_basiclevel = get_all_basiclevel(type = 'csv'),
  cdi = get_cdi_spreadsheet(type = 'csv'),
  motor = get_motor_spreadsheet(type = 'csv')
  # reliability and seedlings_nouns does only have a csv version so there is no
  # need to test them again.
)})


test_that("same results after loading from csv and feather", {
  for (dataset_name in names(datasets_csv)) {
    # dplyr::all_equal considers unordered factors with different order of
    # levels
    expect_true(all.equal(datasets[[dataset_name]],
                          datasets_csv[[dataset_name]],
                          check.attributes = FALSE))
  }
})
