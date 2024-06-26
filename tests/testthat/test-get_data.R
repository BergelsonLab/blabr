library(blabr)


# Check specific versions of datasets that are loaded differently
test_that(
  "dataset versions before and after formatting changes can be loaded",
  {
    without_warning_or_messages({
      for (version in c('0.3.3',
                        '0.4.0',  # only single csv left
                        '0.5.0')  # global_bl column added
      ){
        expect_non_empty_dataframe(get_all_basiclevel(version))
      }
    })
})


# get_functions issue a warning when called without version specified
without_warning_or_messages({
  gbl <- get_global_bl_mappings()
  datasets <- list(
    all_basiclevel = get_all_basiclevel(),
    cdi = get_cdi_spreadsheet(),
    motor = get_motor_spreadsheet(),
    reliability = get_reliability("audio", "06"),
    object_dict = gbl$object_dict,
    annotid_disambiguation = gbl$annotid_disambiguation
)})

# TODO: merge with the loading code.
test_that("all datasets can be downloaded", {
  for (dataset in datasets) {
    expect_non_empty_dataframe(dataset)
  }
})


# # load the csv versions
# without_warning_or_messages({datasets_csv <- list(
#   cdi = get_cdi_spreadsheet(type = 'csv'),
#   motor = get_motor_spreadsheet(type = 'csv')
#   # reliability and seedlings_nouns only have a csv version so there is no
#   # need to test them again.
# )})
#
#
# test_that("same results after loading from csv and feather", {
#   for (dataset_name in names(datasets_csv)) {
#     # dplyr::all_equal considers unordered factors with different order of
#     # levels
#     expect_true(all.equal(datasets[[dataset_name]],
#                           datasets_csv[[dataset_name]],
#                           check.attributes = FALSE))
#   }
# })
