test_that("get_vihi_annotations works", {
  version <- '0.0.0.9006-dev.1'
  for (table in c('annotations', 'intervals', 'merged')) {
    expect_non_empty_dataframe(get_vihi_annotations(version = version,
                                                    table = table))
  }
})
