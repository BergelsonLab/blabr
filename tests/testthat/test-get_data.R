library(blabr)

test_that("all datasets can be downloaded", {
  # get_functions issue a warning when called without version specified
  suppressWarnings({
    all_bl <- get_all_basiclevel()
    cdi <- get_cdi_spreadsheet()
    motor <- get_motor_spreadsheet()
    relia <- get_reliability("audio", "06")
  })

  for (dataset in list(all_bl, cdi, motor, relia)) {
    expect_s3_class(dataset, 'data.frame')
    expect_gt(nrow(dataset), 0)
  }
})
