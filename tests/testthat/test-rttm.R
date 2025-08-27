test_that("Reading and writing rttm files work", {
  # Create an RTTM file
  # test_rttm_contents is defined in R/test-helpers.R
  rttm_file <- withr::local_tempfile(lines = test_rttm_contents)

  # Test reading
  rttm_tibble <- read_rttm(rttm_file)

  # Test writing and then reading from the written file
  rttm_file_written <- withr::local_tempfile()
  rttm_tibble %>% write_rttm(rttm_file_written)
  rttm_tibble2 <- read_rttm(rttm_file_written)
  expect_equal(rttm_tibble, rttm_tibble2)
})
