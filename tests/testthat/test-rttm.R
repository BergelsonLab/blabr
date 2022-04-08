test_that("Reading and writing rttm files work", {
  # Create an RTTM file
  rttm_file <- withr::local_tempfile(
    lines = c("SPEAKER filename 1 13.993 1.065 <NA> <NA> MAL <NA> <NA>",
              "SPEAKER filename 1 13.993 1.166 <NA> <NA> SPEECH <NA> <NA>",
              "SPEAKER filename 1 16.511 5.840 <NA> <NA> SPEECH <NA> <NA>",
              "SPEAKER filename 1 16.590 0.106 <NA> <NA> MAL <NA> <NA>",
              "SPEAKER filename 1 16.991 3.520 <NA> <NA> KCHI <NA> <NA>",
              "SPEAKER filename 1 19.992 0.246 <NA> <NA> CHI <NA> <NA>",
              "SPEAKER filename 1 20.363 1.556 <NA> <NA> MAL <NA> <NA>",
              "SPEAKER filename 1 21.011 0.959 <NA> <NA> FEM <NA> <NA>"))

  # Test reading
  rttm_tibble <- read_rttm(rttm_file)

  # Test writing and then reading from the written file
  rttm_file_written <- withr::local_tempfile()
  rttm_tibble %>% write_rttm(rttm_file_written)
  rttm_tibble2 <- read_rttm(rttm_file_written)
  expect_equal(rttm_tibble, rttm_tibble2)
})
