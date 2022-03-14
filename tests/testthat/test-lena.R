library(tools)
library(digest)

test_that("make_five_min_approximation works", {
  # Paths
  pn_opus_path <- Sys.getenv('PN_OPUS_PATH')
  subject_dir <- file.path(pn_opus_path, 'VIHI/SubjectFiles/LENA/HI/HI_424/HI_424_527/')
  its_path <- file.path(subject_dir, 'HI_424_527.its')
  assertthat::are_equal(md5sum(its_path), "43057eb7635560b8fbd726eeff040280",
                        check.names = FALSE)

  # Load and process the its file
  its <- rlena::read_its_file(its_path)
  five_min <- make_five_min_approximation(its)

  # Load LENA's 5min.csv file
  five_min_path <- file.path(subject_dir, 'HI_424_527_lena5min.csv')
  assertthat::are_equal(md5sum(five_min_path),
                        "03089a8c398a92370a230c076d3d14de",
                        check.names = FALSE)
  five_min_lena <- readr::read_csv(five_min_path, show_col_types = FALSE)

  # Find rows where the difference is more than we can tolerate
  bad_rows <- five_min_lena %>%
    mutate(five_min_time = lubridate::mdy_hm(Timestamp) - lubridate::hours(1)) %>%
    select(five_min_time, CTC.Actual, CVC.Actual, AWC.Actual) %>%
    left_join(five_min, by = 'five_min_time') %>%
    filter((CTC.Actual.x != CTC.Actual.y)
           | (larger_ratio(CVC.Actual.x, CVC.Actual.y) > 1.33)
           | (larger_ratio(AWC.Actual.x, AWC.Actual.y) > 1.33))

  expect_equal(nrow(bad_rows), 0)

  # Check that the output hasn't changed.
  hashes_list <- five_min %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <- list(
    five_min_time = "c8a6801517f999553ed1af4e6300eb87",
    CVC.Actual = "5f7b4ed578e18847a1b4efc70e97416b",
    CTC.Actual = "2691ad0a451337ba2d49eb8a762783ee",
    AWC.Actual = "0afd0ffb338176699ce6142f077160e7",
    duration = "31fe2ff3c3195ccfae3559b654fb4d3d")
  expect_equal(hashes_list, expected_hashes_list)
})
