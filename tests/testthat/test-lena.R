library(tools)
library(digest)
library(dplyr)


# Paths
pn_opus_path <- Sys.getenv('PN_OPUS_PATH')
subject_dir <- file.path(pn_opus_path, 'VIHI/SubjectFiles/LENA/HI/HI_424/HI_424_527/')
its_path <- file.path(subject_dir, 'HI_424_527.its')
assertthat::are_equal(md5sum(its_path), "43057eb7635560b8fbd726eeff040280",
                      check.names = FALSE)
its_xml <- rlena::read_its_file(its_path)

test_that("make_five_min_approximation works", {

  five_min <- make_five_min_approximation(its_xml)

  # Load LENA's 5min.csv file
  five_min_path <- file.path(subject_dir, 'HI_424_527_lena5min.csv')
  assertthat::are_equal(md5sum(five_min_path),
                        "03089a8c398a92370a230c076d3d14de",
                        check.names = FALSE)
  five_min_lena <- readr::read_csv(five_min_path, show_col_types = FALSE)

  # Find rows where the difference is more than we can tolerate
  larger_ratio <- function(x, y) {exp(abs(log(x) - log(y)))}
  bad_rows <- five_min_lena %>%
    mutate(interval_start = lubridate::mdy_hm(Timestamp) - lubridate::hours(1)) %>%
    select(interval_start, CTC.Actual, CVC.Actual, AWC.Actual) %>%
    left_join(five_min, by = 'interval_start') %>%
    filter((CTC.Actual.x != CTC.Actual.y)
           | (larger_ratio(CVC.Actual.x, CVC.Actual.y) > 1.33)
           | (larger_ratio(AWC.Actual.x, AWC.Actual.y) > 1.33))

  expect_equal(nrow(bad_rows), 0)

  # Check that the output hasn't changed.
  hashes_list <- five_min %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <- list(
    interval_start = "30841229360109291c62979020ce221e",
    interval_end = "5c43749516ccc29c9acf34c95ae217db",
    CVC.Actual = "5f7b4ed578e18847a1b4efc70e97416b",
    CTC.Actual = "2691ad0a451337ba2d49eb8a762783ee",
    AWC.Actual = "0afd0ffb338176699ce6142f077160e7")
  expect_equal(hashes_list, expected_hashes_list)
})


test_that("get_speaker_stats works", {
  speaker_stats <- get_speaker_stats(its_xml = its_xml,
                                     intervals = make_five_min_approximation(its_xml))
  hashes_list <- speaker_stats %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <-
    list(
      interval_start = "3944536a218ebdf9beebab9c278e65f8",
      interval_end = "11aad729d5940139bfc3293435133628",
      spkr = "7ebebd07f0a845c832b8219252449f25",
      adult_word_count = "36e936da25cce58e42903281bab232c8",
      utterance_count = "4b3da7892ee4443333266228d82d4999",
      segment_duration = "be048cfc1c64d01e5af4b8ba049d2229"
    )
  expect_equal(hashes_list, expected_hashes_list)
})
