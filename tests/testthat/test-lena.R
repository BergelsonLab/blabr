library(tools)
library(digest)
library(dplyr)

source(test_path('files.R'))


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
    interval_start_wav = '4e0425f5fe42f2268aca95582dfde58d',
    CVC.Actual = "5f7b4ed578e18847a1b4efc70e97416b",
    CTC.Actual = "2691ad0a451337ba2d49eb8a762783ee",
    AWC.Actual = "0afd0ffb338176699ce6142f077160e7")
  expect_equal(hashes_list, expected_hashes_list)
})

intervals <- make_five_min_approximation(its_xml)

test_that("get_lena_speaker_stats works", {
  speaker_stats <- get_lena_speaker_stats(its_xml = its_xml,
                                          intervals = intervals)
  hashes_list <- speaker_stats %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <-
    list(
      interval_start = "75ff43e40a186ae138dc9b709b691a45",
      interval_end = "5e39906727aa950a55bff1f80d4226bb",
      spkr = "8b19ab3ad09943f2c807002c40ebe943",
      adult_word_count = "a3dd76d9042133d4ee0a6ccbc654ba48",
      utterance_count = "d36f09f3bbada305d0925623a9ffb990",
      segment_duration = "178fa344206b188de05bea4f07fe2b50"
    )
  expect_equal(hashes_list, expected_hashes_list)
})


test_that("get_vtc_speaker_stats works", {
  rttm_file <- withr::local_tempfile(lines = test_rttm_contents)
  rttm_tibble <- read_rttm(rttm_file)

  vtc_speaker_stats <- get_vtc_speaker_stats(all_rttm = rttm_tibble,
                                             intervals = intervals)
  hashes_list <- vtc_speaker_stats %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <-
    list(
      interval_start = "baf6a3760dd3f469a162adbc4c488f60",
      interval_end = "6f84f761ab19ee9a31b4bb8d7b7458de",
      voice_type = "83dfcaf087002288a5b78fd3035e40a1",
      duration = "91c2e44c4ab4d2a77e62f9359a7f4fa7",
      count = "63319a19e3821e1cf814285e812f325d"
    )
  expect_equal(hashes_list, expected_hashes_list)
})


test_that("get_seedlings_speaker_stats works", {
  annotations <- seedlings_test_files$audio_annotations()
  intervals <- seedlings_test_files$its_xml() %>%
    make_five_min_approximation()

  seedlings_speaker_stats <-
    get_seedlings_speaker_stats(annotations = annotations,
                                intervals = intervals)

  hashes_list <- seedlings_speaker_stats %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <-
    list(
      interval_start = "77a9629f3b700005c5a9654374e692f8",
      interval_end = "107131b5e20361dfac8286db00dab99a",
      tier = "041f9320ffdb5c412b2a10309de1e1da",
      n_annotations = "96c890d6c4d0ad3735e23dd222e53b9c"
    )
  expect_equal(hashes_list, expected_hashes_list)
})


test_that("sampling functions work as expected", {
  # # Random sampling

  # Running with a seed should produce the same result every time
  sample_with_seed <- sample_intervals_randomly(intervals, period = '5 mins',
                                                size = 15, seed = 72)
  hashes_list <- sample_with_seed %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <-
    list(
      interval_start = "d1d26e6cb8477e7f9945ee60a8e6b33a",
      interval_end = "5d999b9dec4d9b7262f9a76c8f31b88c"
    )
  expect_equal(hashes_list, expected_hashes_list)

  # Running without a seed should produce the same result extremely rarely
  sample_no_seed <- sample_intervals_randomly(intervals, period = '5 mins',
                                              size = 15)
  hashes_list_no_seed <- sample_no_seed %>%
    summarise(across(everything(), digest)) %>%
    as.list
  # Not a single hash should match
  expect_false(any(unlist(hashes_list_no_seed) == unlist(expected_hashes_list)))

  # # Sampling intervals that have a highest value of a metric

  sample_highest <- intervals %>%
    dplyr::mutate(
      .tmp_interval_duration = as.numeric(interval_end - interval_start),
      .tmp_ctc_rate = CTC.Actual * .tmp_interval_duration,
      .tmp_ctc_normalized = .tmp_ctc_rate / sum(.tmp_ctc_rate),
      .tmp_cvc_rate = CVC.Actual * .tmp_interval_duration,
      .tmp_cvc_normalized = .tmp_cvc_rate / sum(.tmp_cvc_rate),
      ctc_cvc_average = 0.5 * (.tmp_ctc_normalized + .tmp_cvc_normalized)) %>%
    select(-starts_with('.tmp')) %>%
    sample_intervals_with_highest(ctc_cvc_average, size = 15, period = '5 mins')
  hashes_list_highest <- sample_highest %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list_highest <- list(
    interval_start = "0731efa8e777713e5a713cddc91838d1",
    interval_end = "fc4387dfb138d38e6fe772dee36fe147",
    ctc_cvc_average = "a277199f5646bfb4d6ff27c7fb121075"
  )
  expect_equal(hashes_list_highest, expected_hashes_list_highest)

  # # Sampling intervals every sampling period, e.g. 1 hour

  sample_periodic <- intervals %>%
    sample_intervals_periodically(interval_period = '5 mins',
                                  sampling_period = '1 hour')
  hashes_list_periodic <- sample_periodic %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list_periodic <- list(
      interval_start = "f49d3fc446b5a49ac50df6635e6d1cf8",
      interval_end = "ac85830a7ac8b274e711adbfbf2fce8e"
    )
  expect_equal(hashes_list_periodic, expected_hashes_list_periodic)
})
