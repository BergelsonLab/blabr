library(tools)
library(digest)
library(dplyr)
library(assertthat)
library(lubridate)

source(test_path('files.R'))


# Paths
pn_opus_path <- Sys.getenv('PN_OPUS_PATH')
subject_dir <- file.path(pn_opus_path, 'VIHI/SubjectFiles/LENA/HI/HI_424/HI_424_527/')
its_path <- file.path(subject_dir, 'HI_424_527.its')
assert_that(are_equal(md5sum(its_path), "43057eb7635560b8fbd726eeff040280",
                      check.names = FALSE))

its_xml <- rlena::read_its_file(its_path)
intervals <- make_five_min_approximation(its_xml) %>%
  filter(CVC.Actual > 0 | CTC.Actual > 0 | AWC.Actual > 0)

test_that("make_five_min_approximation works", {

  five_min <- intervals

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
    interval_start = "dd39a74a6d68c20d1705b7598b0f0c0f",
    interval_end = "eba79d440ecce816059be4137248d61d",
    interval_start_wav = '24f8f6523aacce3f6077ac323f6ad7c5',
    CVC.Actual = "2e56ee6ca737e12db603b2dfca977fd0",
    CTC.Actual = "13ae94f062576bf7077937ed2e642929",
    AWC.Actual = "a37ae81e52ceedab965fce384c31dd01")
  expect_equal(hashes_list, expected_hashes_list)
})

test_that("add_lena_stats works", {
  its_xml <- rlena::read_its_file('/Users/ek221/blab/GIN/bergelson/.git/annex/objects/MM/z1/MD5E-s5185224--56ccf872857f514356e5f33bb3508d78.its/MD5E-s5185224--56ccf872857f514356e5f33bb3508d78.its')
  intervals <- intervals %>%
    add_interval_end_wav
  time_type <- 'wav'

  with_stats <- add_lena_stats(its_xml = its_xml, intervals = intervals,
                               time_type = time_type) %>%
    select(interval_start_wav, cvc, ctc, awc)

  # Check that the output hasn't changed.
  hashes_list <- with_stats %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <- list(
    interval_start_wav = "24f8f6523aacce3f6077ac323f6ad7c5",
    cvc = '57e495a10aac38de9f69ce65a9debcb3',
    ctc = "c111b230d4da19d6bef299ff3cad2f4b",
    awc = "396d875cee824b68806ab98280ffadc3")
  expect_equal(hashes_list, expected_hashes_list)
})

test_that("get_lena_speaker_stats works", {
  speaker_stats <- get_lena_speaker_stats(its_xml = its_xml,
                                          intervals = intervals)
  hashes_list <- speaker_stats %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <-
    list(
      interval_start = "af3b7189261a7e0e42a29fbd0d9f68b8",
      interval_end = "4dc12c6a1263e83a4304b911d609abab",
      spkr = "8ac0f06c6af287fafa4f5be525331a64",
      adult_word_count = "85be0a99e3b8483b49db53339f0ebe8f",
      utterance_count = "d440a246d3e73527227d37d30f2fd43d",
      segment_duration = "a1d8e60f1b05f54f31a98891e83c6b6a"
    )
  expect_equal(hashes_list, expected_hashes_list)
})


test_that("vtc stats functions work", {
  rttm_file <- withr::local_tempfile(lines = test_rttm_contents)
  rttm_tibble <- read_rttm(rttm_file)

  vtc_speaker_stats <- get_vtc_speaker_stats(intervals = intervals,
                                             all_rttm = rttm_tibble)
  hashes_list <- vtc_speaker_stats %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <-
    list(
      interval_start = "4636ec2d47ac698fe0f0e12573f9289f",
      interval_end = "9b520e4b56c2aff5f55fbc62cd68635a",
      voice_type = "20157b8c0da3aba27ed4eab5c2c17862",
      duration = "287b2789f7cf18919efee6399739c5e0",
      count = "2e7b87fed71c160b0b3f15d454f06e2b"
    )
  expect_equal(hashes_list, expected_hashes_list)

  vtc_stats <- add_vtc_stats(intervals = intervals,
                             all_rttm = rttm_tibble)
  hashes_list <- vtc_stats %>%
    summarise(across(everything(), digest)) %>%
    as.list
  # The existing columns haven't changed
  expect_equal(intervals, select(vtc_stats, colnames(intervals)))
  # New columns have expected values
  hashes_list <- vtc_stats %>%
    select(-colnames(intervals)) %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <- list(vtc_ctc = "733f40b946cb233ad69cf7c795d0397a")
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
      interval_start = "3d00e967a7d5f7aefa2e907241759b77",
      interval_end = "c89c1017d5e60fc7c10c13d8f9ccd681",
      speaker = "04838ee4fba81baec9ef46a89bdcfc72",
      n_annotations = "359d31cea834bc913c6bccc5afa1858c"
    )
  expect_equal(hashes_list, expected_hashes_list)
})


test_that("sampling functions work as expected", {
  # # Random sampling

  # Running with a seed should produce the same result every time
  sample_with_seed <- sample_intervals_randomly(intervals, duration = '5 mins',
                                                size = 15, seed = 72)
  hashes_list <- sample_with_seed %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <-
    list(
      interval_start = "9ecae2dda5b2e8af8ad905a0faa85a96",
      interval_end = "f8bc10d4d8fefe549c1df7e192ec0e99"
    )
  expect_equal(hashes_list, expected_hashes_list)

  # Running without a seed should produce the same result extremely rarely
  sample_no_seed <- sample_intervals_randomly(intervals, duration = '5 mins',
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
    sample_intervals_with_highest(ctc_cvc_average, size = 15,
                                  duration = '5 mins')
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
    sample_intervals_periodically(duration = '5 mins', period = '1 hour')
  hashes_list_periodic <- sample_periodic %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list_periodic <- list(
      interval_start = "40615f8ef6cfa2347da279481b69b1f0",
      interval_end = "aca6f8b9e601c64e685a7ab54bdd620a"
    )
  expect_equal(hashes_list_periodic, expected_hashes_list_periodic)
})
