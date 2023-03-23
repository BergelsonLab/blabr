library(tools)
library(digest)
library(assertthat)

source(test_path('files.R'))

test_that("Reading and writing Seedlings audio annotations csvs works", {
  skip('Not sure how to fix. See https://github.com/BergelsonLab/blabr/issues/37')

  # Check that the csv file hasn't changed
  audio_csv_path <- seedlings_test_files$audio_csv_path()
  # If it has, manually check the result and update the hashes
  md5 <- "949fb3f8c03506e628eae9068afdcc46"
  expect_equal(md5sum(audio_csv_path), md5, check.names = FALSE)

  # Load
  annotations <- read_seedlings_audio_annotations(audio_csv_path)

  # Check contents are as expected
  hashes_list <- annotations %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <-
    list(
      ordinal = "2983a904b22c8a474eced0ce35b5f6bd",
      tier = "5f3f70d4e95154a78d8ab7d8e9f810a8",
      word = "55dc6b60d53fe7ebfbf8455c2701da51",
      utterance_type = "8aacf5527d1d05a6ea669b28841d8faa",
      object_present = "e280c78440a86bc30736e1487d1054be",
      speaker = "cf3f384a6901acb5ec1e53affae68468",
      annotid = "b0bdf31559e5747db058b7022d737e81",
      timestamp = "f224c768b777231631fb5da27a0bc046",
      onset = "bdc5715c56b53d8148b7e1c208fe7dcc",
      offset = "8d7fbd23c05221302a60af69185341e4",
      basic_level = "1ec66e8a9c440a70c8b95d6959784aa1",
      comment = "f57ee6b7b1242cde5f183f49d94404c3",
      pho = "3fe20f281971ea85ddb5427133edfc59"
    )
  expect_equal(hashes_list, expected_hashes_list)

  # Check the round trip
  tmp_file <- withr::local_tempfile()
  write_seedlings_audio_annotations(annotations, tmp_file)
  expect_equal(md5sum(tmp_file), md5, ignore_attr = TRUE)
})
