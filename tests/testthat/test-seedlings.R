library(digest)
library(assertthat)

source(test_path('files.R'))

test_that("Reading and writing Seedlings audio annotations csvs works", {
  # Check that the csv files hasn't changed
  audio_csv_path <- seedlings_test_files$audio_csv_path()
  # If it has, manually check the result and update the hashes
  md5 <- "0c64a0a9db20f04d01702dbe2a948bab"
  assertthat::are_equal(md5sum(audio_csv_path), md5, check.names = FALSE)

  # Load
  annotations <- read_seedlings_audio_annotations(audio_csv_path)

  # Check contents are as expected
  hashes_list <- annotations %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_list <- list(
      tier = "5f3f70d4e95154a78d8ab7d8e9f810a8",
      word = "f5fa8de67e8028dbecbc2f45c572efd6",
      utterance_type = "dfa7e1c0ec6713516c9a92f2d837053c",
      object_present = "3ba9417bf93212c119d9158c4c059f84",
      speaker = "a6a4e114372f2971aade1d4b6b8c2e70",
      annotid = "abc920546e744be5fc5260db5974c78e",
      timestamp = "f224c768b777231631fb5da27a0bc046",
      onset = "bdc5715c56b53d8148b7e1c208fe7dcc",
      offset = "8d7fbd23c05221302a60af69185341e4",
      basic_level = "b8c12a9bf1420f0f01c1babeecb209b3",
      comment = "f57ee6b7b1242cde5f183f49d94404c3",
      pho = "3fe20f281971ea85ddb5427133edfc59"
    )
  expect_equal(hashes_list, expected_hashes_list)

  # Write
  tmp_file <- withr::local_tempfile()
  write_seedlings_audio_annotations(annotations, tmp_file)

  # Check that the files are identical
  expect_equal(md5sum(tmp_file), md5, ignore_attr = TRUE)
})
