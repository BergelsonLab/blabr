library(digest)
library(dplyr)
library(assertthat)

test_that("Cleaning local subject ids", {
  expect_equal(parse_raw_id("CLF20"), "CLF_20")
  expect_equal(parse_raw_id("clf20"), "CLF_20")
  expect_equal(parse_raw_id("clf0020"), "CLF_20")
  expect_equal(parse_raw_id("CLF_20"), "CLF_20")
  expect_equal(parse_raw_id("clF_020"), "CLF_20")
  expect_equal(parse_raw_id("CLF_Pilot_20"), "CLFPILOT_20")
})
