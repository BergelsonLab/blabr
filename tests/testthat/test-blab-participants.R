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
  
  # participants_df <- data.frame(
  #   blab_id = c("B1", "B2", "B3"),
  #   RN = c("12345678", "87654321", "00000000"),
  #   CHS_global_id = c(NA, NA, "abc-xyz"),
  #   n_projects = c(2, 2, 1),
  #   biwfr = c("bwfr001", "bwfr_002", "BWFR_003"),
  #   wfr = c(NA, "WFR02", NA),
  #   clf = c("CLF_10", NA, NA),
  #   pbs = c(NA, NA, NA)
  # )
  
})
