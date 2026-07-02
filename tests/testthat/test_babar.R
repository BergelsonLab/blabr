# TODO: potentially test read_babar as well
library(digest)
library(dplyr)
library(assertthat)

babar_df <- read_babar(system.file("extdata", "babar_test.csv", package = "blabr"), FALSE)

test_that("Reading and wrangling babar output files for measures", {
  
  # Test writing and then reading from the written file
  test_inventory <- get_inventory(babar_small, minimum_count = 0)
  test_consonant_inventory <- get_consonant_inventory(babar_small,minimum_count = 0)
  test_canonical <- get_canonical_metrics(babar_small)
  test_all <- get_metrics_and_inventory(babar_small, minimum_count = 0)
  test_with_minimum <- get_metrics_and_inventory(babar_small, minimum_count = 5)
  
  expect_equal(digest(test_inventory), "d884c36b1d62346800b6c84302899605")
  expect_equal(test_inventory$n_phonemes, 19)
  
  expect_equal(digest(test_consonant_inventory), "0fd67a0f18c91f6e49cd962135ea826f")
  expect_equal(test_consonant_inventory$n_consonants, 6)
  
  expect_equal(digest(test_canonical), "b76de598660dcabc87d3b7b98ca256b3")
  expect_equal(digest(test_all), "2189fb10904625079d94ec2c1ae3ab26")
  expect_equal(digest(test_with_minimum), "ac863fb09c3e1399eb5e2f31b222d3ec")
})
