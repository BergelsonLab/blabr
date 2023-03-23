library(digest)
library(dplyr)

test_that("assign_global_basic_level works as expected", {
  # Load the data
  suppressWarnings({
    all_basiclevel_na <- get_all_basiclevel(version = '0.6.4',
                                            drop_basic_level_na = FALSE)
    mappings <- get_global_bl_mappings(version = '0.6.4')
    object_dict <- mappings$object_dict
    annotid_disambiguation <- mappings$annotid_disambiguation
  })

  # Check that the input data haven't changed (since the versions are set above,
  # only changes to the data-reading function can spoil things).
  expect_equal(digest(all_basiclevel_na), "9170807a8cfdc4cead7b450d0c16006c")
  expect_equal(digest(object_dict), "dc1f5f6101d3fad69cfee7c947d34c78")
  expect_equal(digest(annotid_disambiguation),
               "a0484bae138d392a4cdee4f6a73a51f4")

  # Do the assignment
  global_basic_level <- assign_global_basic_level(
    all_basiclevel_na, object_dict,
    annotid_disambiguation)

  hashes <- global_basic_level %>%
    mutate(across(where(is.numeric), as.numeric),
           across(where(is.factor), as.character)) %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes <-
    list(
      annotid = "4107e4afec033433a8d120a3a5943e76",
      object = "5c4dc9cc50e782c2e312283babf3a582",
      basic_level = "0488cad5e1ba357ef15c6ea0fb5a7295",
      global_bl = "0272d3536b812767e7ed5ba6185f4b32"
    )
  expect_equal(hashes, expected_hashes)
})
