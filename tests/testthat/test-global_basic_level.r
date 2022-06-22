library(digest)
library(dplyr)

test_that("assign_global_basic_level works as expected", {
  # Load the data
  suppressWarnings({
    all_basiclevel_na <- get_all_basiclevel(version = '0.1.5',
                                            drop_basic_level_na = FALSE)
    mappings <- get_global_bl_mappings(version = '0.1.4')
    object_dict <- mappings$object_dict
    annotid_disambiguation <- mappings$annotid_disambiguation
  })

  # Check that the input data haven't changed (since the versions are set above,
  # only changes to the data-reading function can spoil things).
  expect_equal(digest(all_basiclevel_na), "ac45f6aae8d35853ab7510f9a7536fc9")
  expect_equal(digest(object_dict), "bddf031a7603164db6c46b57f68cc2bc")
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
      annotid = "b9ecf89588e02f5a33e5e6e3500c8e77",
      object = "d727bde5077f61eab93565ca53cd4943",
      basic_level = "492a9154886f40ac8dc2c7ed4b58eb37",
      global_bl = "df728c5e4d28b57ccfdda8502cfafcd3"
    )
  expect_equal(hashes, expected_hashes)
})
