library(digest)

test_that("add_global_basic_level works as expected", {
  # Load the data
  suppressWarnings({
    all_basiclevel_na <- get_all_basiclevel(version = '0.1.3',
                                            drop_basic_level_na = FALSE)
    mappings <- get_global_bl_mappings(version = '0.1.0')
    object_dict <- mappings$object_dict
    annotid_disambiguation <- mappings$annotid_disambiguation
  })

  # Check that the input data haven't changed (since the versions are set above,
  # only changes to the data-reading function can spoil things).
  expect_equal(digest(all_basiclevel_na), "741757773603e828afa5613344e72709")
  expect_equal(digest(object_dict), "157b4c027cf79ee5e57837a4ae89c6dc")
  expect_equal(digest(annotid_disambiguation),
               "0cfa534c658a1efb0ac36589d0315453")

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
      annotid = "207e1b1c9755effaa844e06d6d726476",
      object = "dccfe65b3fe6407b3dbf5906308eed82",
      basic_level = "08d332c4943e65a312b7b420ad4c29bf",
      global_bl = "63e028754c508fea4d1c923b3ebf8b47"
    )
  expect_equal(hashes, expected_hashes)
})
