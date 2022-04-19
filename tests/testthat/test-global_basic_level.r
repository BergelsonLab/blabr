library(digest)

test_that("add_global_basic_level works as expected", {
  all_basiclevel_na <- get_all_basiclevel(version = '0.1.3',
                                          drop_basic_level_na = FALSE)
  mappings_dir <- file.path(blab_data, 'global_basic_level', 'data')
  object_dict <- read_object_dict(
    file.path(mappings_dir, "global_bl_dictionary.csv"))
  annotid_disambiguation <- read_annotid_disambiguation(
    file.path(mappings_dir, "disambiguated_rows.csv"))

  with_global_basic_level <- make_global_basic_level(
    all_basiclevel_na, object_dict,
    annotid_disambiguation)$with_global_bl

  hashes <- with_global_basic_level %>%
    mutate(across(where(is.numeric), as.numeric),
           across(where(is.factor), as.character)) %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes <-
    list(
      annotid = "207e1b1c9755effaa844e06d6d726476",
      object = "dccfe65b3fe6407b3dbf5906308eed82",
      basic_level = "08d332c4943e65a312b7b420ad4c29bf",
      disambiguate = "ac9b8c415095981cb048e676d4e14b4f",
      global_bl = "8b41b0f9a64f52649167dff170d23390"
    )
  expect_equal(hashes, expected_hashes)
})
