library(digest)

test_that("add_global_basic_level works as expected", {
  all_basiclevel_na <- get_all_basiclevel(version = '0.1.3',
                                          drop_basic_level_na = FALSE)
  mappings_dir <- file.path(blab_data, 'global_basic_level', 'data')
  output <- add_global_basic_level(all_basiclevel_na, mappings_dir)

  with_global_basic_level <- output[[1]]
  dict <- output[[2]]
  to_fix <- output[[3]]

  hashes <- with_global_basic_level %>%
    mutate(across(where(is.numeric), as.numeric),
           across(where(is.factor), as.character)) %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes <-
    list(
      ordinal = "2a1a86e041ca65962428b8b49d938f7a",
      onset = "6c03cd25ef0780feb55d15fc44472dfd",
      offset = "33b0bd6f6d7181286ae3abdcceb50d06",
      object = "28f0b9cf94fc753b4525c5e5def55e57",
      utterance_type = "059ff29157bba28b23a48f7dbb5d3455",
      object_present = "c32de968fb2578ed4d866286dbb35cca",
      speaker = "60bc11e0a31badfd15648dd12656df2f",
      basic_level = "fa3ea6bf10b33ee64046e58d0cd8cd68",
      annotid = "90aa9d4ef632694c317ce2aa8c3b8aa3",
      id = "5dc218556e7c202cb8f24af0929490b6",
      subj = "b594d4cb8774ab705b5109d38e2d5e72",
      month = "4f0f1775b7ae9e72d095ffd18ca91e75",
      SubjectNumber = "5667aadce4d103da567eb7d5f5c7f6c0",
      audio_video = "31e2bbaf624b62c49b842429763a2f23",
      tier = "4800e1b480ebaa4810436aa3bfe7a839",
      pho = "1e99888bc40cf6d45c7a0314ff3f8b93",
      disambiguate = "41dbe1859f00df4d03e2e96f3ac28312",
      global_bl = "4cb188f0a6508d9fac4deacb3cf5cac7"
    )
  expect_equal(hashes, expected_hashes)

  hashes_dict_updated <- dict_updated %>%
    mutate(across(where(is.numeric), as.numeric),
           across(where(is.factor), as.character)) %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_dict_updated <- list(object = "14bfa11490b253c5a6dc729724175b51",
                                       disambiguate = "34da6e26409c102ee1ea9e3877e3003c",
                                       global_bl = "b5e51f8bb04fd022edc3f1f1fb2ed404")
  expect_equal(hashes_dict_updated, expected_hashes_dict_updated)

  hashes_to_fix <- to_fix %>%
    mutate(across(where(is.numeric), as.numeric),
           across(where(is.factor), as.character)) %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_to_fix <-
    list(object = "750b2a3394a8bd154cec0bc043e44e61",
         basic_level = "f98e49bca949e21577a533b5a712aac7",
         global_bl = "acdb3a7ad6e9e51c4a8b87de2a9ef76a")
  expect_equal(hashes_to_fix, expected_hashes_to_fix)

})
