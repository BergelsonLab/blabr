library(digest)

test_that("add_global_basic_level works as expected", {
  all_basiclevel_na <- get_all_basiclevel(version = '0.1.3',
                                          drop_basic_level_na = FALSE)
  output <- add_global_basic_level(all_basiclevel_na)

  with_global_basic_level <- output[[1]]
  dict_updated <- output[[2]]
  to_fix <- output[[3]]

  hashes <- with_global_basic_level %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes <- list(
    ordinal = "efc01375049e248c6fedce608ccbb24f",
    onset = "ca924312da625b0bacac785e79defdf1",
    offset = "24b15484e400d44fbe566f1aa009f1bd",
    object = "af15e94e37b6c6706e00274e79f0d21f",
    utterance_type = "66286f5e18d0e7821549175826ed06a0",
    object_present = "523f454077dc54ce66b7221c4bd2be35",
    speaker = "68487a65af1849011bfbb3649de84cbb",
    basic_level = "a2fedf5aaeb7e27fab07857fd4bcba58",
    annotid = "8b5d50f1978d84d7bd229720f6e3570f",
    id = "592100fb0b7c895daf2ddc934328c3ee",
    subj = "3f32cbc454046214e84f227bbd7c2394",
    month = "cb1386e6206a247af1b1bf9dd3947f92",
    SubjectNumber = "7d792830f8e4b587f26d9dd36e17e44c",
    audio_video = "f1bf2ca80964fb2c25076e577398bb2c",
    tier = "42d3b2d22848477af95e77c86b42fc71",
    pho = "76c8e1c42722ac5bce60c5132e9642d4",
    disambiguate = "1f5a3f182a4a395585ed1aebe14b9529",
    global_bl = "9aebc68fefa3721552f4efa62c15d26e")
  expect_equal(hashes, expected_hashes)

  hashes_dict_updated <- dict_updated %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_dict_updated <- list(
    object = "9079da00132de3805251f085dcd6abee",
    disambiguate = "8312c7f18c6308052a26cea4ddba4f57",
    global_bl = "347b078a4b542f05ecc08b61a46a3ae0")
  expect_equal(hashes_dict_updated, expected_hashes_dict_updated)

  hashes_to_fix <- to_fix %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_to_fix <- list(
    object = "44233b57b452017939df34ccdf25c3ec",
    basic_level = "1f77a1b1e6a61472d411c6319443d7ec",
    global_bl = "59ff237a0787f5db504a9b2d50c1252b")
  expect_equal(hashes_to_fix, expected_hashes_to_fix)

})
