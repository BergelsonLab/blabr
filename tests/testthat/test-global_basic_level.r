library(digest)

test_that("add_global_basic_level works as expected", {
  all_basiclevel_na <- get_all_basiclevel(version = '0.1.3',
                                          drop_basic_level_na = FALSE)
  mappings_dir <- file.path(blab_data, 'global_basic_level', 'data')
  output <- add_global_basic_level(all_basiclevel_na, mappings_dir)

  with_global_basic_level <- output[[1]]
  dict_updated <- output[[2]]
  to_fix <- output[[3]]

  hashes <- with_global_basic_level %>%
    mutate(across(where(is.numeric), as.numeric),
           across(where(is.factor), as.character)) %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes <- list(
      ordinal = "2e582ed0c51fae1ada3c9662b141ad08",
      onset = "0625f731aa050546ef0289b2ae56b389",
      offset = "d092130aa7854906e84df8fd4d7a0b0f",
      object = "c310477329cf424ac96a9a0caa66386a",
      utterance_type = "5fbf641a06d847bb73f4768cf7771a82",
      object_present = "128e9d7a686e8c274a40a145cb84293d",
      speaker = "f79b930d9e2d2402a345248158020449",
      basic_level = "c2922cb5e5b1eed99a9c5f7a48c07cb4",
      annotid = "beae0ba0a1ab7911f2e7ec694d2afb69",
      id = "0d5e73858b691d95eb67c8cf23d6ca9e",
      subj = "576c8ad0be33dcd6623ed05659f182a5",
      month = "c62cad2e5d97a87c178b1f1d6f926010",
      SubjectNumber = "bebe952597ccc7179095b39fa833d21c",
      audio_video = "573803020a672ab792dc19002584c5e8",
      tier = "9fd48b913f0a73e2f7eae40223a5881f",
      pho = "c1f1fdde36641ed0bc8b832f4027ba3f",
      disambiguate = "7d98b93357c916fdc0967851c533e29b",
      global_bl = "0d2731d2b07593222025ed9ecb0523d1"
    )
  expect_equal(hashes, expected_hashes)

  hashes_dict_updated <- dict_updated %>%
    mutate(across(where(is.numeric), as.numeric),
           across(where(is.factor), as.character)) %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_dict_updated <- list(
    object = "9079da00132de3805251f085dcd6abee",
    disambiguate = "8312c7f18c6308052a26cea4ddba4f57",
    global_bl = "347b078a4b542f05ecc08b61a46a3ae0")
  expect_equal(hashes_dict_updated, expected_hashes_dict_updated)

  hashes_to_fix <- to_fix %>%
    mutate(across(where(is.numeric), as.numeric),
           across(where(is.factor), as.character)) %>%
    summarise(across(everything(), digest)) %>%
    as.list
  expected_hashes_to_fix <-
    list(object = "753fef2edea1f1d91f3d9580dba19d4c",
         basic_level = "d27782eece28fe34c8e816a29b2bc750",
         global_bl = "b0c9c0e8cd9e2383df6734dc0d1aa029")
  expect_equal(hashes_to_fix, expected_hashes_to_fix)

})
