library(dplyr)
library(fs)

version <- '0.0.0.9006-dev.2'


test_that("get_vihi_annotations works if errors are allowed", {
  for (table in c('annotations', 'intervals', 'merged')) {
    expect_non_empty_dataframe(
      get_vihi_annotations(version = version,
                           table = table,
                           allow_annotation_errors = TRUE))
  }

  tables <- get_vihi_annotations(version = version, table = 'all',
                                 allow_annotation_errors = TRUE)
  for (table in tables) {
    expect_non_empty_dataframe(table)
  }

  for (subset in c('everything', 'VI+TD-VI')) {
    expect_non_empty_dataframe(
      get_vihi_annotations(version = version,
                           subset = subset,
                           table = 'merged',
                           allow_annotation_errors = TRUE))
  }

  annotations <- get_vihi_annotations(version = version,
                                      allow_annotation_errors = TRUE)

  annotations_with_all_tier_types <-
    get_vihi_annotations(version = version,
                         include_all_tier_types = TRUE,
                         allow_annotation_errors = TRUE)
  expect_equal(annotations_with_all_tier_types %>%
                 select(all_of(colnames(annotations))),
               annotations)

})

test_that("VI+TD-VI subset looks right", {
  vi_tdvi_annotations <-
    get_vihi_annotations(
      version = version,
      subset = 'VI+TD-VI',
      allow_annotation_errors = TRUE) %>%
    select(-starts_with('error_')) %>%
    filter(!transcription_id %in% c('a6561', 'a6579', 'a6637'))

  vi_and_td_matches <- c(
    'VI_001_676', 'TD_436_678',
    'VI_002_336', 'TD_443_341',
    'VI_003_405', 'TD_444_402',
    'VI_004_415', 'TD_447_448',
    'VI_005_263', 'TD_448_304',
    'VI_005_411', 'TD_467_433',
    'VI_007_204', 'TD_445_217',
    'VI_008_707', 'TD_449_716',
    'VI_012_562', 'TD_465_541',
    'VI_013_206', 'TD_477_217',
    'VI_014_242', 'TD_463_254',
    'VI_015_195', 'TD_464_188',
    'VI_018_924', 'TD_474_966',
    'VI_033_820', 'TD_472_829',
    'VI_037_868', 'TD_473_844',
    'VI_047_440', 'TD_475_481')
  expect_setequal(
    fs::path_ext_remove(vi_tdvi_annotations$eaf_filename),
    vi_and_td_matches)

  expect_equal(dim(vi_tdvi_annotations), c(28785, 12))
  expect_column_contents(
    vi_tdvi_annotations,
    list(eaf_filename = "ab6814bd71f1e93b5ab1f5ab2b9b621e",
         participant = "c6e6400aec0ae88129ef5ba32370b762",
         onset = "2556ceaa5c9400b34d5d56aea7a3ab8e",
         offset = "89f145584be56680adf8f1c870bdc67c",
         transcription = "2522dc8cd14820168ec7c04e3de1bfe6",
         transcription_id = "588885b02a97f30f3e574e3e2850c97a",
         mwu = "216a6f3148b9df9c7628442b70342637",
         lex = "4374d72eb6f2403bfa6bd55a9eaf8896",
         vcm = "0d44c4c5847cbce72959b7e2b4a62d9f",
         xds = "40e4699660aed444b03a1aea5d2dd758",
         code_num = "532bd4ec6f0050aa2bc4f69cbae337d7",
         PI = "9cb50da225a187662ec3ec142f68f3a0")
  )

})
