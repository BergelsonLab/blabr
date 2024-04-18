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
    select(-starts_with('error_'))

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

  expect_equal(dim(vi_tdvi_annotations), c(28788, 12))
  expect_column_contents(
    vi_tdvi_annotations,
    list(eaf_filename = "c423f61580f79aac9ee52dec24bbb86b",
         participant = "d4bd9c8395ec83e48b0d0924a0c97684",
         onset = "9378e0f82e76f60c210cefdf0a0fabce",
         offset = "695181900e6be31f42d364da76c72e13",
         transcription = "d3155f5cc79c942f13abbba109868ac4",
         transcription_id = "b70316acd93e615d0cd37bc9690a6616",
         mwu = "084eec7a98c4f9e04b5cdd00cf9fc8b3",
         lex = "7c75095882e2c3601acb663283740f1d",
         vcm = "cfb3fbd0a35aa39f4dc8c4452256b172",
         xds = "c4a40767c30386cd640b8a6c394a483c",
         code_num = "bfaa7e515a31d0b6213f414fd580c24a",
         PI = "38c876f5349f88366a475315c0109ddf")
  )

})
