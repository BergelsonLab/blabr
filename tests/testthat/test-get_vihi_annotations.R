version <- '0.0.0.9006-dev.5'


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

  expect_non_empty_dataframe(
    get_vihi_annotations(version = version,
                         subset = 'everything',
                         table = 'merged',
                         allow_annotation_errors = TRUE))

  annotations <- get_vihi_annotations(version = version,
                                      allow_annotation_errors = TRUE)

  annotations_with_all_tier_types <-
    get_vihi_annotations(version = version,
                         include_all_tier_types = TRUE,
                         allow_annotation_errors = TRUE)
  expect_equal(annotations_with_all_tier_types %>%
                 dplyr::select(dplyr::all_of(colnames(annotations))),
               annotations)

})

test_that("VI+TD-VI subset looks right", {
  vi_tdvi_annotations <-
    get_vihi_annotations(
      version = version,
      subset = 'VI+TD-VI')

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
         mwu = "ce2a786c14093bc8d92e573a404a3ad8",
         lex = "8f8c2bdc6d8675608a7bdd48b8835d2a",
         vcm = "472fc875df5766634cbfb0b5bfa1c050",
         xds = "38c9b3dcdb7ab1b86f59024f3ce6e8fe",
         code_num = "532bd4ec6f0050aa2bc4f69cbae337d7",
         PI = "9cb50da225a187662ec3ec142f68f3a0")
  )

})
