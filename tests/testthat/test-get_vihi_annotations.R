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
  vi_tdvi_annotations <- get_vihi_annotations(version = version,
                                              subset = 'VI+TD-VI',
                                              allow_annotation_errors = TRUE,
                                              include_pi = TRUE) %>%
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

  expect_equal(dim(vi_tdvi_annotations), c(28845, 12))
  expect_column_contents(
    vi_tdvi_annotations,
    list(
      eaf_filename = "c1d377845d0dd9f2cab4b423a9005411",
      participant = "ef351bfacc3593b5830f63859b5faa54",
      onset = "8ebae3e7b6ee8b15e39c34f7bd6c5af4",
      offset = "04954d408e58a40171b2ccd62006c078",
      transcription = "5e00f8ace8940cc98c4ef462bfba10ed",
      transcription_id = "dcebf75854d0915876c6ecd6a0f0d54d",
      mwu = "5101eef6a8da05fa8ce1daf3461875e3",
      lex = "27e2166712ebefd378cf70b0d4628f8c",
      vcm = "7af82950ffa88699046ba434ace3a862",
      xds = "c55805021d3db0eaa01c0dbdb793209f",
      code_num = "06fcbc1880531afc16eb462eb221c9ef",
      PI = "4e853a0600055da784adbbd6fe242f6f"
    )
  )

})
