test_that("the main table loads at all", {
  for (dataset in datasets) {
    expect_non_empty_dataframe(get_seedlings_nouns())
  }
})

test_that("get_seedlings_nouns and friends work", {
  versions_to_test <- c('0.0.0.9016',  # a dev version
                        'v1.0.0')  # a public version
  for (version in versions_to_test) {
    for (table in c('seedlings-nouns', 'regions', 'recordings',
                    'sub-recordings')) {
      if (table == 'seedlings-nouns') {
        get_table <- function() {
          get_seedlings_nouns(version = version)
        }}
      else
      {get_table <- function() {
        get_seedlings_nouns_extra(version = version, table = table)
      }}

      expect_non_empty_dataframe(get_table())
      expect_non_empty_dataframe(
        get_seedlings_nouns_codebook(version = version))
    }
  }
})
