test_that("theme functions work", {
  expect_s3_class(theme_blab(), "theme")
  expect_s3_class(theme_spooky(), "theme")
  expect_s3_class(theme_AMERICA(), "theme")
})

test_that("theme function result in the same plot as before", {
  # TODO: Add relevant elements to this plot. It doesn't have many that the
  # themes affect
  histogram <- ggplot(mtcars, aes(disp)) + geom_histogram(bins = 30)

  vdiffr::expect_doppelganger("plot with theme_blab", histogram + theme_blab())
  vdiffr::expect_doppelganger("plot with theme_spooky",
                              histogram + theme_spooky())
  vdiffr::expect_doppelganger("plot with theme_AMERICA",
                              histogram + theme_AMERICA())
})
