test_that("correlation produces correct results", {
  corr <- correlation(1:10, 1:10)
  expect_s3_class(corr, "matrix")
  expect_s3_class(corr, "Correlation")

  expect_equal(nrow(corr), 2)
  expect_equal(ncol(corr), 2)

  expect_equivalent(as.numeric(corr), rep(1, 4))
  rm(corr)

  expect_error(correlation("text", 1:10), "'x' must be numeric", fixed = TRUE)
  expect_error(correlation(1:10, "text"), "'y' must be numeric", fixed = TRUE)
})
