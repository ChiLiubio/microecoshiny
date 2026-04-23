library(testthat)
library(microeco)

context("Environmental correlation analysis")

test_that("trans_env$new creates object", {
  mt <- create_test_microtable()
  t_env <- trans_env$new(dataset = mt, env_cols = c("pH", "Temperature"))
  expect_s3_class(t_env, "trans_env")
})

test_that("trans_env$new with standardize", {
  mt <- create_test_microtable()
  t_env <- trans_env$new(dataset = mt, env_cols = c("pH", "Temperature"), standardize = TRUE)
  expect_s3_class(t_env, "trans_env")
})

test_that("cal_cor works", {
  mt <- create_test_microtable()
  t_env <- trans_env$new(dataset = mt, env_cols = c("pH", "Temperature"))
  t_env$cal_cor()
  expect_true(!is.null(t_env$data_cor))
})

test_that("plot_cor works", {
  mt <- create_test_microtable()
  t_env <- trans_env$new(dataset = mt, env_cols = c("pH", "Temperature"))
  t_env$cal_cor()
  expect_s3_class(t_env$plot_cor(), "gg")
})

test_that("env works with real protocol data", {
  skip_if_not(has_example_data(), "Real test data not available")
  mt <- create_real_microtable()
  sample_cols <- colnames(mt$sample_table)
  env_cols <- head(sample_cols, min(3, length(sample_cols)))
  t_env <- trans_env$new(dataset = mt, env_cols = env_cols)
  expect_s3_class(t_env, "trans_env")
})
