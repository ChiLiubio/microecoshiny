library(testthat)
library(microeco)

context("Alpha diversity functions")

test_that("trans_alpha$new calculates diversity", {
  mt <- create_test_microtable()
  t_alpha <- trans_alpha$new(dataset = mt, group = "Group")
  expect_s3_class(t_alpha, "trans_alpha")
  expect_true(!is.null(t_alpha$data_alpha))
})

test_that("trans_alpha without group parameter", {
  mt <- create_test_microtable()
  t_alpha <- trans_alpha$new(dataset = mt)
  expect_s3_class(t_alpha, "trans_alpha")
  expect_true(!is.null(t_alpha$data_alpha))
})

test_that("cal_diff with wilcox method", {
  mt <- create_test_microtable()
  t_alpha <- trans_alpha$new(dataset = mt, group = "Group")
  t_alpha$cal_diff(method = "wilcox")
  expect_true(!is.null(t_alpha$data_stat))
})

test_that("cal_diff with KW method", {
  mt <- create_test_microtable()
  t_alpha <- trans_alpha$new(dataset = mt, group = "Group")
  t_alpha$cal_diff(method = "KW")
  expect_true(!is.null(t_alpha$data_stat))
})

test_that("plot_alpha works with group", {
  mt <- create_test_microtable()
  t_alpha <- trans_alpha$new(dataset = mt, group = "Group")
  expect_s3_class(t_alpha$plot_alpha(), "gg")
})

test_that("alpha diversity includes multiple measures", {
  mt <- create_test_microtable()
  t_alpha <- trans_alpha$new(dataset = mt)
  measures <- unique(t_alpha$data_alpha$Measure)
  expect_true("Shannon" %in% measures)
  expect_true("Observed" %in% measures)
})

test_that("alpha diversity works with real protocol data", {
  skip_if_not(has_example_data(), "Real test data not available")
  mt <- create_real_microtable()
  t_alpha <- trans_alpha$new(dataset = mt, group = "Group")
  expect_s3_class(t_alpha, "trans_alpha")
  expect_true(!is.null(t_alpha$data_alpha))
  expect_s3_class(t_alpha$plot_alpha(), "gg")
})
