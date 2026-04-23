library(testthat)
library(microeco)

context("Differential abundance analysis")

test_that("trans_diff$new with wilcox method", {
  mt <- create_test_microtable()
  t_diff <- trans_diff$new(dataset = mt, method = "wilcox", group = "Group")
  expect_s3_class(t_diff, "trans_diff")
  t_diff$cal_diff()
  expect_true(!is.null(t_diff$data_stat))
})

test_that("trans_diff$new with KW method", {
  mt <- create_test_microtable()
  t_diff <- trans_diff$new(dataset = mt, method = "KW", group = "Group")
  expect_s3_class(t_diff, "trans_diff")
  t_diff$cal_diff()
  expect_true(!is.null(t_diff$data_stat))
})

test_that("trans_diff$new with LEfSe method", {
  skip("LEfSe requires additional dependencies")
})

test_that("plot_diff_bar works", {
  mt <- create_test_microtable()
  t_diff <- trans_diff$new(dataset = mt, method = "wilcox", group = "Group")
  t_diff$cal_diff()
  expect_s3_class(t_diff$plot_diff_bar(), "gg")
})

test_that("plot_diff_heatmap works", {
  mt <- create_test_microtable()
  t_diff <- trans_diff$new(dataset = mt, method = "wilcox", group = "Group")
  t_diff$cal_diff()
  expect_s3_class(t_diff$plot_diff_heatmap(), "gg")
})

test_that("diff analysis works with real protocol data", {
  skip_if_not(has_example_data(), "Real test data not available")
  
  mt <- create_real_microtable()
  t_diff <- trans_diff$new(dataset = mt, method = "wilcox", group = "Group")
  t_diff$cal_diff()
  expect_s3_class(t_diff, "trans_diff")
  expect_s3_class(t_diff$plot_diff_bar(), "gg")
})
