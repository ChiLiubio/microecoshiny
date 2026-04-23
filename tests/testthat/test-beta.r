library(testthat)
library(microeco)

context("Beta diversity functions")

test_that("trans_beta$new with bray distance", {
  mt <- create_test_microtable()
  mt$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt, measure = "bray", group = "Group")
  expect_s3_class(t_beta, "trans_beta")
})

test_that("trans_beta$new with jaccard distance", {
  mt <- create_test_microtable()
  mt$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt, measure = "jaccard", group = "Group")
  expect_s3_class(t_beta, "trans_beta")
})

test_that("cal_ordination with PCoA", {
  mt <- create_test_microtable()
  mt$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt, measure = "bray", group = "Group")
  t_beta$cal_ordination(method = "PCoA")
  expect_true(!is.null(t_beta$res_ordination))
})

test_that("cal_ordination with NMDS", {
  mt <- create_test_microtable()
  mt$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt, measure = "bray", group = "Group")
  t_beta$cal_ordination(method = "NMDS")
  expect_true(!is.null(t_beta$res_ordination))
})

test_that("plot_ordination works", {
  mt <- create_test_microtable()
  mt$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt, measure = "bray", group = "Group")
  t_beta$cal_ordination(method = "PCoA")
  expect_s3_class(t_beta$plot_ordination(), "gg")
})

test_that("cal_anosim works", {
  mt <- create_test_microtable()
  mt$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt, measure = "bray", group = "Group")
  t_beta$cal_ordination(method = "PCoA")
  t_beta$cal_anosim()
  expect_true(!is.null(t_beta$res_anosim))
})

test_that("cal_group_distance works", {
  mt <- create_test_microtable()
  mt$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt, measure = "bray", group = "Group")
  t_beta$cal_group_distance()
  expect_true(!is.null(t_beta$res_group_distance))
})

test_that("beta diversity works with real protocol data", {
  skip_if_not(has_example_data(), "Real test data not available")
  mt <- create_real_microtable()
  mt$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt, measure = "bray", group = "Group")
  expect_s3_class(t_beta, "trans_beta")
  t_beta$cal_ordination(method = "PCoA")
  expect_s3_class(t_beta$plot_ordination(), "gg")
})
