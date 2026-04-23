library(testthat)
library(microeco)

context("Core microbiome analysis")

test_that("trans_venn$new creates object with merged samples", {
  mt <- create_test_microtable()
  mt_merged <- mt$merge_samples(group = "Group")
  mt_merged$tidy_dataset()
  
  t_venn <- trans_venn$new(dataset = mt_merged, ratio = "numratio")
  expect_s3_class(t_venn, "trans_venn")
})

test_that("trans_venn$new with ratio parameter", {
  mt <- create_test_microtable()
  mt_merged <- mt$merge_samples(group = "Group")
  mt_merged$tidy_dataset()
  
  t_venn <- trans_venn$new(dataset = mt_merged, ratio = "seqratio")
  expect_s3_class(t_venn, "trans_venn")
})

test_that("plot_bar works for venn", {
  mt <- create_test_microtable()
  mt_merged <- mt$merge_samples(group = "Group")
  mt_merged$tidy_dataset()
  
  t_venn <- trans_venn$new(dataset = mt_merged, ratio = "numratio")
  p <- t_venn$plot_bar()
  expect_s3_class(p, "ggplot")
})

test_that("data_details is populated", {
  mt <- create_test_microtable()
  expect_true(is.data.frame(mt$otu_table))
  expect_true(nrow(mt$otu_table) > 0)
})

test_that("data_summary is populated", {
  mt <- create_test_microtable()
  expect_true(is.data.frame(mt$sample_table))
  expect_true(nrow(mt$sample_table) > 0)
})

test_that("core microbiome works with real protocol data", {
  skip_if_not(has_example_data(), "Real test data not available")
  
  mt <- create_real_microtable()
  mt_merged <- mt$merge_samples(group = "Group")
  mt_merged$tidy_dataset()
  
  t_venn <- trans_venn$new(dataset = mt_merged, ratio = "numratio")
  expect_s3_class(t_venn, "trans_venn")
  p <- t_venn$plot_bar()
  expect_s3_class(p, "ggplot")
})
