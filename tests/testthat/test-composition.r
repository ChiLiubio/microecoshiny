library(testthat)
library(microeco)

context("Taxonomic composition functions")

test_that("trans_abund$new creates object at Phylum level", {
  mt <- create_test_microtable()
  t_abund <- trans_abund$new(dataset = mt, taxrank = "Phylum", ntaxa = 3)
  expect_s3_class(t_abund, "trans_abund")
})

test_that("trans_abund$new creates object at Genus level", {
  mt <- create_test_microtable()
  t_abund <- trans_abund$new(dataset = mt, taxrank = "Genus", ntaxa = 3)
  expect_s3_class(t_abund, "trans_abund")
})

test_that("plot_bar works without group", {
  mt <- create_test_microtable()
  t_abund <- trans_abund$new(dataset = mt, taxrank = "Phylum", ntaxa = 3)
  expect_s3_class(t_abund$plot_bar(), "gg")
})

test_that("plot_bar works with groupmean", {
  mt <- create_test_microtable()
  t_abund <- trans_abund$new(
    dataset = mt,
    taxrank = "Phylum",
    ntaxa = 3,
    groupmean = "Group"
  )
  expect_s3_class(t_abund$plot_bar(), "gg")
})

test_that("plot_heatmap works", {
  mt <- create_test_microtable()
  t_abund <- trans_abund$new(dataset = mt, taxrank = "Phylum", ntaxa = 3)
  expect_s3_class(t_abund$plot_heatmap(), "gg")
})

test_that("trans_abund handles show parameter", {
  mt <- create_test_microtable()
  t_abund <- trans_abund$new(dataset = mt, taxrank = "Phylum", ntaxa = 3, show = 0.01)
  expect_s3_class(t_abund, "trans_abund")
})

test_that("composition works with real protocol data", {
  skip_if_not(has_example_data(), "Real test data not available")
  mt <- create_real_microtable()
  t_abund <- trans_abund$new(dataset = mt, taxrank = "Phylum", ntaxa = 5)
  expect_s3_class(t_abund, "trans_abund")
  expect_s3_class(t_abund$plot_bar(), "gg")
})
