library(testthat)

context("Configuration tests")

# Source the config file
config_env <- new.env()
source(file.path(dirname(dirname(getwd())), "R/app_config.r"), local = config_env)
app_config <- config_env$app_config

test_that("app_config exists and has required fields", {
  expect_true(is.list(app_config))
  expect_true(!is.null(app_config$app_name))
  expect_true(!is.null(app_config$app_version))
  expect_true(!is.null(app_config$app_title))
})

test_that("app_config has valid taxranks", {
  expect_true(is.character(app_config$taxranks))
  expect_true("Kingdom" %in% app_config$taxranks)
  expect_true("Genus" %in% app_config$taxranks)
})

test_that("app_config has valid norm_methods", {
  expect_true(is.character(app_config$norm_methods))
  expect_true("rarefy" %in% app_config$norm_methods)
  expect_true("CLR" %in% app_config$norm_methods)
})

test_that("app_config has valid diff_methods", {
  expect_true(is.character(app_config$diff_methods))
  expect_true("wilcox" %in% app_config$diff_methods)
  expect_true("DESeq2" %in% app_config$diff_methods)
})

test_that("app_config has valid beta_measures", {
  expect_true(is.character(app_config$beta_measures))
  expect_true("bray" %in% app_config$beta_measures)
  expect_true("jaccard" %in% app_config$beta_measures)
})

test_that("app_config has valid cor_methods", {
  expect_true(is.character(app_config$cor_methods))
  expect_true("spearman" %in% app_config$cor_methods)
  expect_true("pearson" %in% app_config$cor_methods)
})

test_that("app_config has modules list", {
  expect_true(is.list(app_config$modules))
  expect_true(!is.null(app_config$modules$import))
  expect_true(!is.null(app_config$modules$preprocess))
  expect_true(!is.null(app_config$modules$norm))
})

test_that("app_config max_upload_size is numeric", {
  expect_true(is.numeric(app_config$max_upload_size))
  expect_true(app_config$max_upload_size > 0)
})

test_that("app_config colors_palette has valid colors", {
  expect_true(is.character(app_config$colors_palette))
  expect_true(length(app_config$colors_palette) >= 8)
})
