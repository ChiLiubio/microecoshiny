library(testthat)
library(microeco)

context("Machine learning classification")

create_test_microtable <- function() {
  otu_table <- matrix(c(10, 5, 3, 2, 8, 4, 1, 0, 15, 7, 2, 1), nrow = 3, byrow = TRUE)
  colnames(otu_table) <- paste0("OTU", 1:4)
  rownames(otu_table) <- paste0("Sample", 1:3)
  
  tax_table <- data.frame(
    Kingdom = rep("Bacteria", 4),
    Phylum = c("Firmicutes", "Bacteroidetes", "Proteobacteria", "Actinobacteria"),
    Class = c("Bacilli", "Bacteroidia", "Gammaproteobacteria", "Actinobacteria"),
    Order = c("Lactobacillales", "Bacteroidales", "Enterobacterales", "Bifidobacteriales"),
    Family = c("Lactobacillaceae", "Bacteroidaceae", "Enterobacteriaceae", "Bifidobacteriaceae"),
    Genus = c("Lactobacillus", "Bacteroides", "Escherichia", "Bifidobacterium"),
    stringsAsFactors = FALSE
  )
  rownames(tax_table) <- paste0("OTU", 1:4)
  
  sample_table <- data.frame(
    Group = c("Control", "Treatment", "Treatment"),
    Timepoint = c("T1", "T1", "T2"),
    stringsAsFactors = FALSE
  )
  rownames(sample_table) <- paste0("Sample", 1:3)
  
  microtable$new(sample_table = sample_table, otu_table = otu_table, tax_table = tax_table)
}

test_that("trans_classifier$new creates object", {
  mt <- create_test_microtable()
  t_cl <- trans_classifier$new(dataset = mt, y.response = "Group")
  expect_s3_class(t_cl, "trans_classifier")
})

test_that("trans_classifier$new requires y.response", {
  mt <- create_test_microtable()
  expect_error(trans_classifier$new(dataset = mt), "y.response")
})

test_that("cal_split works", {
  mt <- create_test_microtable()
  t_cl <- trans_classifier$new(dataset = mt, y.response = "Group")
  t_cl$cal_split(ratio = 0.7)
  expect_true(!is.null(t_cl$data_train))
})

test_that("cal_train with rf method", {
  mt <- create_test_microtable()
  t_cl <- trans_classifier$new(dataset = mt, y.response = "Group")
  t_cl$cal_split(ratio = 0.7)
  t_cl$cal_train(method = "rf", cv.fold = 3)
  expect_true(!is.null(t_cl$model))
})

test_that("cal_train with svm method", {
  mt <- create_test_microtable()
  t_cl <- trans_classifier$new(dataset = mt, y.response = "Group")
  t_cl$cal_split(ratio = 0.7)
  t_cl$cal_train(method = "svm", cv.fold = 3)
  expect_true(!is.null(t_cl$model))
})

test_that("cal_predict works", {
  mt <- create_test_microtable()
  t_cl <- trans_classifier$new(dataset = mt, y.response = "Group")
  t_cl$cal_split(ratio = 0.7)
  t_cl$cal_train(method = "rf", cv.fold = 3)
  t_cl$cal_predict()
  expect_true(!is.null(t_cl$res_predict))
})

test_that("cal_ROC works", {
  mt <- create_test_microtable()
  t_cl <- trans_classifier$new(dataset = mt, y.response = "Group")
  t_cl$cal_split(ratio = 0.7)
  t_cl$cal_train(method = "rf", cv.fold = 3)
  t_cl$cal_predict()
  t_cl$cal_ROC()
  expect_true(!is.null(t_cl$res_ROC))
})

test_that("plot_confusionMatrix works", {
  mt <- create_test_microtable()
  t_cl <- trans_classifier$new(dataset = mt, y.response = "Group")
  t_cl$cal_split(ratio = 0.7)
  t_cl$cal_train(method = "rf", cv.fold = 3)
  t_cl$cal_predict()
  expect_s3_class(t_cl$plot_confusionMatrix(), "gg")
})
