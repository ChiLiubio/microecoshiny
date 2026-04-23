library(testthat)
library(microeco)

context("Functional prediction")

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

test_that("trans_func$new creates object", {
  mt <- create_test_microtable()
  t_func <- trans_func$new(dataset = mt)
  expect_s3_class(t_func, "trans_func")
})

test_that("trans_func$new sets for_what correctly", {
  mt <- create_test_microtable()
  t_func <- trans_func$new(dataset = mt)
  expect_equal(t_func$for_what, "prok")
})

test_that("cal_func with FAPROTAX", {
  mt <- create_test_microtable()
  t_func <- trans_func$new(dataset = mt)
  t_func$cal_func(prok_database = "FAPROTAX")
  expect_true(!is.null(t_func$res_function))
})

test_that("res_function is data.frame", {
  mt <- create_test_microtable()
  t_func <- trans_func$new(dataset = mt)
  t_func$cal_func(prok_database = "FAPROTAX")
  expect_s3_class(t_func$res_function, "data.frame")
})
