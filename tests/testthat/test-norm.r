library(testthat)
library(microeco)

context("Normalization functions")

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

test_that("trans_norm$new creates object", {
  mt <- create_test_microtable()
  t_norm <- trans_norm$new(dataset = mt)
  expect_s3_class(t_norm, "trans_norm")
})

test_that("rarefy maintains sample number", {
  mt <- create_test_microtable()
  t_norm <- trans_norm$new(dataset = mt)
  result <- t_norm$norm(method = "rarefy", sample.size = 5, seed = 123)
  
  expect_s3_class(result, "microtable")
  expect_equal(nrow(result$sample_table), nrow(mt$sample_table))
})

test_that("CLR transformation works", {
  mt <- create_test_microtable()
  t_norm <- trans_norm$new(dataset = mt)
  result <- t_norm$norm(method = "CLR")
  
  expect_s3_class(result, "microtable")
})

test_that("TSS normalization works", {
  mt <- create_test_microtable()
  t_norm <- trans_norm$new(dataset = mt)
  result <- t_norm$norm(method = "TSS")
  
  expect_s3_class(result, "microtable")
  col_sums <- colSums(result$otu_table)
  expect_true(all(abs(col_sums - 1) < 0.001))
})

test_that("GMPR normalization works", {
  mt <- create_test_microtable()
  t_norm <- trans_norm$new(dataset = mt)
  result <- t_norm$norm(method = "GMPR")
  
  expect_s3_class(result, "microtable")
})

test_that("sample_sums returns correct values after rarefy", {
  mt <- create_test_microtable()
  t_norm <- trans_norm$new(dataset = mt)
  result <- t_norm$norm(method = "rarefy", sample.size = 5, seed = 123)
  
  sums <- result$sample_sums()
  expect_equal(length(sums), nrow(result$sample_table))
  expect_true(all(sums > 0))
})
