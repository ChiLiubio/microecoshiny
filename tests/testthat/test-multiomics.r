library(testthat)
library(microeco)

context("Multi-omics analysis")

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

test_that("trans_metab$new creates object with matrix", {
  mt <- create_test_microtable()
  metab <- matrix(runif(12), nrow = 3, ncol = 4)
  colnames(metab) <- paste0("Metab", 1:4)
  rownames(metab) <- rownames(mt$sample_table)
  
  t_metab <- trans_metab$new(metab = metab, microb = mt)
  expect_s3_class(t_metab, "trans_metab")
})

test_that("trans_metab$new creates object with data.frame", {
  mt <- create_test_microtable()
  metab <- data.frame(
    MetaboliteA = c(0.1, 0.2, 0.15),
    MetaboliteB = c(0.05, 0.08, 0.06),
    row.names = rownames(mt$sample_table)
  )
  
  t_metab <- trans_metab$new(metab = metab, microb = mt)
  expect_s3_class(t_metab, "trans_metab")
})

test_that("trans_metab$new handles numeric metab data", {
  mt <- create_test_microtable()
  metab <- matrix(runif(12), nrow = 3, ncol = 4)
  rownames(metab) <- rownames(mt$sample_table)
  colnames(metab) <- paste0("M", 1:4)
  
  t_metab <- trans_metab$new(metab = metab, microb = mt)
  expect_true(!is.null(t_metab$data_metab))
})
