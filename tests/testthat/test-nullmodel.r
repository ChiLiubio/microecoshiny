library(testthat)
library(microeco)

context("Null model analysis")

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

test_that("trans_nullmodel$new requires phylo_tree", {
  mt <- create_test_microtable()
  expect_error(trans_nullmodel$new(dataset = mt), "phylo_tree")
})

test_that("trans_nullmodel$new with filter_thres", {
  mt <- create_test_microtable()
  t_null <- trans_nullmodel$new(dataset = mt, filter_thres = 0.001)
  expect_s3_class(t_null, "trans_nullmodel")
})

test_that("trans_nullmodel$new with group parameter", {
  mt <- create_test_microtable()
  t_null <- trans_nullmodel$new(dataset = mt, group = "Group", measure = "bray")
  expect_s3_class(t_null, "trans_nullmodel")
})
