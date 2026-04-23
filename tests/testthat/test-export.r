library(testthat)
library(microecoshiny)

context("Data export")

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

test_that("format_code_download generates valid R script", {
  codes <- c("x <- 1\n", "y <- 2\n")
  output <- format_code_download(codes)
  expect_true(grepl("library\\(microeco\\)", output))
  expect_true(grepl("library\\(ggplot2\\)", output))
  expect_true(grepl("x <- 1", output))
  expect_true(grepl("microecoshiny Generated Code", output))
})

test_that("format_code_download includes timestamp", {
  codes <- c("x <- 1\n")
  output <- format_code_download(codes)
  expect_true(grepl("Generated:", output))
})

test_that("workspace_save saves microtable to file", {
  mt <- create_test_microtable()
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  workspace_save(mt, temp_file)
  expect_true(file.exists(temp_file))
  
  loaded <- readRDS(temp_file)
  expect_s3_class(loaded, "microtable")
})

test_that("workspace_save saves to RData format", {
  mt <- create_test_microtable()
  temp_file <- tempfile(fileext = ".RData")
  on.exit(unlink(temp_file))
  
  workspace_save(mt, temp_file, format = "RData")
  expect_true(file.exists(temp_file))
})
