library(testthat)
library(microeco)

context("Complete analysis workflow")

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

test_that("full pipeline: import -> preprocess -> normalize -> analyze", {
  # Step 1: Create test data
  mt <- create_test_microtable()
  expect_s3_class(mt, "microtable")
  
  # Step 2: Preprocess - filter pollution
  mt$filter_pollution(taxa = c("mitochondria", "chloroplast"))
  mt$tidy_dataset()
  expect_s3_class(mt, "microtable")
  
  # Step 3: Normalize
  t_norm <- trans_norm$new(dataset = mt)
  mt_norm <- t_norm$norm(method = "rarefy", sample.size = 5, seed = 123)
  expect_s3_class(mt_norm, "microtable")
  
  # Step 4: Alpha diversity
  t_alpha <- trans_alpha$new(dataset = mt_norm, group = "Group")
  expect_true(!is.null(t_alpha$data_alpha))
  
  # Step 5: Beta diversity
  mt_norm$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt_norm, measure = "bray", group = "Group")
  t_beta$cal_ordination(method = "PCoA")
  expect_true(!is.null(t_beta$res_ordination))
  
  # Step 6: Composition
  t_abund <- trans_abund$new(dataset = mt_norm, taxrank = "Phylum", ntaxa = 3)
  expect_s3_class(t_abund, "trans_abund")
})

test_that("full pipeline: normalize with CLR -> alpha -> beta", {
  mt <- create_test_microtable()
  
  # Normalize with CLR
  t_norm <- trans_norm$new(dataset = mt)
  mt_norm <- t_norm$norm(method = "CLR")
  
  # Alpha diversity
  t_alpha <- trans_alpha$new(dataset = mt_norm, group = "Group")
  expect_true(!is.null(t_alpha$data_alpha))
  
  # Beta diversity
  mt_norm$cal_betadiv()
  t_beta <- trans_beta$new(dataset = mt_norm, measure = "bray", group = "Group")
  t_beta$cal_ordination(method = "PCoA")
  expect_true(!is.null(t_beta$res_ordination))
})

test_that("workspace save and load roundtrip", {
  mt <- create_test_microtable()
  
  # Save to temp file
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  saveRDS(mt, temp_file)
  
  # Load
  mt_loaded <- readRDS(temp_file)
  
  expect_equal(nrow(mt$otu_table), nrow(mt_loaded$otu_table))
  expect_equal(ncol(mt$otu_table), ncol(mt_loaded$otu_table))
  expect_equal(colnames(mt$tax_table), colnames(mt_loaded$tax_table))
})

test_that("multiple normalization methods produce valid microtable", {
  mt <- create_test_microtable()
  
  methods <- c("rarefy", "CLR", "TSS")
  
  for (method in methods) {
    if (method == "rarefy") {
      t_norm <- trans_norm$new(dataset = mt)
      result <- t_norm$norm(method = method, sample.size = 3, seed = 123)
    } else {
      t_norm <- trans_norm$new(dataset = mt)
      result <- t_norm$norm(method = method)
    }
    
    expect_s3_class(result, "microtable")
    expect_true(nrow(result$otu_table) > 0)
    expect_true(ncol(result$otu_table) > 0)
  }
})
