library(testthat)
library(microeco)
library(shiny)

context("Utility functions")

# ===== Helper Functions =====

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

# ===== append_code Tests =====

test_that("append_code adds code with correct format", {
  rv <- reactiveValues(generated_codes = character(0), code_counter = 0L)
  
  append_code(rv, 'x <- 1', "测试")
  expect_equal(rv$code_counter, 1L)
  expect_true(grepl("Step 1", rv$generated_codes[1]))
  expect_true(grepl("测试", rv$generated_codes[1]))
})

test_that("append_code handles empty comment", {
  rv <- reactiveValues(generated_codes = character(0), code_counter = 0L)
  
  append_code(rv, 'y <- 2')
  expect_equal(rv$code_counter, 1L)
  expect_false(grepl("\\|", rv$generated_codes[1]))
})

test_that("append_code increments counter correctly", {
  rv <- reactiveValues(generated_codes = character(0), code_counter = 0L)
  
  append_code(rv, 'x <- 1')
  append_code(rv, 'y <- 2')
  append_code(rv, 'z <- 3')
  
  expect_equal(rv$code_counter, 3L)
  expect_equal(length(rv$generated_codes), 3)
})

# ===== clear_codes Tests =====

test_that("clear_codes resets counter and codes", {
  rv <- reactiveValues(generated_codes = c("code1", "code2"), code_counter = 2L)
  
  clear_codes(rv)
  expect_equal(rv$code_counter, 0L)
  expect_equal(length(rv$generated_codes), 0)
})

test_that("clear_codes handles empty state", {
  rv <- reactiveValues(generated_codes = character(0), code_counter = 0L)
  
  clear_codes(rv)
  expect_equal(rv$code_counter, 0L)
  expect_equal(length(rv$generated_codes), 0)
})

# ===== check_microtable Tests =====

test_that("check_microtable returns FALSE for NULL", {
  rv <- reactiveValues(microtable = NULL)
  expect_false(check_microtable(rv))
})

test_that("check_microtable returns TRUE for valid microtable", {
  mt <- create_test_microtable()
  rv <- reactiveValues(microtable = mt)
  expect_true(check_microtable(rv))
})

# ===== safe_run Tests =====

test_that("safe_run catches errors", {
  result <- safe_run(stop("test error"))
  expect_false(result$success)
  expect_true(!is.null(result$error))
  expect_true(grepl("test error", result$error))
})

test_that("safe_run returns successful result", {
  result <- safe_run(42)
  expect_true(result$success)
  expect_equal(result$result, 42)
})

test_that("safe_run handles warnings", {
  result <- safe_run(warning("test warning"))
  expect_true(result$success)
})

test_that("safe_run returns NULL on error", {
  result <- safe_run(NULL)
  expect_true(result$success)
  expect_null(result$result)
})

# ===== get_sample_cols Tests =====

test_that("get_sample_cols returns correct columns", {
  mt <- create_test_microtable()
  rv <- reactiveValues(microtable = mt)
  
  cols <- get_sample_cols(rv)
  expect_true("Group" %in% cols)
  expect_true("Timepoint" %in% cols)
})

test_that("get_sample_cols handles empty sample_table", {
  otu_table <- matrix(1:4, nrow = 2)
  mt <- microtable$new(
    sample_table = data.frame(row.names = c("S1", "S2")),
    otu_table = otu_table
  )
  rv <- reactiveValues(microtable = mt)
  
  cols <- get_sample_cols(rv)
  expect_equal(length(cols), 0)
})

# ===== get_tax_ranks Tests =====

test_that("get_tax_ranks returns correct ranks", {
  mt <- create_test_microtable()
  rv <- reactiveValues(microtable = mt)
  
  ranks <- get_tax_ranks(rv)
  expect_true("Kingdom" %in% ranks)
  expect_true("Genus" %in% ranks)
  expect_true("Phylum" %in% ranks)
})

test_that("get_tax_ranks handles missing tax_table", {
  otu_table <- matrix(1:4, nrow = 2)
  mt <- microtable$new(
    sample_table = data.frame(Group = c("A", "B"), row.names = c("S1", "S2")),
    otu_table = otu_table
  )
  rv <- reactiveValues(microtable = mt)
  
  ranks <- get_tax_ranks(rv)
  expect_equal(length(ranks), 0)
})

# ===== fmt_pval Tests =====

test_that("fmt_pval formats p-values correctly", {
  expect_equal(fmt_pval(0.05), "0.050")
  expect_equal(fmt_pval(0.001), "0.001")
  expect_equal(fmt_pval(0.0001), "<0.001")
  expect_equal(fmt_pval(1e-10), "<0.001")
  expect_equal(fmt_pval(NA), "NA")
})

# ===== fmt_num Tests =====

test_that("fmt_num formats numbers correctly", {
  expect_equal(fmt_num(1234, 0), "1,234")
  expect_equal(fmt_num(1234.56, 1), "1,234.6")
  expect_equal(fmt_num(0.001234, 4), "0.0012")
  expect_equal(fmt_num(NA, 2), "NA")
})

# ===== format_code_download Tests =====

test_that("format_code_download generates valid R script", {
  codes <- c("x <- 1\n", "y <- 2\n")
  output <- format_code_download(codes)
  expect_true(grepl("library\\(microeco\\)", output))
  expect_true(grepl("library\\(ggplot2\\)", output))
  expect_true(grepl("x <- 1", output))
  expect_true(grepl("y <- 2", output))
  expect_true(grepl("microecoshiny Generated Code", output))
})

# ===== detect_file_roles Tests =====

test_that("detect_file_roles identifies feature table", {
  files <- data.frame(
    name = c("feature-table.tsv", "taxonomy.tsv"),
    datapath = c("/tmp/feature-table.tsv", "/tmp/taxonomy.tsv"),
    stringsAsFactors = FALSE
  )
  roles <- detect_file_roles(files)
  expect_equal(roles$feature, "feature-table.tsv")
})

test_that("detect_file_roles identifies taxonomy", {
  files <- data.frame(
    name = c("feature-table.tsv", "taxonomy.tsv"),
    datapath = c("/tmp/feature-table.tsv", "/tmp/taxonomy.tsv"),
    stringsAsFactors = FALSE
  )
  roles <- detect_file_roles(files)
  expect_equal(roles$taxonomy, "taxonomy.tsv")
})

# ===== generate_import_code Tests =====

test_that("generate_import_code handles rdata type", {
  paths <- list(rdata = "test.RData")
  code <- generate_import_code("rdata", paths, "my_data")
  expect_true(grepl("load\\(", code))
  expect_true(grepl("my_data", code))
})

# ===== remove_taxa_prefixes Tests =====

test_that("remove_taxa_prefixes removes prefixes", {
  tax_table <- data.frame(
    Kingdom = c("d__Bacteria", "d__Archaea"),
    Phylum = c("p__Firmicutes", "p__Proteobacteria"),
    stringsAsFactors = FALSE
  )
  
  result <- remove_taxa_prefixes(tax_table)
  expect_equal(result$Kingdom[1], "Bacteria")
  expect_equal(result$Phylum[1], "Firmicutes")
})
