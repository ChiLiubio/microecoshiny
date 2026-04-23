test_that("data import functions work correctly", {
  identify_file_type <- function(filepath, filename) {
    ext <- tolower(tools::file_ext(filename))
    if (ext == "qza") return("qza")
    if (ext %in% c("rdata", "rds")) return("rdata")
    if (ext %in% c("nwk", "tree", "newick")) return("tree")
    if (ext %in% c("fasta", "fa", "fna")) return("fasta")
    return("unknown_table")
  }

  expect_equal(identify_file_type("test.qza", "test.qza"), "qza")
  expect_equal(identify_file_type("test.RData", "test.RData"), "rdata")
  expect_equal(identify_file_type("test.rds", "test.rds"), "rdata")
  expect_equal(identify_file_type("test.nwk", "tree.nwk"), "tree")
  expect_equal(identify_file_type("test.fasta", "seqs.fasta"), "fasta")
  expect_equal(identify_file_type("test.csv", "data.csv"), "unknown_table")
})

test_that("microtable can be created from test data", {
  mt <- create_test_microtable()
  expect_s3_class(mt, "microtable")
  expect_true(!is.null(mt$otu_table))
  expect_true(!is.null(mt$tax_table))
  expect_true(!is.null(mt$sample_table))
  expect_equal(nrow(mt$otu_table), 4)
  expect_equal(ncol(mt$otu_table), 3)
})

test_that("microtable can be created from real protocol data", {
  skip_if_not(has_example_data(), "Real test data not available")

  mt <- create_real_microtable()
  expect_s3_class(mt, "microtable")
  expect_true(!is.null(mt$otu_table))
  expect_true(!is.null(mt$tax_table))
  expect_true(!is.null(mt$sample_table))
  expect_true(!is.null(mt$phylo_tree))
  expect_true(!is.null(mt$rep_fasta))
  expect_true(is.numeric(mt$otu_table[1, 1]))
})

test_that("microtable can be created from Excel/TSV auto-import simulation", {
  skip_if_not(has_example_data(), "Real test data not available")

  mt <- create_microtable_from_excel()
  expect_s3_class(mt, "microtable")
  expect_true(!is.null(mt$otu_table))
  expect_true(is.data.frame(mt$otu_table) || is.matrix(mt$otu_table))
  expect_true(all(sapply(mt$otu_table, is.numeric)) || is.numeric(as.matrix(mt$otu_table)))
})

test_that("append_code works correctly", {
  rv <- reactiveValues(generated_codes = character(0), code_counter = 0L)
  append_code(rv, 'x <- 1', "\u6d4b\u8bd5")
  expect_equal(rv$code_counter, 1L)
  expect_true(length(rv$generated_codes) == 1)
  expect_true(grepl("Step 1", rv$generated_codes[1]))
  expect_true(grepl("\u6d4b\u8bd5", rv$generated_codes[1]))

  append_code(rv, 'y <- 2', "\u6d4b\u8bd52")
  expect_equal(rv$code_counter, 2L)
  expect_true(length(rv$generated_codes) == 2)
})

test_that("clear_codes works correctly", {
  rv <- reactiveValues(generated_codes = c("code1", "code2"), code_counter = 2L)
  clear_codes(rv)
  expect_equal(rv$code_counter, 0L)
  expect_equal(length(rv$generated_codes), 0)
})

test_that("check_microtable works correctly", {
  rv_empty <- reactiveValues(microtable = NULL)
  expect_false(check_microtable(rv_empty))

  mt <- create_test_microtable()
  rv_valid <- reactiveValues(microtable = mt)
  expect_true(check_microtable(rv_valid))
})

test_that("safe_run catches errors", {
  result <- safe_run(stop("test error"))
  expect_false(result$success)
  expect_true(!is.null(result$error))

  result2 <- safe_run(42)
  expect_true(result2$success)
  expect_equal(result2$result, 42)
})

test_that("format_code_download generates valid R script", {
  codes <- c("x <- 1\n", "y <- 2\n")
  output <- format_code_download(codes)
  expect_true(grepl("library\\(microeco\\)", output))
  expect_true(grepl("x <- 1", output))
})
