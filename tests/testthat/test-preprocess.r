test_that("filter_pollution works on test microtable", {
  mt <- create_test_microtable()
  n_before <- ncol(mt$otu_table)
  mt$filter_pollution(taxa = c("mitochondria", "chloroplast"))
  mt$tidy_dataset()
  n_after <- ncol(mt$otu_table)
  expect_true(n_after <= n_before)
})

test_that("filter_taxa works on test microtable", {
  mt <- create_test_microtable()
  n_before <- ncol(mt$otu_table)
  mt$filter_taxa(rel_abund = 0.01, freq = 0.1)
  mt$tidy_dataset()
  n_after <- ncol(mt$otu_table)
  expect_true(n_after <= n_before)
})

test_that("tidy_taxonomy works correctly", {
  mt <- create_test_microtable()
  tax_before <- mt$tax_table
  mt$tax_table <- microeco::tidy_taxonomy(mt$tax_table)
  expect_true(is.data.frame(mt$tax_table))
  expect_equal(nrow(mt$tax_table), nrow(tax_before))
})

test_that("sample_sums returns correct values", {
  mt <- create_test_microtable()
  sums <- mt$sample_sums()
  expect_equal(length(sums), nrow(mt$otu_table))
  expect_true(all(sums > 0))
})

test_that("get_sample_cols returns correct columns", {
  mt <- create_test_microtable()
  rv <- reactiveValues(microtable = mt)
  cols <- get_sample_cols(rv)
  expect_true("Group" %in% cols)
  expect_true("Timepoint" %in% cols)
})

test_that("get_tax_ranks returns correct columns", {
  mt <- create_test_microtable()
  rv <- reactiveValues(microtable = mt)
  ranks <- get_tax_ranks(rv)
  expect_true("Kingdom" %in% ranks)
  expect_true("Genus" %in% ranks)
})
