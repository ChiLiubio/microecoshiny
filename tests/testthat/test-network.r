library(testthat)
library(microeco)

context("Network analysis")

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

test_that("trans_network$new creates object", {
  mt <- create_test_microtable()
  t_net <- trans_network$new(dataset = mt, cor_method = "spearman", taxa_level = "Genus")
  expect_s3_class(t_net, "trans_network")
})

test_that("trans_network$new with pearson method", {
  mt <- create_test_microtable()
  t_net <- trans_network$new(dataset = mt, cor_method = "pearson", taxa_level = "Genus")
  expect_s3_class(t_net, "trans_network")
})

test_that("cal_network works", {
  mt <- create_test_microtable()
  t_net <- trans_network$new(dataset = mt, cor_method = "spearman", taxa_level = "Genus")
  t_net$cal_network(r_threshold = 0.6, p_threshold = 0.05)
  expect_true(!is.null(t_net$res_network))
})

test_that("plot_network works", {
  mt <- create_test_microtable()
  t_net <- trans_network$new(dataset = mt, cor_method = "spearman", taxa_level = "Genus")
  t_net$cal_network(r_threshold = 0.6, p_threshold = 0.05)
  expect_s3_class(t_net$plot_network(), "gg")
})

test_that("res_network_topo is populated", {
  mt <- create_test_microtable()
  t_net <- trans_network$new(dataset = mt, cor_method = "spearman", taxa_level = "Genus")
  t_net$cal_network(r_threshold = 0.6, p_threshold = 0.05)
  expect_true(!is.null(t_net$res_network_topo))
})

test_that("res_network_nodes is populated", {
  mt <- create_test_microtable()
  t_net <- trans_network$new(dataset = mt, cor_method = "spearman", taxa_level = "Genus")
  t_net$cal_network(r_threshold = 0.6, p_threshold = 0.05)
  expect_true(!is.null(t_net$res_network_nodes))
})
