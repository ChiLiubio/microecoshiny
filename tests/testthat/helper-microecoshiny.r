# Helper functions for microecoshiny tests

# Example data paths from app_research.md
TEST_DATA_DIR <- "E:/3_R_packages/AI/microeco_protocol_v1/Input"
QIIME2_CONVERTED_DIR <- file.path(TEST_DATA_DIR, "1.Amplicon/QIIME2_converted")
SAMPLE_INFO_PATH <- file.path(TEST_DATA_DIR, "1.Amplicon/Sample_info.xlsx")

# Check if example data exists
has_example_data <- function() {
  dir.exists(QIIME2_CONVERTED_DIR) &&
    file.exists(file.path(QIIME2_CONVERTED_DIR, "feature_table.tsv")) &&
    file.exists(file.path(QIIME2_CONVERTED_DIR, "taxonomy.tsv")) &&
    file.exists(file.path(QIIME2_CONVERTED_DIR, "tree.nwk")) &&
    file.exists(file.path(QIIME2_CONVERTED_DIR, "dna-sequences.fasta")) &&
    file.exists(SAMPLE_INFO_PATH)
}

# Create a minimal microtable for testing (fallback when real data not available)
create_test_microtable <- function() {
  otu_table <- data.frame(
    S1 = c(10, 5, 3, 2),
    S2 = c(8, 4, 1, 0),
    S3 = c(15, 7, 2, 1),
    row.names = c("T1", "T2", "T3", "T4"),
    check.names = FALSE
  )

  tax_table <- data.frame(
    Kingdom = rep("Bacteria", 4),
    Phylum = c("Firmicutes", "Bacteroidetes", "Proteobacteria", "Actinobacteria"),
    Class = c("Bacilli", "Bacteroidia", "Gammaproteobacteria", "Actinobacteria"),
    Order = c("Lactobacillales", "Bacteroidales", "Enterobacterales", "Bifidobacteriales"),
    Family = c("Lactobacillaceae", "Bacteroidaceae", "Enterobacteriaceae", "Bifidobacteriaceae"),
    Genus = c("Lactobacillus", "Bacteroides", "Escherichia", "Bifidobacterium"),
    Species = c("sp.", "sp.", "coli", "longum"),
    stringsAsFactors = FALSE,
    row.names = c("T1", "T2", "T3", "T4")
  )

  sample_table <- data.frame(
    Group = c("Control", "Treatment", "Treatment"),
    Timepoint = c("T1", "T1", "T2"),
    pH = c(6.5, 7.0, 7.5),
    Temperature = c(25, 30, 35),
    stringsAsFactors = FALSE,
    row.names = c("S1", "S2", "S3")
  )

  mt <- microtable$new(
    sample_table = sample_table,
    otu_table = otu_table,
    tax_table = tax_table
  )
  mt$tidy_dataset()
  mt
}

# Create a microtable from real protocol test data
create_real_microtable <- function() {
  if (!has_example_data()) {
    stop("Real test data not found at: ", QIIME2_CONVERTED_DIR)
  }

  # Feature abundance table (QIIME2 converted TSV)
  feature_table <- read.table(
    file.path(QIIME2_CONVERTED_DIR, "feature_table.tsv"),
    sep = "\t", skip = 1, comment.char = "$",
    header = TRUE, row.names = 1
  )

  # Taxonomy table (split merged taxonomy)
  taxonomy_table_raw <- read.table(
    file.path(QIIME2_CONVERTED_DIR, "taxonomy.tsv"),
    sep = "\t", skip = 1, comment.char = "$",
    header = FALSE
  )
  taxonomy_table <- file2meco:::split_assignments(
    taxonomy_table_raw[, 2],
    ranks = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
    split = ";"
  )
  taxonomy_table <- as.data.frame(taxonomy_table)
  rownames(taxonomy_table) <- taxonomy_table_raw[, 1]

  # Phylogenetic tree
  tree <- ape::read.tree(file.path(QIIME2_CONVERTED_DIR, "tree.nwk"))

  # Representative sequences
  sequences <- Biostrings::readDNAStringSet(
    file.path(QIIME2_CONVERTED_DIR, "dna-sequences.fasta")
  )

  # Sample information from Excel
  tmp_sample <- as.data.frame(
    readxl::read_excel(SAMPLE_INFO_PATH, col_names = TRUE),
    stringsAsFactors = FALSE
  )
  rownames(tmp_sample) <- tmp_sample[, 1]

  # Construct microtable
  mt <- microtable$new(
    sample_table = tmp_sample,
    otu_table = feature_table,
    tax_table = taxonomy_table,
    phylo_tree = tree,
    rep_fasta = sequences
  )
  mt$tidy_dataset()
  mt
}

# Create a microtable from Excel feature table only (simulating auto-import)
create_microtable_from_excel <- function() {
  if (!has_example_data()) {
    stop("Real test data not found at: ", QIIME2_CONVERTED_DIR)
  }

  # Read feature table from converted TSV (simulating what read_table_auto does)
  feature_table <- read.table(
    file.path(QIIME2_CONVERTED_DIR, "feature_table.tsv"),
    sep = "\t", skip = 1, comment.char = "$",
    header = TRUE, row.names = 1
  )

  # Sample information
  tmp_sample <- as.data.frame(
    readxl::read_excel(SAMPLE_INFO_PATH, col_names = TRUE),
    stringsAsFactors = FALSE
  )
  rownames(tmp_sample) <- tmp_sample[, 1]

  # Taxonomy table
  taxonomy_table_raw <- read.table(
    file.path(QIIME2_CONVERTED_DIR, "taxonomy.tsv"),
    sep = "\t", skip = 1, comment.char = "$",
    header = FALSE
  )
  taxonomy_table <- file2meco:::split_assignments(
    taxonomy_table_raw[, 2],
    ranks = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
    split = ";"
  )
  taxonomy_table <- as.data.frame(taxonomy_table)
  rownames(taxonomy_table) <- taxonomy_table_raw[, 1]

  mt <- microtable$new(
    otu_table = feature_table,
    tax_table = taxonomy_table,
    sample_table = tmp_sample,
    auto_tidy = TRUE
  )
  mt
}
