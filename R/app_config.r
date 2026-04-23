#' @title microecoshiny Application Configuration
#' @description
#' Central configuration constants for the application including supported methods,
#' taxonomic ranks, color palettes, and module identifiers.
#' @keywords configuration settings
#' @family configuration
app_config <- list(
  app_name = "microecoshiny",
  app_version = "0.1.0",
  app_title = "microecoshiny \u2014 \u5fae\u751f\u7269\u7ec4\u6570\u636e\u5206\u6790\u4ea4\u4e92\u5f0f\u5de5\u5177",
  app_description = paste(
    "\u57fa\u4e8e R \u8bed\u8a00 microeco \u5305\u7684\u5fae\u751f\u7269\u7ec4\u6570\u636e\u5206\u6790\u4ea4\u4e92\u5f0f\u5de5\u5177",
    "\u652f\u6301\u6570\u636e\u5bfc\u5165\u3001\u9884\u5904\u7406\u3001\u591a\u6837\u6027\u5206\u6790\u3001\u5dee\u5f02\u5206\u6790\u3001\u7f51\u7edc\u5206\u6790\u3001\u96f6\u6a21\u578b\u3001\u529f\u80fd\u9884\u6d4b\u548c\u673a\u5668\u5b66\u4e60"
  ),
  author = "microecoshiny Developer",
  license = "MIT",
  url = "https://github.com/microecoshiny/microecoshiny",

  # Supported file formats
  supported_formats = c(
    ".csv", ".tsv", ".txt", ".xlsx", ".xls",
    ".qza", ".RData", ".rds", ".fasta", ".fa", ".nwk"
  ),

  # Alpha diversity measures
  alpha_measures = c(
    "Observed", "Chao1", "ACE", "Shannon", "Simpson",
    "Pielou_evenness", "InvSimpson", "Fisher", "NPShannon", "Coverage"
  ),

  # Beta diversity measures
  beta_measures = c(
    "bray", "jaccard", "aitchison", "euclidean",
    "canberra", "manhattan", "gower"
  ),

  # Ordination methods
  ordination_methods = c(
    "PCoA", "NMDS", "PCA", "DCA"
  ),

  # Normalization methods
  norm_methods = c(
    "rarefy", "CLR", "rclr", "GMPR", "CSS",
    "TMM", "RLE", "TSS", "DESeq2", "Wrench"
  ),

  # Differential abundance methods
  diff_methods = c(
    "metastat", "metagenomeSeq", "ALDEx2", "DESeq2",
    "edgeR", "ancombc2", "linda", "maaslin2", "LEfSe",
    "wilcox", "anova", "KW_dunn", "glmm_beta", "lme"
  ),

  # Differential abundance thresholds
  default_lfc = 0,
  default_alpha = 0.05,

  # Taxonomic ranks
  taxranks = c(
    "Kingdom", "Phylum", "Class", "Order",
    "Family", "Genus", "Species", "OTU", "ASV"
  ),

  # Correlation methods
  cor_methods = c(
    "pearson", "spearman", "kendall", "sparcc"
  ),

  # Network correlation methods
  network_cor_methods = c(
    "pearson", "spearman", "kendall", "sparcc", "cclasso"
  ),

  # Default colors palette
  colors_palette = c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
    "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
    "#bcbd22", "#17becf", "#aec7e8", "#ffbb78",
    "#98df8a", "#ff9896", "#c5b0d5", "#c49c94"
  ),

  # Default plot theme
  plot_theme = ggplot2::theme_classic(),

  # Max file size (MB)
  max_upload_size = 500,

  # Max preview rows
  max_preview_rows = 100,

  # Module IDs for sidebar navigation
  modules = list(
    import = "mod_import",
    preprocess = "mod_preprocess",
    norm = "mod_norm",
    composition = "mod_composition",
    alpha = "mod_alpha",
    beta = "mod_beta",
    core = "mod_core",
    diff = "mod_diff",
    env = "mod_env",
    network = "mod_network",
    nullmodel = "mod_nullmodel",
    func = "mod_func",
    ml = "mod_ml",
    multiomics = "mod_multiomics",
    export = "mod_export",
    codes = "mod_codes"
  )
)
