#' @title microecoshiny Utility Functions
#' @description
#' Helper functions used across the application for code generation,
#' data validation, and workspace management.
#' @keywords utility microbiome shiny
#' @family utility

#' Convert all columns of a data.frame to numeric (modifies in place via return)
#'
#' @param df A data.frame
#' @return List with 'df' (converted data.frame) and 'non_numeric_cols' (column names that could not be converted)

convert_to_numeric_safe <- function(df) {
  non_numeric_cols <- c()
  for (j in seq_len(ncol(df))) {
    col_name <- colnames(df)[j]
    original <- df[[j]]
    converted <- suppressWarnings(as.numeric(as.character(original)))
    if (all(is.na(converted)) && !all(is.na(original))) {
      non_numeric_cols <- c(non_numeric_cols, col_name)
    } else {
      df[[j]] <- converted
    }
  }
  list(df = df, non_numeric_cols = non_numeric_cols)
}

#' @title Append code to the generated codes panel
#' @description
#' Adds R code strings with timestamp and comment to the generated codes
#' collection for later viewing and exporting.
#' @param rv A reactiveValues object containing `generated_codes` and `code_counter`
#' @param code_str The R code string to append
#' @param comment A descriptive comment (typically in Chinese)
#' @return Invisible character string of the formatted code
#' @export
#' @examples
#' \donttest{
#' rv <- reactiveValues(generated_codes = character(0), code_counter = 0L)
#' append_code(rv, 'summary(mtcars)', "数据摘要")
#' }
append_code <- function(rv, code_str, comment = "") {
  if (is.null(rv$code_counter)) {
    rv$code_counter <- 0L
  }
  if (is.null(rv$generated_codes)) {
    rv$generated_codes <- character(0)
  }
  rv$code_counter <- rv$code_counter + 1L
  timestamp <- format(Sys.time(), "%H:%M:%S")
  header <- paste0(
    "## ", timestamp, " | Step ", rv$code_counter,
    if (isTRUE(nzchar(comment))) paste0(" | ", comment) else ""
  )
  formatted <- paste0(header, "\n", code_str, "\n\n")
  rv$generated_codes <- c(rv$generated_codes, formatted)
  invisible(formatted)
}

#' @title Clear all generated codes
#' @description
#' Resets the code counter and clears all accumulated codes.
#' @param rv A reactiveValues object
#' @return Invisible TRUE
#' @export
clear_codes <- function(rv) {
  rv$generated_codes <- character(0)
  rv$code_counter <- 0L
  invisible(TRUE)
}

#' @title Generate R code string for file import
#' @description
#' Creates R code for importing various file formats into microtable objects.
#' @param file_type Type of import ("qiime2_qza", "qiime2_tsv", "rdata", "manual")
#' @param paths Named list of file paths
#' @param object_name Name of the resulting microtable object
#' @return Character string of R code
generate_import_code <- function(file_type, paths, object_name = "tmp_microtable") {
  code <- switch(file_type,
    "qiime2_qza" = {
      args <- paste0(
        '  feature_table = "', paths$feature_table, '"\n',
        '  taxonomy_table = "', paths$taxonomy_table, '"'
      )
      if (!is.null(paths$phylo_tree) && isTRUE(nzchar(paths$phylo_tree))) {
        args <- paste0(args, ',\n  phylo_tree = "', paths$phylo_tree, '"')
      }
      if (!is.null(paths$rep_fasta) && isTRUE(nzchar(paths$rep_fasta))) {
        args <- paste0(args, ',\n  rep_fasta = "', paths$rep_fasta, '"')
      }
      if (!is.null(paths$sample_table) && isTRUE(nzchar(paths$sample_table))) {
        args <- paste0(args, ',\n  sample_table = "', paths$sample_table, '"')
      }
      paste0(
        'library(file2meco)\n',
        object_name, ' <- qiime2meco(\n', args, '\n)\n',
        object_name, '$tidy_dataset()\n'
      )
    },
    "qiime2_tsv" = {
      lines <- character(0)
      lines <- c(lines, paste0(
        'feature_table <- read.table("', paths$feature_table, '",\n',
        '  sep = "\\t", skip = 1, comment.char = "$", header = TRUE, row.names = 1\n',
        ', check.names = FALSE)'
      ))
      lines <- c(lines, paste0(
        'taxonomy_table_raw <- read.table("', paths$taxonomy_table, '",\n',
        '  sep = "\\t", skip = 1, comment.char = "$", header = FALSE)'
      ))
      lines <- c(lines, paste0(
        'taxonomy_table <- file2meco:::split_assignments(\n',
        '  taxonomy_table_raw[, 2],\n',
        '  ranks = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),\n',
        '  split = ";"\n',
        ') %>% as.data.frame\n',
        'rownames(taxonomy_table) <- taxonomy_table_raw[, 1]'
      ))
      if (!is.null(paths$phylo_tree) && isTRUE(nzchar(paths$phylo_tree))) {
        lines <- c(lines, paste0(
          'tree <- ape::read.tree("', paths$phylo_tree, '")'
        ))
      }
      if (!is.null(paths$rep_fasta) && isTRUE(nzchar(paths$rep_fasta))) {
        lines <- c(lines, paste0(
          'sequences <- Biostrings::readDNAStringSet("', paths$rep_fasta, '")'
        ))
      }
      lines <- c(lines, paste0(
        object_name, ' <- microtable$new(\n',
        '  otu_table = feature_table,\n',
        '  tax_table = taxonomy_table',
        if (!is.null(paths$phylo_tree) && isTRUE(nzchar(paths$phylo_tree))) ',\n  phylo_tree = tree' else '',
        if (!is.null(paths$rep_fasta) && isTRUE(nzchar(paths$rep_fasta))) ',\n  rep_fasta = sequences' else '',
        '\n)'
      ))
      lines <- c(lines, paste0(object_name, '$tidy_dataset()'))
      lines <- c(lines, paste0(object_name, ' %<>% tidy_taxonomy'))
      paste(lines, collapse = "\n\n")
    },
    "rdata" = {
      paste0('load("', paths$rdata, '")')
    },
    "manual" = {
      paste0("# Manual import\n", object_name, ' <- microtable$new(...)')
    },
    "# Unknown import type"
  )
  return(code)
}

#' Safe wrapper for analysis operations with error handling
#'
#' @param expr Expression to evaluate
#' @param error_msg Custom error message
#' @return Result of expression or error message
safe_run <- function(expr, error_msg = "\u5206\u6790\u5931\u8d25\uff0c\u8bf7\u68c0\u67e5\u53c2\u6570\u548c\u6570\u636e\u683c\u5f0f") {
  tryCatch(
    {
      result <- expr
      list(success = TRUE, result = result, error = NULL)
    },
    error = function(e) {
      list(success = FALSE, result = NULL, error = paste(error_msg, "\n", e$message))
    },
    warning = function(w) {
      result <- suppressWarnings(expr)
      list(success = TRUE, result = result, warning = w$message)
    }
  )
}

#' Check if microtable object exists and is valid
#'
#' @param rv reactiveValues containing microtable
#' @return TRUE if microtable exists, FALSE otherwise
check_microtable <- function(rv) {
  !is.null(rv$microtable) &&
    inherits(rv$microtable, "microtable") &&
    !is.null(rv$microtable$otu_table)
}

#' Get available sample metadata columns
#'
#' @param rv reactiveValues containing microtable
#' @return Character vector of column names from sample_table (excluding first if it's IDs)
get_sample_cols <- function(rv) {
  if (!check_microtable(rv)) return(character(0))
  st <- rv$microtable$sample_table
  if (is.null(st) || ncol(st) == 0) return(character(0))
  colnames(st)
}

#' Get available taxonomic ranks
#'
#' @param rv reactiveValues containing microtable
#' @return Character vector of taxonomic rank column names
get_tax_ranks <- function(rv) {
  if (!check_microtable(rv)) return(character(0))
  tt <- rv$microtable$tax_table
  if (is.null(tt) || ncol(tt) == 0) return(character(0))
  colnames(tt)
}

#' Format a numeric value for display
#'
#' @param x Numeric value
#' @param digits Number of decimal places
#' @return Formatted character string
fmt_num <- function(x, digits = 4) {
  if (is.na(x)) return("NA")
  formatC(x, digits = digits, format = "f", big.mark = ",")
}

#' Format a p-value for display
#'
#' @param p P-value
#' @return Formatted character string
fmt_pval <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.0001) return("< 0.0001")
  formatC(p, digits = 4, format = "f")
}

#' Create a styled info box
#'
#' @param title Box title
#' @param value Box value
#' @param icon FontAwesome icon name
#' @param color Box color (primary, success, info, warning, danger)
#' @return HTML tags for info box
info_box <- function(title, value, icon = "info-circle", color = "primary") {
  bs4Dash::box(
    title = title,
    status = color,
    solidHeader = TRUE,
    width = 3,
    icon = icon,
    h4(as.character(value), style = "margin: 0; font-weight: bold;"),
    collapsible = FALSE
  )
}

#' Download handler for R scripts
#'
#' @param code Character vector of code lines
#' @return Content string for download
format_code_download <- function(code) {
  header <- paste0(
    "# microecoshiny Generated Code\n",
    "# Generated: ", Sys.time(), "\n",
    "# ============================================\n\n",
    'library(microeco)\nlibrary(file2meco)\nlibrary(ggplot2)\n\n'
  )
  paste0(header, paste(code, collapse = ""), "\n")
}

#' Auto-detect file roles from uploaded files
#'
#' Analyzes file names and extensions to determine which file is
#' feature table, taxonomy, sample info, phylogenetic tree, or sequences.
#'
#' @param files A data.frame from shiny fileInput (with $name, $datapath columns)
#' @return A named list with elements: feature, taxonomy, sample, tree, seq
detect_file_roles <- function(files) {
  roles <- list(feature = NULL, taxonomy = NULL, sample = NULL, tree = NULL, seq = NULL)

  for (i in seq_len(nrow(files))) {
    name <- files$name[i]
    ext <- tolower(tools::file_ext(name))
    name_lower <- tolower(name)

    if (ext %in% c("nwk", "tree", "newick")) {
      roles$tree <- files[i, ]
      next
    }

    if (ext %in% c("fasta", "fa", "fna")) {
      roles$seq <- files[i, ]
      next
    }

    if (ext == "qza") {
      if (grepl("taxon", name_lower)) {
        roles$taxonomy <- files[i, ]
      } else if (grepl("tree|phylo", name_lower)) {
        roles$tree <- files[i, ]
      } else if (grepl("seq|rep|dna|fasta", name_lower)) {
        roles$seq <- files[i, ]
      } else if (grepl("sample|meta|map", name_lower)) {
        roles$sample <- files[i, ]
      } else {
        roles$feature <- files[i, ]
      }
      next
    }

    if (ext %in% c("tsv", "txt", "csv", "xlsx", "xls")) {
      file_content <- tryCatch({
        if (ext %in% c("xlsx", "xls")) {
          df_head <- suppressMessages(suppressWarnings(
            readxl::read_excel(files$datapath[i], n_max = 10)
          ))
        } else {
          read.table(files$datapath[i], sep = "\t", header = TRUE,
                    nrows = 10, check.names = FALSE, stringsAsFactors = FALSE)
        }
        df_head
      }, error = function(e) NULL)

      if (!is.null(file_content) && is.data.frame(file_content) && nrow(file_content) > 0) {
        col_names <- tolower(colnames(file_content))

        tax_keywords <- c("phylum", "class", "order", "family", "genus",
                         "species", "kingdom", "domain", "taxonomy", "tax")
        tax_col_indices <- which(sapply(col_names, function(cn) {
          any(sapply(tax_keywords, function(k) grepl(k, cn)))
        }))

        numeric_cols <- which(sapply(file_content, function(x) {
          is.numeric(x) || is.integer(x)
        }))
        total_cols <- ncol(file_content)
        numeric_ratio <- length(numeric_cols) / total_cols

        char_cols <- which(sapply(file_content, function(x) is.character(x)))
        char_ratio <- length(char_cols) / total_cols

        if (length(tax_col_indices) > 0 && length(numeric_cols) > 0) {
          combined_sheet <- list(
            type = "combined",
            name = name,
            datapath = files$datapath[i],
            tax_col_indices = tax_col_indices,
            numeric_col_indices = numeric_cols,
            ext = ext
          )
          if (is.null(roles$feature)) {
            roles$feature <- combined_sheet
          } else {
            roles$sample <- files[i, ]
          }
        } else if (length(tax_col_indices) > 0) {
          roles$taxonomy <- files[i, ]
        } else if (numeric_ratio >= 0.8) {
          if (is.null(roles$feature)) {
            roles$feature <- files[i, ]
          } else {
            roles$sample <- files[i, ]
          }
        } else if (char_ratio >= 0.5) {
          if (is.null(roles$sample)) {
            roles$sample <- files[i, ]
          } else {
            roles$feature <- files[i, ]
          }
        } else {
          if (is.null(roles$feature)) {
            roles$feature <- files[i, ]
          } else {
            roles$sample <- files[i, ]
          }
        }
      } else {
        if (is.null(roles$feature)) {
          roles$feature <- files[i, ]
        } else {
          roles$sample <- files[i, ]
        }
      }
      next
    }
  }

  roles
}

read_combined_file <- function(file_info) {
  ext <- file_info$ext %||% tolower(tools::file_ext(file_info$datapath))

  if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(file_info$datapath, col_names = TRUE), stringsAsFactors = FALSE)
  } else {
    df <- as.data.frame(read.table(file_info$datapath, sep = "\t", header = TRUE,
                    check.names = FALSE, stringsAsFactors = FALSE))
  }

  if (nrow(df) > 0) {
    rownames(df) <- df[[1]]
    df <- df[, -1, drop = FALSE]
  }

  tax_cols <- file_info$tax_col_indices
  numeric_cols <- file_info$numeric_col_indices

  tax_df <- df[, tax_cols, drop = FALSE]
  rownames(tax_df) <- rownames(df)

  feat_df <- df[, numeric_cols, drop = FALSE]
  rownames(feat_df) <- rownames(df)

  result <- convert_to_numeric_safe(feat_df)

  list(feature = result$df, taxonomy = tax_df)
}

#' Read a table file automatically, detecting format
#'
#' @param filepath Path to the file
#' @param force_numeric Whether to force convert all columns to numeric (default: FALSE)
#' @return A data.frame
read_table_auto <- function(filepath, force_numeric = FALSE) {
  ext <- tolower(tools::file_ext(filepath))

  if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(filepath, col_names = TRUE), stringsAsFactors = FALSE)
    if (nrow(df) > 0) {
      rownames(df) <- df[[1]]
      df <- df[, -1, drop = FALSE]
      if (force_numeric) {
        result <- convert_to_numeric_safe(df)
        df <- result$df
      }
    }
    return(df)
  }

  # Try to detect separator for text files
  # Read first few lines to detect
  first_lines <- readLines(filepath, n = 10, warn = FALSE)
  first_lines <- first_lines[nchar(first_lines) > 0]

  if (length(first_lines) == 0) {
    stop("File is empty")
  }

  # Check if it's a QIIME2 BIOM-converted TSV (has $ comment line)
  has_qiime2_header <- any(grepl("^#", first_lines))

  if (has_qiime2_header) {
    data_start <- which(!grepl("^#", first_lines))
    if (length(data_start) > 0) {
      df <- read.table(filepath, sep = "\t", header = TRUE,
                       comment.char = "#", row.names = 1,
                       check.names = FALSE, stringsAsFactors = FALSE)
      df <- as.data.frame(df)
      if (force_numeric) {
        result <- convert_to_numeric_safe(df)
        df <- result$df
      }
      return(df)
    }
  }

  tryCatch({
    df <- read.table(filepath, sep = "\t", header = TRUE,
                     row.names = 1, check.names = FALSE,
                     stringsAsFactors = FALSE, comment.char = "")
    df <- as.data.frame(df)
    if (force_numeric) {
      result <- convert_to_numeric_safe(df)
      df <- result$df
    }
    return(df)
  }, error = function(e) NULL)

  tryCatch({
    df <- read.csv(filepath, header = TRUE, row.names = 1,
                   check.names = FALSE, stringsAsFactors = FALSE)
    df <- as.data.frame(df)
    if (force_numeric) {
      result <- convert_to_numeric_safe(df)
      df <- result$df
    }
    return(df)
  }, error = function(e) NULL)

  tryCatch({
    df <- read.table(filepath, sep = ";", header = TRUE,
                     row.names = 1, check.names = FALSE,
                     stringsAsFactors = FALSE)
    df <- as.data.frame(df)
    if (force_numeric) {
      result <- convert_to_numeric_safe(df)
      df <- result$df
    }
    return(df)
  }, error = function(e) NULL)

  stop("Unable to read file as table: ", filepath)
}

#' Remove common taxonomy prefixes from tax_table
#' @param tax_table A data.frame containing taxonomic classifications
#' @return A data.frame with prefixes removed
remove_taxa_prefixes <- function(tax_table) {
  if (is.null(tax_table) || nrow(tax_table) == 0) return(tax_table)
  prefixes <- c("k__", "p__", "c__", "o__", "f__", "g__", "s__",
                "D_0__", "D_1__", "D_2__", "D_3__", "D_4__", 
                "D_5__", "D_6__", "D_7__", "D_8__", "D_9__")
  pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")")
  cleaned <- as.data.frame(lapply(tax_table, function(col) {
    if (is.character(col)) gsub(pattern, "", col) else col
  }), stringsAsFactors = FALSE)
  rownames(cleaned) <- rownames(tax_table)
  return(cleaned)
}

#' Safe wrapper for shinyFiles::getVolumes()
#' 
#' @description 
#' This function safely calls shinyFiles::getVolumes() and filters out
#' invalid strings that might cause errors in sub() function.
#' On Windows, getVolumes() may emit encoding warnings for drive labels
#' with non-ASCII characters (e.g. Chinese), so we suppress those warnings
#' and rebuild drive labels using only ASCII drive letters.
#' 
#' @return A named list of volumes
#' @export
get_volumes_safe <- function() {
  tryCatch({
    # Suppress encoding warnings from sub() on Windows with non-ASCII drive labels
    volumes <- suppressWarnings(shinyFiles::getVolumes()())
    
    # On Windows, replace garbled non-ASCII volume names with clean drive letters
    if (.Platform$OS.type == "windows") {
      clean_names <- names(volumes)
      for (i in seq_along(clean_names)) {
        # If the name contains non-ASCII or replacement chars, replace with drive letter
        if (any(Encoding(clean_names[i]) == "UTF-8") ||
            grepl("[^\x01-\x7F]", clean_names[i])) {
          # Extract drive letter from the path (e.g. "C:/" -> "C")
          drive_letter <- gsub(":.*$", "", basename(clean_names[i]))
          if (nchar(drive_letter) == 1) {
            clean_names[i] <- paste0(drive_letter, ":")
          }
        }
      }
      names(volumes) <- clean_names
    }
    
    # Filter out entries with invalid names
    valid_volumes <- volumes[sapply(names(volumes), function(x) {
      tryCatch({
        !is.na(x) && is.character(x) && nchar(x) > 0
      }, error = function(e) FALSE)
    })]
    valid_volumes
  }, error = function(e) {
    # Fallback: manually list Windows drives
    if (.Platform$OS.type == "windows") {
      drives <- system("wmic logicaldisk get caption", intern = TRUE)
      drives <- trimws(drives[grepl("^[A-Z]:", drives)])
      setNames(as.list(paste0(drives, "/")), drives)
    } else {
      list(Home = path.expand("~"))
    }
  })
}
