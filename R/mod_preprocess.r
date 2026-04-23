#' @title Data Preprocessing Module UI
#' @description
#' Provides interface for microbiome data preprocessing operations including:
#' filtering pollution sequences, tidying taxonomy, feature filtering,
#' sample renaming, merging datasets, subsetting, and factor level reordering.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @keywords preprocessing microbiome
#' @family data-processing
mod_preprocess_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, h2(if (lang == "zh") "\U0001f9f9 数据预处理" else "\U0001f9f9 Data Preprocessing"))
    ),

    # Row 1: Filter Pollution + Tidy Taxonomy
    fluidRow(
      column(6,
        bs4Dash::box(
          title = if (lang == "zh") "\U0001f6a8 过滤污染序列" else "\U0001f6a8 Filter Pollution",
          status = "danger", solidHeader = TRUE, width = NULL, collapsible = TRUE,
          tags$div(
            class = "alert alert-info",
            style = "font-size:0.78rem;padding:6px 10px;margin-bottom:10px;",
            icon("info-circle"),
            if (lang == "zh") " 过滤逻辑：匹配 tax_table 中每个特征的 **所有分类列**（Kingdom→Species），" else " Filter logic: match each feature's **all taxonomy columns** in tax_table (Kingdom\u2192Species), ",
            if (lang == "zh") "只要任一列包含目标关键词（不区分大小写），该特征即被移除。" else "if any column contains the target keyword (case-insensitive), the feature is removed."
          ),
          shiny::checkboxGroupInput(
            inputId = ns("pollution_taxa"),
            label = if (lang == "zh") "选择要过滤的类群" else "Select taxa to filter",
            choices = c(if (lang == "zh") "线粒体 Mitochondria" else "Mitochondria" = "mitochondria", if (lang == "zh") "叶绿体 Chloroplast" else "Chloroplast" = "chloroplast"),
            selected = c("mitochondria", "chloroplast")
          ),
          shiny::textInput(inputId = ns("custom_pollution"), label = if (lang == "zh") "自定义过滤类群（多个用逗号分隔）" else "Custom filter taxa (multiple separated by comma)", placeholder = "e.g. Cyanobacteria, Streptophyta"),
          shiny::actionButton(inputId = ns("run_filter_pollution"), label = if (lang == "zh") "执行过滤" else "Run Filter", icon = icon("filter"), class = "btn-danger")
        )
      ),
      column(6,
        bs4Dash::box(
          title = if (lang == "zh") "\U0001f504 整理分类信息" else "\U0001f504 Tidy Taxonomy",
          status = "info", solidHeader = TRUE, width = NULL, collapsible = TRUE,
          shiny::checkboxInput(inputId = ns("optimize_taxonomy"), label = if (lang == "zh") "优化分类信息 分类名称 (自动添加标准前缀)" else "Optimize taxonomy taxonomy names (auto-add standard prefix)", value = TRUE),
          shiny::checkboxInput(inputId = ns("add_to_taxonomy"), label = if (lang == "zh") "将特征名添加到分类表" else "Add feature names to taxonomy table", value = FALSE),
          shiny::conditionalPanel(
            condition = "input.add_to_taxonomy",
            ns = ns,
            shiny::textInput(inputId = ns("taxonomy_column_name"), label = if (lang == "zh") "分类表列名" else "Taxonomy table column name", value = "ASV", placeholder = "e.g. ASV, Feature ID")
          ),
          shiny::checkboxInput(inputId = ns("remove_prefix"), label = if (lang == "zh") "移除分类前缀 (k__, p__, D_0__ 等)" else "Remove taxonomy prefix (k__, p__, D_0__ etc)", value = FALSE),
          shiny::actionButton(inputId = ns("run_tidy_taxonomy"), label = if (lang == "zh") "执行整理" else "Run Tidy", icon = icon("broom"), class = "btn-info")
        )
      )
    ),

    # Row 2: Taxa Selection (NEW) + Filter Taxa
    fluidRow(
      column(6,
        bs4Dash::box(
          title = if (lang == "zh") "\U0001f3af 类群筛选 Taxa Selection" else "\U0001f3af Taxa Selection",
          status = "primary", solidHeader = TRUE, width = NULL, collapsible = TRUE,
          tags$div(
            class = "alert alert-info",
            style = "font-size:0.78rem;padding:6px 10px;margin-bottom:10px;",
            icon("info-circle"),
            if (lang == "zh") " 筛选逻辑：指定 tax_table 的列名（如 Kingdom）和值（如 Bacteria），" else " Filter logic: specify tax_table column name (e.g. Kingdom) and value (e.g. Bacteria), ",
            if (lang == "zh") "仅保留在指定列中包含指定值的特征。多个条件为'与'关系（同时满足）。" else "only keep features containing the specified value in the specified column. Multiple conditions are AND (all must be met)."
          ),
          shiny::uiOutput(ns("taxa_sel_rows")),
          shiny::actionButton(
            inputId = ns("add_taxa_sel_row"), label = NULL,
            icon = icon("plus-circle"),
            class = "btn-sm btn-outline-primary",
            style = "margin-bottom:10px;"
          ),
          shiny::actionButton(inputId = ns("run_taxa_selection"), label = if (lang == "zh") "执行筛选" else "Run Selection", icon = icon("filter"), class = "btn-primary")
        )
      ),
      column(6,
        bs4Dash::box(
          title = if (lang == "zh") "\U0001f5d1 过滤低丰度特征" else "\U0001f5d1 Filter Taxa",
          status = "warning", solidHeader = TRUE, width = NULL, collapsible = TRUE,
          fluidRow(
            column(5, shiny::sliderInput(inputId = ns("rel_abund_thresh"), label = if (lang == "zh") "相对丰度阈值 (Relative Abundance)" else "Relative Abundance Threshold", min = 0, max = 0.1, value = 0.0001, step = 0.00001)),
            column(3, shiny::numericInput(inputId = ns("rel_abund_input"), label = if (lang == "zh") "直接输入" else "Direct Input", value = 0.0001, min = 0, max = 0.1, step = 0.00001, width = "100%")),
            column(4, tags$small(if (lang == "zh") "过滤所有样本中相对丰度均低于此阈值的特征" else "Filter features with mean relative abundance below this threshold across all samples"))
          ),
          fluidRow(
            column(5, shiny::sliderInput(inputId = ns("freq_thresh"), label = if (lang == "zh") "出现频率阈值 (Prevalence)" else "Prevalence Threshold", min = 0, max = 1, value = 0.1, step = 0.05)),
            column(3, shiny::numericInput(inputId = ns("freq_input"), label = if (lang == "zh") "直接输入" else "Direct Input", value = 0.1, min = 0, max = 1, step = 0.05, width = "100%")),
            column(4, tags$small(if (lang == "zh") "仅保留在指定比例以上样本中出现的特征" else "Only keep features present in above specified proportion of samples"))
          ),
          fluidRow(
            column(12, br(), shiny::actionButton(inputId = ns("run_filter_taxa"), label = if (lang == "zh") "执行过滤" else "Run Filter", icon = icon("filter"), class = "btn-warning"), shiny::textOutput(ns("filter_result")))
          )
        )
      )
    ),

    # Row 3: Rename Taxa + Merge Samples
    fluidRow(
      column(6, bs4Dash::box(title = if (lang == "zh") "\u270f 特征重命名" else "\u270f Rename Taxa", status = "primary", solidHeader = TRUE, width = NULL, collapsible = TRUE, shiny::textInput(inputId = ns("rename_prefix"), label = if (lang == "zh") "新名称前缀" else "New Name Prefix", placeholder = "e.g. ASV, OTU"), shiny::actionButton(inputId = ns("run_rename"), label = if (lang == "zh") "执行重命名" else "Run Rename", class = "btn-primary"))),
      column(6, bs4Dash::box(title = if (lang == "zh") "\U0001f517 合并样本" else "\U0001f517 Merge Samples", status = "success", solidHeader = TRUE, width = NULL, collapsible = TRUE, shiny::selectInput(inputId = ns("merge_by"), label = if (lang == "zh") "按列合并" else "Merge By Column", choices = character(0)), shiny::selectInput(inputId = ns("merge_method"), label = if (lang == "zh") "合并方法" else "Merge Method", choices = c("sum" = "sum", "mean" = "mean"), selected = "sum"), shiny::actionButton(inputId = ns("run_merge"), label = if (lang == "zh") "执行合并" else "Run Merge", class = "btn-success")))
    ),

    # Row 4: Sample Subset + Factor Levels
    fluidRow(
      column(6, bs4Dash::box(title = if (lang == "zh") "\U0001f4cb 样本子集" else "\U0001f4cb Sample Subset", status = "info", solidHeader = TRUE, width = NULL, collapsible = TRUE, shiny::selectInput(inputId = ns("subset_col"), label = if (lang == "zh") "筛选列" else "Filter Column", choices = character(0)), shinyWidgets::pickerInput(inputId = ns("subset_values"), label = if (lang == "zh") "筛选值（多选）" else "Filter Values (multiple)", choices = character(0), selected = character(0), multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE)), shiny::actionButton(inputId = ns("run_subset"), label = if (lang == "zh") "执行筛选" else "Run Filter", class = "btn-info"))),
      column(6, bs4Dash::box(title = if (lang == "zh") "\U0001f3a8 因子水平排序" else "\U0001f3a8 Factor Levels", status = "secondary", solidHeader = TRUE, width = NULL, collapsible = TRUE, shiny::selectInput(inputId = ns("factor_col"), label = if (lang == "zh") "选择因子列" else "Select Factor Column", choices = character(0)), shiny::uiOutput(ns("factor_levels_ui")), shiny::actionButton(inputId = ns("run_factor_levels"), label = if (lang == "zh") "应用排序" else "Apply Order", class = "btn-secondary")))
    )
  )
}

#' Data Preprocessing Module Server
#'
#' @param id Module ID
#' @param rv Global reactiveValues
#' @import shiny
mod_preprocess_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    op_counter <- reactiveVal(0L)
    bump <- function() {
      op_counter(op_counter() + 1L)
      rv$microtable_version <- op_counter()
    }

    # ===== Update sample column choices =====
    observe({
      if (!check_microtable(rv)) {
        updateSelectInput(session, "merge_by", choices = character(0))
        updateSelectInput(session, "subset_col", choices = character(0))
        updateSelectInput(session, "factor_col", choices = character(0))
        return()
      }
      cols <- get_sample_cols(rv)
      updateSelectInput(session, "merge_by", choices = cols)
      updateSelectInput(session, "subset_col", choices = cols)
      updateSelectInput(session, "factor_col", choices = cols, selected = NULL)
    })

    observeEvent(input$subset_col, {
      req(check_microtable(rv), isTRUE(nzchar(input$subset_col)))
      vals <- unique(as.character(rv$microtable$sample_table[[input$subset_col]]))
      vals <- vals[!is.na(vals)]
      updatePickerInput(session, "subset_values", choices = vals, selected = vals)
    })

    output$factor_levels_ui <- renderUI({
      req(check_microtable(rv), isTRUE(nzchar(input$factor_col)))
      levels <- unique(as.character(rv$microtable$sample_table[[input$factor_col]]))
      levels <- levels[!is.na(levels)]
      shinyjqui::orderInput(
        inputId = ns("factor_order"),
        label = "拖拽调整因子水平顺序",
        items = levels,
        width = "100%"
      )
    })

    # ===== Taxa Selection: dynamic rows =====
    taxa_sel_counter <- reactiveVal(0L)

    output$taxa_sel_rows <- renderUI({
      n <- taxa_sel_counter()
      if (n == 0) return(NULL)

      if (!check_microtable(rv)) {
        return(tags$div(class = "text-muted", style = "font-size:0.8rem;", "\u8bf7\u5148\u5bfc\u5165\u6570\u636e"))
      }

      tax_cols <- colnames(rv$microtable$tax_table)
      if (is.null(tax_cols) || length(tax_cols) == 0) {
        return(tags$div(class = "text-muted", style = "font-size:0.8rem;", "\u65e0 tax_table \u6570\u636e"))
      }

      tagList(
        lapply(seq_len(n), function(i) {
          tags$div(
            class = "taxa-sel-row",
            style = "display:flex;gap:8px;align-items:center;margin-bottom:8px;",
            shiny::selectInput(
              inputId = ns(paste0("taxa_col_", i)),
              label = NULL,
              choices = tax_cols,
              selected = tax_cols[1],
              width = "35%"
            ),
            shinyWidgets::pickerInput(
              inputId = ns(paste0("taxa_val_", i)),
              label = NULL,
              choices = character(0),
              selected = character(0),
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                placeholder = "\u9009\u62e9\u5217\u540e\u81ea\u52a8\u52a0\u8f7d\u503c"
              ),
              width = "60%"
            ),
            shiny::actionButton(
              inputId = ns(paste0("taxa_rm_", i)),
              label = NULL,
              icon = icon("times"),
              class = "btn-sm btn-outline-danger",
              style = "flex-shrink:0;"
            )
          )
        })
      )
    })

    observeEvent(input$add_taxa_sel_row, {
      taxa_sel_counter(taxa_sel_counter() + 1L)
    })

    # Update value pickerInput when column selection changes for each row
    observe({
      n <- taxa_sel_counter()
      if (n == 0) return()
      for (i in seq_len(n)) {
        local({
          idx <- i
          observeEvent(input[[paste0("taxa_col_", idx)]], {
            req(check_microtable(rv), isTRUE(nzchar(input[[paste0("taxa_col_", idx)]])))
            col_name <- input[[paste0("taxa_col_", idx)]]
            tt <- rv$microtable$tax_table
            if (is.null(tt) || !col_name %in% colnames(tt)) return()
            vals <- unique(as.character(tt[[col_name]]))
            vals <- vals[!is.na(vals) & vals != ""]
            updatePickerInput(session, paste0("taxa_val_", idx),
              choices = vals, selected = character(0))
          }, ignoreInit = TRUE)
        })
      }
    })

    observe({
      n <- taxa_sel_counter()
      if (n == 0) return()
      for (i in seq_len(n)) {
        local({
          idx <- i
          observeEvent(input[[paste0("taxa_rm_", idx)]], {
            current <- taxa_sel_counter()
            if (current <= 1) {
              taxa_sel_counter(0L)
            } else {
              vals <- list()
              for (j in seq_len(current)) {
                if (j == idx) next
                vals[[length(vals) + 1]] <- list(
                  col = input[[paste0("taxa_col_", j)]],
                  val = input[[paste0("taxa_val_", j)]]
                )
              }
              taxa_sel_counter(0L)
              taxa_sel_counter(length(vals))
            }
          }, ignoreInit = TRUE)
        })
      }
    })

    # ===== Filter pollution =====
    observeEvent(input$run_filter_pollution, {
      if (!check_microtable(rv)) { showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error"); return() }
      taxa <- input$pollution_taxa
      if (isTRUE(nzchar(input$custom_pollution))) { taxa <- c(taxa, trimws(strsplit(input$custom_pollution, ",")[[1]])) }
      if (length(taxa) == 0) { showNotification("\u8bf7\u9009\u62e9\u8981\u8fc7\u6ee4\u7684\u7c7b\u7fa4", type = "warning"); return() }
      result <- safe_run({ rv$microtable$filter_pollution(taxa = taxa); rv$microtable$tidy_dataset(); rv$microtable }, "\u8fc7\u6ee4\u6c61\u67d3\u5e8f\u5217\u5931\u8d25")
      if (result$success) {
        bump()
        showNotification("\u2705 \u6c61\u67d3\u5e8f\u5217\u8fc7\u6ee4\u5b8c\u6210", type = "message")
        dn <- rv$microtable_name %||% "tmp_microtable"
        append_code(rv, paste0(dn, '$filter_pollution(taxa = c("', paste(taxa, collapse = '", "'), '"))\n', dn, '$tidy_dataset()\n'), "\u6570\u636e\u9884\u5904\u7406 - \u8fc7\u6ee4\u6c61\u67d3\u5e8f\u5217")
      } else { showNotification(result$error, type = "error", duration = 10) }
    })

    # ===== Taxa Selection =====
    observeEvent(input$run_taxa_selection, {
      if (!check_microtable(rv)) { showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error"); return() }
      n <- taxa_sel_counter()
      if (n == 0) { showNotification("\u8bf7\u6dfb\u52a0\u7b5b\u9009\u6761\u4ef6", type = "warning"); return() }

      conditions <- list()
      for (i in seq_len(n)) {
        col_val <- input[[paste0("taxa_col_", i)]]
        vals <- input[[paste0("taxa_val_", i)]]
        if (isTRUE(nzchar(col_val)) && length(vals) > 0) {
          conditions[[length(conditions) + 1]] <- list(col = col_val, vals = vals)
        }
      }
      if (length(conditions) == 0) { showNotification("\u8bf7\u9009\u62e9\u7b5b\u9009\u503c", type = "warning"); return() }

      result <- safe_run({
        tt <- rv$microtable$tax_table
        keep <- rep(TRUE, nrow(tt))
        for (cond in conditions) {
          if (cond$col %in% colnames(tt)) {
            keep <- keep & (as.character(tt[[cond$col]]) %in% cond$vals)
          }
        }
        removed <- sum(!keep)
        rv$microtable$tax_table <- tt[keep, , drop = FALSE]
        rv$microtable$tidy_dataset()
        list(table = rv$microtable, removed = removed)
      }, "\u7c7b\u7fa4\u7b5b\u9009\u5931\u8d25")

      if (result$success) {
        rv$microtable <- result$result$table
        bump()
        rem <- result$result$removed
        showNotification(paste0("\u2705 \u7c7b\u7fa4\u7b5b\u9009\u5b8c\u6210\uff0c\u79fb\u9664 ", rem, " \u4e2a\u7279\u5f81"), type = "message")
        cond_str <- paste(sapply(conditions, function(c) paste0(c$col, " \u2208 {", paste(c$vals, collapse = ", "), "}")), collapse = "; ")
        dn <- rv$microtable_name %||% "tmp_microtable"
        append_code(rv, paste0('# \u7c7b\u7fa4\u7b5b\u9009: ', cond_str, '\nkeep <- rep(TRUE, nrow(', dn, '$tax_table))\nfor(cond in list(', paste(sapply(conditions, function(c) paste0('list(col="', c$col, '",vals=c("', paste(c$vals, collapse = '", "'), '"))')), collapse = ", "), ')) {\n  keep <- keep & (as.character(', dn, '$tax_table[[cond$col]]) %in% cond$vals)\n}\n', dn, '$tax_table <- ', dn, '$tax_table[keep, ]\n', dn, '$tidy_dataset()\n'), "\u6570\u636e\u9884\u5904\u7406 - \u7c7b\u7fa4\u7b5b\u9009")
      } else { showNotification(result$error, type = "error", duration = 10) }
    })

    # ===== Tidy taxonomy =====
    observeEvent(input$run_tidy_taxonomy, {
      if (!check_microtable(rv)) { showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error"); return() }
      result <- safe_run({
        # Step 1: 优化分类信息 (自动添加标准前缀)
        if (isTRUE(input$optimize_taxonomy)) {
          rv$microtable$tax_table <- microeco::tidy_taxonomy(rv$microtable$tax_table)
        }
        
        # Step 2: 将特征名添加到分类表
        if (isTRUE(input$add_to_taxonomy)) {
          col_name <- input$taxonomy_column_name %||% "ASV"
          rv$microtable$add_rownames2tax(use_name = col_name)
        }
        
        # Step 3: 移除分类前缀 (k__, p__, D_0__ 等)
        if (isTRUE(input$remove_prefix)) {
          rv$microtable$tax_table <- remove_taxa_prefixes(rv$microtable$tax_table)
        }
        
        rv$microtable$tidy_dataset()
        rv$microtable
      }, "\u6574\u7406\u5206\u7c7b\u4fe1\u606f\u5931\u8d25")
      
      if (result$success) {
        bump()
        showNotification("\u2705 \u5206\u7c7b\u4fe1\u606f\u6574\u7406\u5b8c\u6210", type = "message")
        dn <- rv$microtable_name %||% "tmp_microtable"
        cl <- character(0)
        if (isTRUE(input$optimize_taxonomy)) cl <- c(cl, paste0(dn, '$tax_table <- tidy_taxonomy(', dn, '$tax_table)'))
        if (isTRUE(input$add_to_taxonomy)) {
          col_name <- input$taxonomy_column_name %||% "ASV"
          cl <- c(cl, sprintf(paste0(dn, '$add_rownames2tax(use_name = "%s")'), col_name))
        }
        if (isTRUE(input$remove_prefix)) cl <- c(cl, paste0(dn, '$tax_table <- remove_taxa_prefixes(', dn, '$tax_table)'))
        cl <- c(cl, paste0(dn, '$tidy_dataset()'))
        append_code(rv, paste(cl, collapse = "\n"), "\u6570\u636e\u9884\u5904\u7406 - \u6574\u7406\u5206\u7c7b\u4fe1\u606f")
      } else { showNotification(result$error, type = "error", duration = 10) }
    })

    # ===== Filter taxa =====
    # Sync slider and numeric input
    observeEvent(input$rel_abund_thresh, {
      updateNumericInput(session, "rel_abund_input", value = input$rel_abund_thresh)
    })
    observeEvent(input$rel_abund_input, {
      updateSliderInput(session, "rel_abund_thresh", value = input$rel_abund_input)
    })
    observeEvent(input$freq_thresh, {
      updateNumericInput(session, "freq_input", value = input$freq_thresh)
    })
    observeEvent(input$freq_input, {
      updateSliderInput(session, "freq_thresh", value = input$freq_input)
    })

    observeEvent(input$run_filter_taxa, {
      if (!check_microtable(rv)) { showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error"); return() }
      rel_abund <- input$rel_abund_input %||% input$rel_abund_thresh
      freq <- input$freq_input %||% input$freq_thresh
      result <- safe_run({
        nb <- nrow(rv$microtable$otu_table)
        rv$microtable$filter_taxa(rel_abund = rel_abund, freq = freq)
        rv$microtable$tidy_dataset()
        na <- nrow(rv$microtable$otu_table)
        list(table = rv$microtable, n_before = nb, n_after = na)
      }, "\u8fc7\u6ee4\u7279\u5f81\u5931\u8d25")
      if (result$success) {
        rv$microtable <- result$result$table
        bump()
        nb <- result$result$n_before; na <- result$result$n_after
        output$filter_result <- renderText({ paste0(" | \u4fdd\u7559: ", na, "/", nb, " (", round(na/nb*100,1), "%)") })
        showNotification(paste0("\u2705 \u8fc7\u6ee4\u5b8c\u6210\uff0c\u4fdd\u7559 ", na, "/", nb, " \u7279\u5f81"), type = "message")
        append_code(rv, paste0('tmp_microtable$filter_taxa(rel_abund = ', rel_abund, ', freq = ', freq, ')\ntmp_microtable$tidy_dataset()\n'), "\u6570\u636e\u9884\u5904\u7406 - \u8fc7\u6ee4\u4f4e\u4e30\u5ea6\u7279\u5f81")
      } else { showNotification(result$error, type = "error", duration = 10) }
    })

    # ===== Rename taxa =====
    observeEvent(input$run_rename, {
      if (!check_microtable(rv) || !isTRUE(nzchar(input$rename_prefix))) return()
      result <- safe_run({ rv$microtable$rename_taxa(newname_prefix = input$rename_prefix); rv$microtable }, "\u91cd\u547d\u540d\u5931\u8d25")
      if (result$success) { bump(); showNotification("\u2705 \u7279\u5f81\u91cd\u547d\u540d\u5b8c\u6210", type = "message"); append_code(rv, paste0('tmp_microtable$rename_taxa(newname_prefix = "', input$rename_prefix, '")\n'), "\u6570\u636e\u9884\u5904\u7406 - \u7279\u5f81\u91cd\u547d\u540d") } else { showNotification(result$error, type = "error", duration = 10) }
    })

    # ===== Merge samples =====
    observeEvent(input$run_merge, {
      if (!check_microtable(rv) || !isTRUE(nzchar(input$merge_by))) return()
      result <- safe_run({
        new_mt <- rv$microtable$merge_samples(group = input$merge_by)
        new_mt$tidy_dataset()
        new_mt
      }, "\u5408\u5e76\u6837\u672c\u5931\u8d25")
      if (result$success) {
        new_name <- paste0(rv$microtable_name, "_merged_", format(Sys.time(), "%H%M%S"))
        rv$workspace[[new_name]] <- result$result
        rv$microtable <- result$result
        rv$microtable_name <- new_name
        rv$workspace_active <- new_name
        rv$data_loaded <- TRUE
        bump()
        showNotification(paste0("\u2705 \u6837\u672c\u5408\u5e76\u5b8c\u6210\uff0c\u5df2\u81ea\u52a8\u5e94\u7528\u5bf9\u8c61: ", new_name), type = "message")
        append_code(rv, paste0(new_name, ' <- tmp_microtable$merge_samples(group = "', input$merge_by, '")\n', new_name, '$tidy_dataset()\n'), "\u6570\u636e\u9884\u5904\u7406 - \u5408\u5e76\u6837\u672c")
      } else { showNotification(result$error, type = "error", duration = 10) }
    })

    # ===== Subset samples =====
    observeEvent(input$run_subset, {
      if (!check_microtable(rv) || length(input$subset_values) == 0) return()
      result <- safe_run({ keep <- rv$microtable$sample_table[[input$subset_col]] %in% input$subset_values; rv$microtable$sample_table <- rv$microtable$sample_table[keep, , drop = FALSE]; rv$microtable$tidy_dataset(); rv$microtable }, "\u6837\u672c\u7b5b\u9009\u5931\u8d25")
      if (result$success) { bump(); showNotification("\u2705 \u6837\u672c\u5b50\u96c6\u5b8c\u6210", type = "message"); append_code(rv, paste0('keep <- tmp_microtable$sample_table[["', input$subset_col, '"]] %in% c("', paste(input$subset_values, collapse = '", "'), '")\ntmp_microtable$sample_table <- tmp_microtable$sample_table[keep, ]\ntmp_microtable$tidy_dataset()\n'), "\u6570\u636e\u9884\u5904\u7406 - \u6837\u672c\u5b50\u96c6\u7b5b\u9009") } else { showNotification(result$error, type = "error", duration = 10) }
    })

    # ===== Factor levels =====
    observeEvent(input$run_factor_levels, {
      if (!check_microtable(rv)) return()
      col <- input$factor_col
      if (!isTRUE(nzchar(col))) return()
      ordered_levels <- input$factor_order
      if (is.null(ordered_levels) || length(ordered_levels) == 0) {
        showNotification("请先设置因子水平顺序", type = "warning")
        return()
      }
      if (is.null(names(ordered_levels))) {
        ordered_levels <- as.character(ordered_levels)
      } else {
        ordered_levels <- names(ordered_levels)
      }
      result <- safe_run({ if (length(ordered_levels) > 0) { rv$microtable$sample_table[[col]] <- factor(rv$microtable$sample_table[[col]], levels = ordered_levels) }; rv$microtable }, "\u56e0\u5b50\u6392\u5e8f\u5931\u8d25")
      if (result$success) { bump(); showNotification("\u2705 \u56e0\u5b50\u6c34\u5e73\u6392\u5e8f\u5b8c\u6210", type = "message"); append_code(rv, paste0('tmp_microtable$sample_table[["', col, '"]] <- factor(tmp_microtable$sample_table[["', col, '"]], levels = c("', paste(ordered_levels, collapse = '", "'), '"))\n'), "\u6570\u636e\u9884\u5904\u7406 - \u56e0\u5b50\u6c34\u5e73\u6392\u5e8f") } else { showNotification(result$error, type = "error", duration = 10) }
    })

  })
}
