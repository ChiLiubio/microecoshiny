#' @title Multi-omics Module UI
#' @description
#' Provides interface for multi-omics integration including HUMAnN data import,
#' metabolome data import, pathway analysis, and microbe-metabolite correlation.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash
#' @keywords multi-omics metabolome microbiome
#' @family advanced-analysis
mod_multiomics_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(column(12, h2(tr("\U0001f9ec 多组学分析 Multi-omics Analysis", "\U0001f9ec Multi-omics Analysis")))),
    fluidRow(
      column(12,
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            title = tr("HUMAnN 导入", "HUMAnN Import"),
            bs4Dash::box(
              title = tr("HUMAnN2/HUMAnN3 数据导入", "HUMAnN2/HUMAnN3 Data Import"),
              status = "primary", solidHeader = TRUE, width = 12,
              shiny::fileInput(ns("humann_file"), tr("HUMAnN 输出文件 (.tsv)", "HUMAnN output file (.tsv)"), accept = ".tsv"),
              shiny::selectInput(ns("humann_db"), tr("功能数据库", "Function Database"),
                choices = c("UniRef90", "MetaCyc", "KEGG"), selected = "MetaCyc"),
              shiny::fileInput(ns("humann_sample"), tr("样本信息 (csv/tsv)", "Sample info (csv/tsv)"), accept = c(".csv", ".tsv")),
              shiny::actionButton(ns("run_humann_import"), tr("导入", "Import"), icon = icon("upload"), class = "btn-primary")
            )
          ),
          shiny::tabPanel(
            title = tr("代谢组导入", "Metabolome Import"),
            bs4Dash::box(
              title = tr("代谢组数据导入", "Metabolome Data Import"),
              status = "info", solidHeader = TRUE, width = 12,
              shiny::fileInput(ns("metab_file"), tr("代谢组数据 (.csv/.tsv)", "Metabolome data (.csv/.tsv)"), accept = c(".csv", ".tsv")),
              shiny::fileInput(ns("metab_sample"), tr("样本信息", "Sample info"), accept = c(".csv", ".tsv")),
              shiny::actionButton(ns("run_metab_import"), tr("导入", "Import"), class = "btn-info")
            )
          ),
          shiny::tabPanel(
            title = tr("功能通路分析", "Pathway Analysis"),
            fluidRow(
              column(6,
                bs4Dash::box(
                  title = tr("参数设置", "Parameters"),
                  status = "warning", solidHeader = TRUE, width = NULL,
                  shiny::selectInput(ns("pathway_db"), tr("功能数据库", "Function Database"),
                    choices = c("MetaCyc", "KEGG"), selected = "MetaCyc"),
                  shiny::selectInput(ns("pathway_group"), tr("分组列", "Group Column"), choices = character(0)),
                  shiny::selectInput(ns("pathway_level"), tr("分析水平", "Analysis Level"),
                    choices = c("Pathway Level 1", "Pathway Level 2", "Pathway Level 3")),
                  shiny::numericInput(ns("pathway_top"), "Top N", value = 20, min = 5, max = 100),
                  shiny::actionButton(ns("run_pathway"), tr("功能通路分析", "Pathway Analysis"), class = "btn-warning")
                )
              ),
              column(6,
                bs4Dash::box(
                  title = tr("图表", "Charts"),
                  status = "success", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("pathway_plot"), height = "500px"))
                )
              )
            )
          ),
          shiny::tabPanel(
            title = tr("微生物-代谢物关联", "Microbe-Metabolite Correlation"),
            fluidRow(
              column(6,
                bs4Dash::box(
                  title = tr("参数设置", "Parameters"),
                  status = "primary", solidHeader = TRUE, width = NULL,
                  shiny::selectInput(ns("mm_cor_method"), tr("相关方法", "Correlation Method"),
                    choices = c("spearman", "pearson", "kendall"), selected = "spearman"),
                  shiny::numericInput(ns("mm_cor_thresh"), tr("相关系数阈值", "Correlation Threshold"), value = 0.6, step = 0.05),
                  shiny::numericInput(ns("mm_p_thresh"), tr("P值阈值", "P-value Threshold"), value = 0.05),
                  shiny::actionButton(ns("run_mm_cor"), tr("微生物-代谢物关联分析", "Microbe-Metabolite Correlation"), class = "btn-primary")
                )
              ),
              column(6,
                bs4Dash::box(
                  title = tr("关联网络", "Correlation Network"),
                  status = "info", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("mm_plot"), height = "500px"))
                )
              )
            )
          )
        )
      ),
      shiny::uiOutput(ns("multiomics_status"))
    )
  )
}

#' Multi-omics Module Server
#' @import shiny
mod_multiomics_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      if (!check_microtable(rv)) return()
      cols <- get_sample_cols(rv)
      updateSelectInput(session, "pathway_group", choices = c("None" = "", cols))
    })

    # HUMAnN import
    observeEvent(input$run_humann_import, {
      if (is.null(input$humann_file)) {
        showNotification("\u8bf7\u4e0a\u4f20 HUMAnN \u6587\u4ef6", type = "warning")
        return()
      }

      result <- safe_run({
        match_table <- NULL
        if (!is.null(input$humann_sample)) {
          match_table <- read.table(input$humann_sample$datapath, sep = "\t",
                                     header = TRUE, stringsAsFactors = FALSE)
        }
        t_metab <- file2meco::humann2meco(
          file_path = input$humann_file$datapath,
          db = input$humann_db,
          match_table = match_table,
          sample_table = NULL
        )
        t_metab
      }, "HUMAnN \u5bfc\u5165\u5931\u8d25")

      if (result$success) {
        rv$metagenome_data <- result$result
        showNotification("\u2705 HUMAnN \u6570\u636e\u5bfc\u5165\u6210\u529f", type = "message")

        code <- paste0(
          't_metab <- file2meco::humann2meco(\n',
          '  file_path = "path/to/humann_output.tsv",\n',
          '  db = "', input$humann_db, '"\n)\n'
        )
        append_code(rv, code, "HUMAnN \u6570\u636e\u5bfc\u5165")
      } else {
        showNotification(result$error, type = "error", duration = 10)
      }
    })

    # Metabolome import
    observeEvent(input$run_metab_import, {
      if (is.null(input$metab_file)) {
        showNotification("\u8bf7\u4e0a\u4f20\u4ee3\u8c22\u7ec4\u6570\u636e", type = "warning")
        return()
      }

      result <- safe_run({
        ext <- tolower(tools::file_ext(input$metab_file$name))
        sep <- if (ext == "csv") "," else "\t"
        metab_data <- read.table(input$metab_file$datapath, sep = sep,
                                  header = TRUE, row.names = 1, check.names = FALSE)
        metab_data
      }, "\u4ee3\u8c22\u7ec4\u5bfc\u5165\u5931\u8d25")

      if (result$success) {
        rv$metab_data <- result$result
        showNotification("\u2705 \u4ee3\u8c22\u7ec4\u6570\u636e\u5bfc\u5165\u6210\u529f", type = "message")
        append_code(rv, 'metab_data <- read.table("path/to/metab.csv", header = TRUE, row.names = 1)\n',
                    "\u4ee3\u8c22\u7ec4\u6570\u636e\u5bfc\u5165")
      } else {
        showNotification(result$error, type = "error", duration = 10)
      }
    })

    # Pathway analysis (placeholder - uses ab-based visualization)
    observeEvent(input$run_pathway, {
      if (is.null(rv$metagenome_data)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165 HUMAnN \u6570\u636e", type = "warning")
        return()
      }

      result <- safe_run({
        t_metab <- rv$metagenome_data
        groupmean_val <- if (isTRUE(nzchar(input$pathway_group))) input$pathway_group else NULL
        t_abund <- microeco::trans_abund$new(
          dataset = t_metab,
          taxrank = colnames(t_metab$tax_table)[1],
          ntaxa = input$pathway_top,
          groupmean = groupmean_val
        )
        t_abund
      }, "\u529f\u80fd\u901a\u8def\u5206\u6790\u5931\u8d25")

      if (result$success) {
        rv$pathway_result <- result$result
        showNotification("\u2705 \u529f\u80fd\u901a\u8def\u5206\u6790\u5b8c\u6210", type = "message")
        code <- paste0(
          't_abund <- trans_abund$new(\n',
          '  dataset = t_metab,\n',
          '  ntaxa = ', input$pathway_top, '\n)\n',
          't_abund$plot_bar()\n'
        )
        append_code(rv, code, "\u529f\u80fd\u901a\u8def\u5206\u6790")
      } else {
        showNotification(result$error, type = "error", duration = 10)
      }
    })

    output$pathway_plot <- shiny::renderPlot({
      req(rv$pathway_result)
      tryCatch(rv$pathway_result$plot_bar(), error = function(e) NULL)
    })

    # Microbe-metabolite correlation
    observeEvent(input$run_mm_cor, {
      if (is.null(rv$microtable) || is.null(rv$metab_data)) {
        showNotification("\u9700\u8981\u540c\u65f6\u52a0\u8f7d\u5fae\u751f\u7269\u7ec4\u548c\u4ee3\u8c22\u7ec4\u6570\u636e", type = "warning")
        return()
      }

      result <- safe_run({
        t_metab <- microeco::trans_metab$new(
          metab = rv$metab_data,
          microb = rv$microtable
        )
        t_metab$cal_cor(
          method = input$mm_cor_method,
          r_threshold = input$mm_cor_thresh,
          p_threshold = input$mm_p_thresh
        )
        t_metab
      }, "\u5fae\u751f\u7269-\u4ee3\u8c22\u7269\u5173\u8054\u5206\u6790\u5931\u8d25")

      if (result$success) {
        rv$mm_result <- result$result
        showNotification("\u2705 \u5fae\u751f\u7269-\u4ee3\u8c22\u7269\u5173\u8054\u5206\u6790\u5b8c\u6210", type = "message")
        code <- paste0(
          't_metab <- trans_metab$new(\n',
          '  metab = metab_data,\n',
          '  microb = tmp_microtable\n)\n',
          't_metab$cal_cor(\n',
          '  method = "', input$mm_cor_method, '",\n',
          '  r_threshold = ', input$mm_cor_thresh, ',\n',
          '  p_threshold = ', input$mm_p_thresh, '\n)\n',
          't_metab$plot_cor()\n'
        )
        append_code(rv, code, "\u5fae\u751f\u7269-\u4ee3\u8c22\u7269\u5173\u8054")
      } else {
        showNotification(result$error, type = "error", duration = 10)
      }
    })

    output$mm_plot <- shiny::renderPlot({
      req(rv$mm_result)
      tryCatch(rv$mm_result$plot_cor(), error = function(e) NULL)
    })

    output$multiomics_status <- renderUI({
      status_items <- character(0)
      if (!is.null(rv$metagenome_data)) status_items <- c(status_items, "\u2705 HUMAnN \u6570\u636e")
      if (!is.null(rv$metab_data)) status_items <- c(status_items, "\u2705 \u4ee3\u8c22\u7ec4\u6570\u636e")
      if (length(status_items) > 0) {
        tags$div(class = "alert alert-info", paste(status_items, collapse = " | "))
      }
    })
  })
}
