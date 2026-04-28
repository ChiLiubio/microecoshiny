## ============================================================================
## mod_import.r — microecoshiny Data Import Module
## ============================================================================
#' @title Data Import Module UI
#' @description
#' Provides user interface for importing microbiome data files.
#' Supports multiple import methods including auto-detection, manual file selection,
#' RData/RDS loading, and QIIME2 artifact import.
#' @param id Module ID for namespace
#' @return Shiny UI tagList
#' @import shiny bs4Dash
#' @importFrom shinycssloaders withSpinner
#' @keywords import microbiome data
#' @family data-input
mod_import_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h2(if (lang == "zh") "\U0001f4c2 数据导入" else "\U0001f4c2 Data Import"),
        p(if (lang == "zh") "上传微生物组数据文件，系统将自动识别文件类型并构建 microtable 对象。" else "Upload microbiome data files, the system will automatically detect file types and build microtable objects.")
      )
    ),

    # Import method tabs
    fluidRow(
      column(12,
        shiny::tabsetPanel(
          id = ns("import_method"),
          type = "tabs",

          # Tab 1: Auto Import
          shiny::tabPanel(
            title = if (lang == "zh") "\U0001f916 自动导入" else "\U0001f916 Auto Import",
            value = "auto",
            bs4Dash::box(
              title = if (lang == "zh") "上传文件" else "Upload Files",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              tags$div(
                class = "alert alert-info",
                style = "font-size:0.85rem;padding:8px 12px;margin-bottom:10px;",
                icon("info-circle"),
                " ",
                if (lang == "zh") " 建议使用 .xlsx 或 .csv 格式。.xls 格式可能存在兼容性问题。" else " It is recommended to use .xlsx or .csv format. .xls format may have compatibility issues."
              ),
              shiny::fileInput(
                inputId = ns("data_files"),
                label = if (lang == "zh") "选择文件（支持多选）" else "Select files (multiple supported)",
                multiple = TRUE,
                accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls",
                           ".qza", ".RData", ".rds", ".fasta", ".fa", ".nwk")
              ),
              DT::dataTableOutput(ns("file_list")),
              shiny::uiOutput(ns("file_type_confirm")),
              shiny::actionButton(
                inputId = ns("build_auto"),
                label = if (lang == "zh") "构建 microtable 对象" else "Build microtable object",
                icon = icon("play"),
                class = "btn-primary btn-lg"
              )
            )
          ),

          # Tab 2: QIIME2 Import
          shiny::tabPanel(
            title = if (lang == "zh") "\U0001f9ec QIIME2 导入" else "\U0001f9ec QIIME2 Import",
            value = "qiime2",
            bs4Dash::box(
              title = if (lang == "zh") "QIIME2 文件导入" else "QIIME2 File Import",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(6,
                  shiny::fileInput(inputId = ns("qza_feature"), label = "Feature Table (.qza)", accept = ".qza"),
                  shiny::fileInput(inputId = ns("qza_taxonomy"), label = "Taxonomy (.qza)", accept = ".qza")
                ),
                column(6,
                  shiny::fileInput(inputId = ns("qza_tree"), label = "Phylogenetic Tree (.qza)", accept = ".qza"),
                  shiny::fileInput(inputId = ns("qza_seq"), label = "Representative Sequences (.qza)", accept = ".qza")
                )
              ),
              fluidRow(
                column(6,
                  shiny::fileInput(inputId = ns("qza_sample"), label = "Sample Info (csv/tsv/xlsx)", accept = c(".csv", ".tsv", ".xlsx"))
                ),
                column(6, br(),
                  shiny::actionButton(inputId = ns("build_qiime2"), label = if (lang == "zh") "使用 qiime2meco 构建" else "Build with qiime2meco", icon = icon("play"), class = "btn-info btn-lg")
                )
              )
            )
          ),

          # Tab 3: QIIME2 TSV Import
          shiny::tabPanel(
            title = if (lang == "zh") "\U0001f4c4 QIIME2 TSV 导入" else "\U0001f4c4 QIIME2 TSV Import",
            value = "qiime2_tsv",
            bs4Dash::box(
              title = if (lang == "zh") "QIIME2 转换文件导入" else "QIIME2 Converted File Import",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(6,
                  shiny::fileInput(inputId = ns("tsv_feature"), label = "Feature Table (.tsv)", accept = c(".tsv", ".txt")),
                  shiny::fileInput(inputId = ns("tsv_taxonomy"), label = "Taxonomy (.tsv)", accept = c(".tsv", ".txt"))
                ),
                column(6,
                  shiny::fileInput(inputId = ns("tsv_tree"), label = "Phylogenetic Tree (.nwk)", accept = c(".nwk", ".tree")),
                  shiny::fileInput(inputId = ns("tsv_seq"), label = "Representative Sequences (.fasta)", accept = c(".fasta", ".fa"))
                )
              ),
              fluidRow(
                column(6,
                  shiny::fileInput(inputId = ns("tsv_sample"), label = "Sample Info (csv/tsv/xlsx)", accept = c(".csv", ".tsv", ".xlsx"))
                ),
                column(6, br(),
                  shiny::actionButton(inputId = ns("build_tsv"), label = if (lang == "zh") "构建 TSV 文件" else "Build TSV Files", icon = icon("play"), class = "btn-info btn-lg")
                )
              )
            )
          ),

          # Tab 4: RData Import
          shiny::tabPanel(
            title = paste0("\U0001f4be ", tr("mod.import.rdata_tab", lang)),
            value = "rdata",
            bs4Dash::box(
              title = tr("mod.import.rdata_load", lang),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(8,
                  shiny::fileInput(
                    inputId = ns("rdata_file"),
                    label = tr("mod.import.rdata_select", lang),
                    accept = c(".RData", ".rds")
                  )
                ),
                column(4,
                  shiny::uiOutput(ns("rdata_obj_selector"))
                )
              ),
              shiny::uiOutput(ns("rdata_progress")),
              shiny::uiOutput(ns("rdata_load_button"))
            )
          ),

          # Tab 5: Built-in Data (separated)
          shiny::tabPanel(
            title = paste0("\U0001f4e6 ", tr("mod.import.built_in", lang)),
            value = "builtin",
            bs4Dash::box(
              title = paste0("\U0001f4e6 ", tr("mod.import.builtin_title", lang)),
              status = "success",
              solidHeader = TRUE,
              width = 12,
              tags$div(
                style = "margin-top:15px;",
                tags$div(
                  class = "alert alert-info",
                  style = "font-size:0.85rem;padding:10px 15px;",
                  icon("info-circle"),
                  " ", tr("mod.import.builtin_desc", lang)
                ),
                tags$hr(),
                shiny::selectInput(
                  inputId = ns("builtin_dataset"),
                  label = tr("mod.import.builtin_select", lang),
                  choices = c("microeco example data (dataset)" = "dataset"),
                  selected = "dataset"
                ),
                shiny::actionButton(
                  inputId = ns("load_builtin"),
                  label = tr("mod.import.builtin_load", lang),
                  icon = icon("play"),
                  class = "btn-success btn-lg"
                )
              )
            )
          )
        )
      )
    ),

    # Loading status / progress
    shiny::uiOutput(ns("loading_indicator")),

    # microtable summary (shown after load)
    shiny::uiOutput(ns("data_summary_section")),

    # Data preview tabs
    shiny::uiOutput(ns("data_preview_section")),

    # Status messages
    shiny::uiOutput(ns("import_status"))
  )
}


#' Data Import Module Server
#'
#' @param id Module ID
#' @param rv Global reactiveValues from app_server
#' @import shiny
#' @import bs4Dash
#' @importFrom DT renderDataTable datatable dataTableOutput
#' @importFrom utils capture.output str head
mod_import_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ==================== RData Import ====================

    # Store objects found in RData file
    rdata_objects <- reactiveVal(NULL)
    rdata_env <- reactiveVal(new.env())

    # Store scanned RData environments for auto import
    auto_rdata_envs <- reactiveVal(list())

    # When RData file is selected, scan for available objects
    observeEvent(input$rdata_file, {
      req(input$rdata_file)
      filepath <- input$rdata_file$datapath
      filename <- input$rdata_file$name

      # Show progress
      output$rdata_progress <- renderUI({
        tags$div(
          style = "padding: 10px 0;",
          shiny::busyIndicator(
            text = paste0("正在扫描 ", filename, " 中的对象..."),
            wait = 0
          )
        )
      })

      # Use withProgress for actual progress
      tmp_env <- new.env()
      result <- tryCatch({
        ext <- tools::file_ext(filename)
        if (ext == "rds") {
          obj <- readRDS(filepath)
          varname <- tools::file_path_sans_ext(filename)
          assign(varname, obj, envir = tmp_env)
        } else {
          load(filepath, envir = tmp_env)
        }
        list(success = TRUE, env = tmp_env)
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })

      if (!result$success) {
        output$rdata_progress <- renderUI({
          tags$div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            paste0("文件读取失败: ", result$error)
          )
        })
        rdata_objects(NULL)
        return(NULL)
      }

      # Find all objects, prefer microtable objects
      obj_names <- ls(result$env)
      obj_classes <- sapply(obj_names, function(n) {
        paste(class(result$env[[n]]), collapse = "/")
      })

      rdata_env(result$env)
      rdata_objects(data.frame(
        name = obj_names,
        class = obj_classes,
        size_mb = sapply(obj_names, function(n) {
          format(object.size(result$env[[n]]), units = "MB")
        }),
        stringsAsFactors = FALSE
      ))

      # Auto-select microtable objects
      mt_names <- obj_names[sapply(obj_names, function(n) {
        inherits(result$env[[n]], "microtable")
      })]

      output$rdata_obj_selector <- renderUI({
        df <- rdata_objects()
        req(df)
        has_mt <- any(grepl("microtable", df$class))
        tagList(
          if (has_mt) {
            tags$div(
              style = "padding: 5px 0;",
              tags$span(icon("check-circle", style = "color: #27AE60;"), " 检测到 microtable 对象")
            )
          },
          shiny::selectInput(
            inputId = ns("rdata_obj_name"),
            label = "选择要加载的对象",
            choices = setNames(df$name, paste0(df$name, " [", df$class, ", ", df$size_mb, "]")),
            selected = if (length(mt_names) > 0) mt_names[1] else df$name[1]
          ),
          shiny::actionButton(
            inputId = ns("load_rdata"),
            label = "加载数据",
            icon = icon("upload"),
            class = "btn-success"
          )
        )
      })

      output$rdata_progress <- renderUI({
        df <- rdata_objects()
        tags$div(
          class = "alert alert-success",
          icon("check"),
          paste0("扫描完成，找到 ", nrow(df), " 个对象")
        )
      })

      # Show load button
      output$rdata_load_button <- renderUI({
        df <- rdata_objects()
        req(df)
        shiny::actionButton(
          inputId = ns("load_rdata"),
          label = "加载数据",
          icon = icon("upload"),
          class = "btn-success btn-lg"
        )
      })
    }, ignoreNULL = TRUE)

    # Load selected RData object into global rv
    observeEvent(input$load_rdata, {
      req(input$load_rdata)
      obj_name <- input$rdata_obj_name
      env <- rdata_env()
      req(obj_name, obj_name %in% ls(env))

      # Show loading progress
      output$loading_indicator <- renderUI({
        tags$div(
          style = "padding: 15px 0;",
          tags$div(
            class = "progress",
            style = "height: 24px; margin-bottom: 10px;",
            tags$div(
              class = "progress-bar progress-bar-striped progress-bar-animated",
              role = "progressbar",
              style = "width: 30%;",
              "30% - 正在加载对象..."
            )
          ),
          tags$p(style = "color: #666; font-size: 0.9rem;",
            icon("spinner", class = "fa-spin"),
            paste0("正在加载 ", obj_name, " ...")
          )
        )
      })

      # Use a small delay to allow UI to update, then load
      result <- tryCatch({
        obj <- env[[obj_name]]

        # Validate: prefer microtable, but accept data.frames too
        if (!inherits(obj, "microtable") && !is.data.frame(obj)) {
          stop(paste0("对象 '", obj_name, "' 不是 microtable 或 data.frame 类型 (实际类型: ",
                      paste(class(obj), collapse = "/"), ")"))
        }

        list(success = TRUE, obj = obj)
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })

      if (!result$success) {
        output$loading_indicator <- renderUI({
          tags$div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            paste0("加载失败: ", result$error)
          )
        })
        showNotification(result$error, type = "error", duration = 10)
        return(NULL)
      }

      # Store to global rv
      rv$microtable <- result$obj
      rv$microtable_name <- obj_name
      rv$data_loaded <- TRUE

      # Generate code
      filename <- input$rdata_file$name
      if (tools::file_ext(filename) == "rds") {
        code_str <- paste0(obj_name, ' <- readRDS("', filename, '")')
      } else {
        code_str <- paste0('load("', filename, '")')
      }
      append_code(rv, code_str, paste0("加载数据 - 从 ", filename, " 加载 ", obj_name))

      # Update loading indicator to success
      output$loading_indicator <- renderUI({
        tags$div(
          class = "alert alert-success",
          icon("check-circle"),
          paste0("数据加载成功！对象: ", obj_name, " (", paste(class(result$obj), collapse = "/"), ")")
        )
      })

      # Trigger summary and preview
      update_summary()
      showNotification(sprintf(tr("mod.import.notify.loaded", rv$current_language), obj_name), type = "message", duration = 5)
    }, ignoreNULL = TRUE)

    # ==================== Summary Display ====================

    update_summary <- reactive({
      req(check_microtable(rv))
      mt <- rv$microtable

      # Capture print output for microtable object
      print_output <- tryCatch({
        capture.output(print(mt), type = "output")
      }, error = function(e) {
        paste0("无法打印对象: ", e$message)
      })
      print_text <- paste(print_output, collapse = "\n")

      # Gather summary statistics
      summary_info <- list(
        n_samples = nrow(mt$sample_table),
        n_features = nrow(mt$otu_table),
        n_tax_ranks = ncol(mt$tax_table),
        has_tree = !is.null(mt$phylo_tree),
        has_seq = !is.null(mt$rep_fasta),
        has_sample = !is.null(mt$sample_table) && ncol(mt$sample_table) > 0,
        tax_ranks = if (!is.null(mt$tax_table)) colnames(mt$tax_table) else character(0),
        sample_cols = if (!is.null(mt$sample_table)) colnames(mt$sample_table) else character(0),
        total_seqs = if (!is.null(mt$otu_table)) sum(mt$otu_table) else 0,
        obj_name = rv$microtable_name,
        print_text = print_text
      )

      # Render summary section
      output$data_summary_section <- renderUI({
        lang <- rv$current_language
        info <- summary_info
        tagList(
          tags$hr(),
          h3(icon("info-circle"), " ", tr("summary.title", lang)),
          # Info boxes row
          fluidRow(
            bs4Dash::infoBox(
              title = tr("summary.samples", lang),
              value = format(info$n_samples, big.mark = ","),
              icon = icon("vials"),
              color = "primary",
              fill = TRUE
            ),
            bs4Dash::infoBox(
              title = tr("obj.browser.features", lang),
              value = format(info$n_features, big.mark = ","),
              icon = icon("dna"),
              color = "info",
              fill = TRUE
            ),
            bs4Dash::infoBox(
              title = tr("summary.tax_levels", lang),
              value = info$n_tax_ranks,
              icon = icon("layer-group"),
              color = "success",
              fill = TRUE
            ),
            bs4Dash::infoBox(
              title = tr("summary.total_seqs", lang),
              value = format(info$total_seqs, big.mark = ","),
              icon = icon("barcode"),
              color = "warning",
              fill = TRUE
            )
          ),
          # Detailed info cards
          fluidRow(
            column(6,
              bs4Dash::box(
                title = tr("summary.integrity", lang),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                tags$ul(
                  tags$li(if (info$has_sample)
                    tags$span(icon("check", style = "color: #27AE60;"), " sample_table: ", info$n_samples, " samples, ", length(info$sample_cols), " cols")
                  else
                    tags$span(icon("times", style = "color: #E74C3C;"), " sample_table: ", tr("summary.not_found", lang))),
                  tags$li(tags$span(icon("check", style = "color: #27AE60;"), " otu_table: ", info$n_features, " x ", info$n_samples)),
                  tags$li(tags$span(icon("check", style = "color: #27AE60;"), " tax_table: ", info$n_tax_ranks, " ranks [", paste(info$tax_ranks, collapse = ", "), "]")),
                  tags$li(if (info$has_tree)
                    tags$span(icon("check", style = "color: #27AE60;"), " phylo_tree: OK")
                  else
                    tags$span(icon("times", style = "color: #E74C3C;"), " phylo_tree: ", tr("summary.not_found", lang))),
                  tags$li(if (info$has_seq)
                    tags$span(icon("check", style = "color: #27AE60;"), " rep_fasta: OK")
                  else
                    tags$span(icon("times", style = "color: #E74C3C;"), " rep_fasta: ", tr("summary.not_found", lang)))
                )
              )
            ),
            column(6,
              bs4Dash::box(
                title = tr("summary.obj_info", lang),
                status = "info",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                tags$pre(
                  style = "background: #f8f9fa; padding: 10px; border-radius: 5px; font-size: 0.85rem; white-space: pre-wrap; max-height: 300px; overflow-y: auto;",
                  info$print_text
                )
              )
            )
          )
        )
      })

      # Render data preview section
      output$data_preview_section <- renderUI({
        lang <- rv$current_language
        info <- summary_info
        tagList(
          fluidRow(
            column(12,
              bs4Dash::tabsetPanel(
                id = ns("data_preview"),
                type = "tabs",
                # Feature table preview
                shiny::tabPanel(
                  title = paste0("Feature Table (", info$n_features, " x ", info$n_samples, ")"),
                  DT::dataTableOutput(ns("preview_otu"))
                ),
                # Sample info preview
                shiny::tabPanel(
                  title = paste0("Sample Info (", info$n_samples, " x ", length(info$sample_cols), ")"),
                  DT::dataTableOutput(ns("preview_sample"))
                ),
                # Taxonomy preview
                shiny::tabPanel(
                  title = paste0("Taxonomy (", info$n_features, " x ", info$n_tax_ranks, ")"),
                  DT::dataTableOutput(ns("preview_tax"))
                ),
                # Sample metadata columns
                if (length(info$sample_cols) > 0) {
                  shiny::tabPanel(
                    title = tr("mod.import.sample_vars_overview", lang),
                    shiny::uiOutput(ns("sample_vars_overview"))
                  )
                }
              )
            )
          )
        )
      })

      # Feature table preview
      output$preview_otu <- DT::renderDataTable({
        lang <- rv$current_language
        req(check_microtable(rv))
        otu <- rv$microtable$otu_table
        DT::datatable(
          as.data.frame(otu[1:min(50, nrow(otu)), 1:min(20, ncol(otu))]),
          options = list(scrollX = TRUE, pageLength = 10),
          caption = sprintf(tr("mod.import.showing_rows", lang), min(50, nrow(otu)), min(20, ncol(otu)), nrow(otu), ncol(otu))
        )
      })

      # Sample info preview
      output$preview_sample <- DT::renderDataTable({
        req(check_microtable(rv))
        st <- rv$microtable$sample_table
        DT::datatable(
          as.data.frame(st),
          options = list(scrollX = TRUE, pageLength = 10)
        )
      })

      # Taxonomy preview
      output$preview_tax <- DT::renderDataTable({
        lang <- rv$current_language
        req(check_microtable(rv))
        tt <- rv$microtable$tax_table
        DT::datatable(
          as.data.frame(tt[1:min(50, nrow(tt)), ]),
          options = list(scrollX = TRUE, pageLength = 10),
          caption = sprintf(tr("mod.import.showing_tax_rows", lang), min(50, nrow(tt)), nrow(tt))
        )
      })

      # Sample variables overview
      output$sample_vars_overview <- shiny::renderUI({
        lang <- rv$current_language
        req(check_microtable(rv))
        st <- rv$microtable$sample_table
        if (is.null(st) || ncol(st) == 0) return(NULL)

        var_info <- lapply(colnames(st), function(col) {
          val <- st[[col]]
          if (is.numeric(val)) {
            tags$div(
              style = "padding: 5px; margin: 3px 0; background: #f0f4f8; border-radius: 4px;",
              tags$strong(col), sprintf(" [%s]: ", tr("mod.import.numeric", lang)),
              tags$small(
                tr("mod.import.min", lang), "=", round(min(val, na.rm = TRUE), 2),
                " | ", tr("mod.import.max", lang), "=", round(max(val, na.rm = TRUE), 2),
                " | ", tr("mod.import.na", lang), "=", sum(is.na(val)))
            )
          } else {
            lvls <- unique(val)
            tags$div(
              style = "padding: 5px; margin: 3px 0; background: #f0f4f8; border-radius: 4px;",
              tags$strong(col), sprintf(" [%s]: ", tr("mod.import.factor", lang)),
              tags$small(
                length(lvls), " ", tr("mod.import.levels", lang), ": ",
                paste(head(lvls, 10), collapse = ", "),
                if (length(lvls) > 10) paste0(" ... ", sprintf(tr("mod.import.and_more", lang), length(lvls))))
            )
          }
        })
        tagList(var_info)
      })

      # Status message
      output$import_status <- renderUI({
        lang <- rv$current_language
        tags$div(
          class = "alert alert-success",
          icon("check-circle"),
          paste0(tr("mod.import.data_ready", lang), rv$microtable_name)
        )
      })
    })

    # Observe data_loaded to update display
    observeEvent(rv$data_loaded, {
      if (isTRUE(rv$data_loaded) && check_microtable(rv)) {
        update_summary()
      } else {
        output$data_summary_section <- renderUI({ NULL })
        output$data_preview_section <- renderUI({ NULL })
        output$import_status <- renderUI({ NULL })
        output$loading_indicator <- renderUI({ NULL })
      }
    }, ignoreNULL = TRUE)

    # ==================== Auto Import ====================

    # Store uploaded files info
    auto_files_info <- reactiveVal(NULL)
    auto_file_type <- reactiveVal(NULL)
    auto_detected_roles <- reactiveVal(NULL)
    auto_rdata_envs <- reactiveVal(list())
    auto_rdata_scan_results <- reactiveVal(list())

    # When files are uploaded, auto-detect types and roles
    observeEvent(input$data_files, {
      req(input$data_files)
      files <- input$data_files
      n <- nrow(files)

      # Detect overall strategy based on extensions first
      exts <- tolower(tools::file_ext(files$name))
      has_rdata <- any(exts %in% c("rdata"))
      has_rds <- any(exts %in% c("rds"))
      has_qza <- any(exts %in% c("qza"))
      has_tsv <- any(exts %in% c("tsv", "txt"))
      has_csv <- any(exts %in% c("csv"))
      has_xlsx <- any(exts %in% c("xlsx", "xls"))

      # Detect roles for table files
      roles <- NULL
      if (has_tsv || has_csv || has_xlsx) {
        auto_file_type("table")
        roles <- detect_file_roles(files)
        auto_detected_roles(roles)
      } else if (has_rdata || has_rds) {
        auto_file_type("rdata")
      } else if (has_qza) {
        auto_file_type("qiime2")
      } else {
        auto_file_type("unknown")
      }

      # Build file info table with detected role
      lang <- rv$current_language
      file_info <- data.frame(
        name = files$name,
        size = sapply(files$size, function(s) format(object.size(rep(1, s)), units = "auto")),
        ext = tolower(tools::file_ext(files$name)),
        role = sapply(files$name, function(fn) {
          if (!is.null(roles)) {
            if (!is.null(roles$feature)) {
              is_combined <- is.list(roles$feature) && !is.null(roles$feature$type) && roles$feature$type == "combined"
              if (is_combined && roles$feature$name == fn) return(tr("mod.import.abund_table_combined", lang))
            }
            if (!is.null(roles$feature) && is.data.frame(roles$feature) && roles$feature$name == fn) return(tr("mod.import.abund_table", lang))
            if (!is.null(roles$taxonomy) && roles$taxonomy$name == fn) return(tr("mod.import.tax_table", lang))
            if (!is.null(roles$sample) && roles$sample$name == fn) return(tr("mod.import.sample_info", lang))
            if (!is.null(roles$tree) && roles$tree$name == fn) return(tr("mod.import.phylogenetic_tree", lang))
            if (!is.null(roles$seq) && roles$seq$name == fn) return(tr("mod.import.rep_seqs", lang))
          }
          tolower(tools::file_ext(fn))
        }),
        stringsAsFactors = FALSE
      )

      auto_files_info(file_info)

      # Render file list table
      output$file_list <- DT::renderDataTable({
        fi <- auto_files_info()
        req(fi)
        DT::datatable(
          fi[, c("name", "size", "role"), drop = FALSE],
          options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
          colnames = c("文件名", "大小", "识别类型"),
          rownames = FALSE
        )
      })

      # Render detection results
      output$file_type_confirm <- renderUI({
        ft <- auto_file_type()
        req(ft)

        switch(ft,
          "rdata" = {
            tagList(
              tags$div(
                class = "alert alert-info",
                tags$strong("\U0001f4e6 检测到 R/RDS 文件"),
                tags$br(),
                tags$small("将扫描文件中的 microtable 对象并自动加载")
              ),
              shiny::uiOutput(ns("auto_rdata_scan"))
            )
          },
          "table" = {
            roles <- auto_detected_roles()
            req(roles)
            is_combined <- !is.null(roles$feature) && is.list(roles$feature) && roles$feature$type == "combined"
            tagList(
              if (is_combined) {
                tags$div(
                  class = "alert alert-info",
                  tags$strong("\U0001f50d 检测到混合文件"),
                  tags$br(),
                  tags$small(roles$feature$name, " - 将自动拆分为 Feature Table 和 Taxonomy")
                )
              },
              tags$div(
                class = "alert alert-success",
                tags$strong("\U0001f50d 自动识别结果"),
                tags$ul(
                  if (!is.null(roles$feature)) {
                    if (is_combined) {
                      tags$li(tags$span(style = "color:#27AE60;", "\u2705 Feature Table: "), roles$feature$name, tags$span(style = "color:#666;font-size:0.85em;", " (\u5300\u5408\u6587\u4ef6\u62c6\u5206)"))
                    } else {
                      tags$li(tags$span(style = "color:#27AE60;", "\u2705 Feature Table: "), roles$feature$name)
                    }
                  },
                  if (!is.null(roles$taxonomy)) {
                    if (is_combined) {
                      tags$li(tags$span(style = "color:#27AE60;", "\u2705 Taxonomy: "), roles$feature$name, tags$span(style = "color:#666;font-size:0.85em;", " (\u5300\u5408\u6587\u4ef6\u62c6\u5206)"))
                    } else {
                      tags$li(tags$span(style = "color:#27AE60;", "\u2705 Taxonomy: "), roles$taxonomy$name)
                    }
                  },
                  if (!is.null(roles$sample) && !is_combined) tags$li(tags$span(style = "color:#27AE60;", "\u2705 Sample Info: "), roles$sample$name),
                  if (!is.null(roles$tree)) tags$li(tags$span(style = "color:#27AE60;", "\u2705 Phylo Tree: "), roles$tree$name),
                  if (!is.null(roles$seq)) tags$li(tags$span(style = "color:#27AE60;", "\u2705 Sequences: "), roles$seq$name),
                  if (is.null(roles$feature)) tags$li(tags$span(style = "color:#E74C3C;", "\u274c 未找到 Feature Table（必需）"))
                )
              )
            )
          },
          "qiime2" = {
            tags$div(
              class = "alert alert-warning",
              tags$strong("\U0001f9ec 检测到 QIIME2 文件"),
              tags$br(),
              tags$small("QIIME2 导入需要 qiime2R 包，当前建议使用 RData 导入")
            )
          },
          "unknown" = {
            tags$div(
              class = "alert alert-warning",
              "\u26a0\ufe0f 无法识别文件类型，请尝试使用其他导入方式"
            )
          }
        )
      })
    }, ignoreNULL = TRUE)

    # Scan RData/RDS files (in observer, NOT in renderUI)
    observeEvent(input$data_files, {
      req(input$data_files)
      fi <- auto_files_info()
      req(fi)
      rdata_files <- fi[fi$ext %in% c("rdata", "rds"), ]

      if (nrow(rdata_files) == 0) return()

      all_files <- input$data_files
      scan_results <- list()

      for (i in seq_len(nrow(rdata_files))) {
        row <- rdata_files[i, ]
        file_idx <- which(all_files$name == row$name)[1]
        if (is.na(file_idx)) next

        filepath <- all_files$datapath[file_idx]
        filename <- all_files$name[file_idx]
        ext <- row$ext

        tmp_env <- new.env()
        scan_result <- tryCatch({
          if (ext == "rds") {
            obj <- readRDS(filepath)
            varname <- tools::file_path_sans_ext(filename)
            assign(varname, obj, envir = tmp_env)
          } else {
            load(filepath, envir = tmp_env)
          }
          list(success = TRUE, env = tmp_env)
        }, error = function(e) {
          list(success = FALSE, error = e$message)
        })

        if (!scan_result$success) {
          scan_results[[filename]] <- list(
            success = FALSE, error = scan_result$error,
            obj_names = character(0), mt_names = character(0)
          )
          next
        }

        obj_names <- ls(scan_result$env)
        mt_names <- obj_names[sapply(obj_names, function(n) {
          inherits(scan_result$env[[n]], "microtable")
        })]

        scan_results[[filename]] <- list(
          success = TRUE, env = scan_result$env,
          obj_names = obj_names, mt_names = mt_names,
          file_idx = file_idx
        )
      }

      auto_rdata_scan_results(scan_results)
      auto_rdata_envs(scan_results)

      # Render scan results UI
      output$auto_rdata_scan <- renderUI({
        results <- auto_rdata_scan_results()
        req(length(results) > 0)

        tagList(
          lapply(names(results), function(filename) {
            res <- results[[filename]]
            if (!res$success) {
              return(tags$div(
                style = "margin: 10px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
                tags$strong(filename),
                tags$div(
                  class = "alert alert-danger",
                  style = "margin-top:5px;font-size:0.8rem;",
                  paste0("读取失败: ", res$error)
                )
              ))
            }

            obj_names <- res$obj_names
            mt_names <- res$mt_names
            file_idx <- res$file_idx

            if (length(obj_names) == 0) {
              return(tags$div(
                style = "margin: 10px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
                tags$strong(filename),
                tags$div(
                  style = "font-size:0.8rem;color:#888;margin-top:5px;",
                  "\u6587\u4ef6\u4e2d\u672a\u627e\u5230\u5bf9\u8c61"
                )
              ))
            }

            tagList(
              tags$div(
                style = "margin: 10px 0; padding: 10px; background: #f8f9fa; border-radius: 6px;",
                tags$strong(filename),
                if (length(mt_names) > 0) {
                  tags$div(
                    style = "font-size:0.8rem;color:#27AE60;margin-top:5px;",
                    paste0("\u2705 \u627e\u5230 ", length(mt_names), " \u4e2a microtable \u5bf9\u8c61: ", paste(mt_names, collapse = ", "))
                  )
                } else {
                  tags$div(
                    style = "font-size:0.8rem;color:#F39C12;margin-top:5px;",
                    paste0("\u26a0\ufe0f \u672a\u627e\u5230 microtable \u5bf9\u8c61\uff0c\u627e\u5230 ", length(obj_names), " \u4e2a\u666e\u901a\u5bf9\u8c61")
                  )
                },
                shiny::selectInput(
                  inputId = ns(paste0("auto_obj_", file_idx)),
                  label = NULL,
                  choices = obj_names,
                  selected = if (length(mt_names) > 0) mt_names[1] else obj_names[1]
                )
              )
            )
          })
        )
      })
    }, ignoreNULL = TRUE)

    # Helper function to read tables with auto-detection
    read_table_auto <- function(filepath, force_numeric = FALSE) {
      ext <- tolower(tools::file_ext(filepath))
      
      data <- switch(ext,
        "csv" = readr::read_csv(filepath, show_col_types = FALSE),
        "tsv" = readr::read_tsv(filepath, show_col_types = FALSE),
        "txt" = readr::read_tsv(filepath, show_col_types = FALSE),
        "xlsx" = readxl::read_excel(filepath),
        "xls" = {
          tryCatch({
            readxl::read_excel(filepath)
          }, error = function(e) {
            stop(paste0(
              "无法读取 .xls 文件。可能原因：\n",
              "1. 文件格式不兼容旧版 Excel 格式\n",
              "2. 文件已损坏或受保护\n\n",
              "建议解决方案：\n",
              "- 在 Excel 中打开文件，另存为 .xlsx 格式\n",
              "- 或导出为 .csv 格式后重新导入\n\n",
              "原始错误: ", e$message
            ))
          })
        },
        stop(paste0("不支持的文件扩展名: ", ext))
      )
      
      # 强制转换为传统 data.frame（避免 tbl_df 引起后续错误）
      data <- as.data.frame(data, stringsAsFactors = FALSE)
      
      # 设置第一列为行名并删除
      if (nrow(data) > 0) {
        rownames(data) <- data[[1]]
        data <- data[, -1, drop = FALSE]
        
        # 仅在读取特征丰度表时强制转换为数值型
        if (force_numeric) {
          result <- convert_to_numeric_safe(data)
          data <- result$df
        }
      }
      
      data
    }

    # Build microtable from auto import
    observeEvent(input$build_auto, {
      ft <- auto_file_type()
      req(ft)

      if (ft == "rdata") {
        fi <- auto_files_info()
        req(fi)
        rdata_files <- fi[fi$ext %in% c("rdata", "rds"), ]
        req(nrow(rdata_files) > 0)

        all_files <- input$data_files
        file_idx <- which(all_files$name == rdata_files$name[1])[1]
        if (is.na(file_idx)) {
          showNotification(tr("mod.import.notify.file_not_found", rv$current_language), type = "error")
          return()
        }

        obj_input_id <- paste0("auto_obj_", file_idx)
        obj_name <- input[[obj_input_id]]
        req(obj_name)

        scan_results <- auto_rdata_scan_results()
        filename <- rdata_files$name[1]
        scan_data <- scan_results[[filename]]
        req(scan_data)
        req(scan_data$success)

        env <- scan_data$env
        obj <- env[[obj_name]]

        if (inherits(obj, "microtable")) {
          rv$microtable <- obj
          rv$microtable_name <- obj_name
          rv$data_loaded <- TRUE
          append_code(rv, paste0('# Auto import: load ', obj_name, ' from ', filename), "Auto import")
          showNotification(sprintf(tr("mod.import.notify.microtable_loaded", rv$current_language), obj_name), type = "message")
          update_summary()
        } else if (is.data.frame(obj)) {
          result <- safe_run({
            mt <- microeco::microtable$new(otu_table = obj)
            mt
          }, "\u6784\u5efa microtable \u5931\u8d25")
          if (result$success) {
            rv$microtable <- result$result
            rv$microtable_name <- obj_name
            rv$data_loaded <- TRUE
            append_code(rv, paste0('# Auto import: build microtable from data.frame in ', filename), "Auto import")
            showNotification(sprintf(tr("mod.import.notify.microtable_from_df", rv$current_language), obj_name), type = "message")
            update_summary()
          } else {
            showNotification(result$error, type = "error", duration = 10)
          }
        } else {
          showNotification(sprintf(tr("mod.import.notify.invalid_object", rv$current_language), obj_name, paste(class(obj), collapse = "/")), type = "error")
        }

      } else if (ft == "table") {
        roles <- auto_detected_roles()
        req(roles)

        if (is.null(roles$feature)) {
          showNotification(tr("mod.import.notify.no_feature_table", rv$current_language), type = "error")
          return()
        }

        result <- shiny::withProgress(message = "\u6b63\u5728\u6784\u5efa microtable", value = 0, {
          is_combined <- !is.null(roles$feature) && is.list(roles$feature) && roles$feature$type == "combined"

          if (is_combined) {
            shiny::incProgress(0.1, detail = "\u6b63\u5728\u8bfb\u53d6\u5e76\u62c6\u5206\u6df7\u5408\u6587\u4ef6...")
            combined_data <- read_combined_file(roles$feature)
            feat <- combined_data$feature
            tax <- combined_data$taxonomy
          } else {
            shiny::incProgress(0.1, detail = "\u6b63\u5728\u8bfb\u53d6 Feature Table...")
            feat_file <- input$data_files[which(input$data_files$name == roles$feature$name), ]
            feat <- read_table_auto(feat_file$datapath, force_numeric = TRUE)

            tax <- NULL
            if (!is.null(roles$taxonomy)) {
              shiny::incProgress(0.2, detail = "\u6b63\u5728\u8bfb\u53d6 Taxonomy...")
              tax_file <- input$data_files[which(input$data_files$name == roles$taxonomy$name), ]
              if (grepl("taxonomy", tolower(tax_file$name))) {
                tax <- read_taxonomy_file(tax_file$datapath)
              } else {
                tax <- read_table_auto(tax_file$datapath, force_numeric = FALSE)
              }
            }
          }

          sample <- NULL
          if (!is.null(roles$sample)) {
            shiny::incProgress(0.2, detail = "\u6b63\u5728\u8bfb\u53d6 Sample Info...")
            samp_file <- input$data_files[which(input$data_files$name == roles$sample$name), ]
            sample <- read_table_auto(samp_file$datapath, force_numeric = FALSE)
          }

          tree <- NULL
          if (!is.null(roles$tree)) {
            shiny::incProgress(0.2, detail = "\u6b63\u5728\u8bfb\u53d6 Phylo Tree...")
            tree_file <- input$data_files[which(input$data_files$name == roles$tree$name), ]
            tree <- ape::read.tree(tree_file$datapath)
          }

          seq <- NULL
          if (!is.null(roles$seq)) {
            shiny::incProgress(0.1, detail = "\u6b63\u5728\u8bfb\u53d6 Sequences...")
            seq_file <- input$data_files[which(input$data_files$name == roles$seq$name), ]
            seq <- Biostrings::readDNAStringSet(seq_file$datapath)
          }

          shiny::incProgress(0.2, detail = "\u6b63\u5728\u6784\u5efa microtable \u5bf9\u8c61...")
          mt <- microeco::microtable$new(
            otu_table = feat,
            tax_table = tax,
            sample_table = sample,
            phylo_tree = tree,
            rep_fasta = seq,
            auto_tidy = TRUE
          )
          shiny::incProgress(0.1, detail = "\u5b8c\u6210\uff01")
          list(success = TRUE, result = mt)
        })

        if (result$success) {
          rv$microtable <- result$result
          rv$microtable_name <- "auto_import"
          rv$data_loaded <- TRUE
          append_code(rv, '# Auto import: build microtable from TSV/CSV file', "Auto import")
          showNotification(tr("mod.import.notify.microtable_from_table", rv$current_language), type = "message")
          update_summary()
        } else {
          showNotification(result$error, type = "error", duration = 10)
        }
      } else {
        showNotification(tr("mod.import.notify.auto_import_unsupported", rv$current_language), type = "warning")
      }
    })

    # ==================== Built-in Data ====================
    observeEvent(input$load_builtin, {
      result <- safe_run({
        data(list = "dataset", package = "microeco")
        obj <- get("dataset", envir = asNamespace("microeco"))
        if (!inherits(obj, "microtable")) {
          stop("内置数据集不是 microtable 对象")
        }
        obj
      }, "加载内置数据失败")
      
      if (result$success) {
        rv$microtable <- result$result
        rv$microtable_name <- "microeco_dataset"
        rv$data_loaded <- TRUE
        append_code(rv, 'data(dataset, package = "microeco")\nmicroeco_dataset <- clone(dataset)', "Data import - Built-in data")
        showNotification(tr("mod.import.notify.builtin_loaded", rv$current_language), type = "message")
        update_summary()
      } else {
        showNotification(result$error, type = "error", duration = 10)
      }
    })

    # ==================== QIIME2 Import (placeholder) ====================
    observeEvent(input$build_qiime2, {
      showNotification(tr("mod.import.notify.qiime2_developing", rv$current_language), type = "warning", duration = 5)
    })

    # ==================== QIIME2 TSV Import (placeholder) ====================
    observeEvent(input$build_tsv, {
      showNotification(tr("mod.import.notify.qiime2_tsv_developing", rv$current_language), type = "warning", duration = 5)
    })

  })
}