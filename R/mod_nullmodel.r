#' @title Null Model Analysis Module UI
#' @description
#' Provides interface for null model analysis including NTI, NRI, betaNTI, betaNRI,
#' RCbray calculations, mantel correlogram, NST, and ecological process inference.
#' Supports complete parameter options from trans_nullmodel class.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @importFrom shinyFiles getVolumes parseDirPath shinyDirButton shinyDirChoose
#' @keywords null model phylogenetics betaNTI NTI microbiome
#' @family advanced-analysis
mod_nullmodel_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(column(12, h2(tr("\U0001f3b2 零模型分析 Null Model Analysis", "\U0001f3b2 Null Model Analysis")))),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4cb 参数设置", "\U0001f4cb Parameters"),
          status = "primary", solidHeader = TRUE, width = NULL, collapsible = FALSE,
          h4(tr("基本参数", "Basic Parameters")),
          fluidRow(
            column(3, shiny::numericInput(ns("null_filter_thres"), tr("filter_thres (相对丰度阈值)", "filter_thres (RA threshold)"),
              value = 0, min = 0, max = 1, step = 0.0001)),
            column(3, shiny::numericInput(ns("null_taxa_number"), tr("taxa_number (笛组数)", "taxa_number (group number)"),
              value = NA, min = 1)),
            column(3, shiny::selectInput(ns("null_group"), tr("分组列", "Group Column"),
              choices = character(0))),
            column(3, shiny::selectInput(ns("null_select_group"), "select_group",
              choices = character(0), multiple = TRUE))
          ),
          fluidRow(
            column(3, shinyWidgets::pickerInput(ns("null_env_cols"), tr("env_cols (环境因子)", "env_cols"),
              choices = character(0), selected = character(0), multiple = TRUE,
              options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE))),
            column(3, shinyWidgets::materialSwitch(ns("null_complete_na"), "complete_na",
              value = FALSE, status = "info")),
            column(6)
          ),
          hr(),
          h4(tr("分析类型", "Analysis Type")),
          fluidRow(
            column(12, shiny::radioButtons(ns("null_analysis_type"), tr("分析类型", "Analysis Type"),
              choices = setNames(c("mantel_corr", "ses_betampd", "ses_betamntd", "rcbray", "nri", "nti", "cscore", "nst", "process"),
                                 c(tr("Mantel相关树", "Mantel Correlogram"), tr("betaMPD/betaNRI", "betaMPD/betaNRI"),
                                   tr("betaMNTD/betaNTI", "betaMNTD/betaNTI"), "RCbray",
                                   tr("NRI (进化指数)", "NRI (Net Relatedness Index)"), tr("NTI (近缘物种指数)", "NTI (Nearest Taxon Index)"),
                                   "C-score", "NST", tr("生态过程推断", "Ecological Process Inference"))),
              selected = "ses_betamntd", inline = TRUE))
          ),
          shiny::conditionalPanel(condition = "input.null_analysis_type == 'mantel_corr'", ns = ns,
            h4(paste0("cal_mantel_corr ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shiny::selectInput(ns("null_mantel_use_env"), "use_env",
                choices = character(0))),
              column(3, shiny::textInput(ns("null_mantel_break_pts"), "break.pts",
                value = "seq(0, 1, 0.02)"))
            )
          ),
          shiny::conditionalPanel(condition = "input.null_analysis_type == 'ses_betampd' || input.null_analysis_type == 'ses_betamntd' || input.null_analysis_type == 'rcbray' || input.null_analysis_type == 'process'", ns = ns,
            h4(tr("置换模型参数", "Null Model Parameters")),
            fluidRow(
              column(2, shiny::numericInput(ns("null_runs"), tr("runs (模拟次数)", "runs (permutations)"),
                value = 1000, min = 99, max = 9999)),
              column(3, shiny::selectInput(ns("null_null_model"), "null.model",
                choices = c("taxa.labels", "richness", "frequency", "sample.pool",
                  "phylogeny.pool", "independentswap", "trialswap"),
                selected = "taxa.labels")),
              column(2, shinyWidgets::materialSwitch(ns("null_abundance_weighted"), "abundance.weighted",
                value = TRUE, status = "primary")),
              column(2, shinyWidgets::materialSwitch(ns("null_exclude_conspec"), "exclude.conspecifics",
                value = FALSE, status = "info")),
              column(3)
            )
          ),
          shiny::conditionalPanel(condition = "input.null_analysis_type == 'ses_betamntd' && input.null_runs > 5000", ns = ns,
            h4(paste0("iCAMP ", tr("加速参数", "Speed-up Parameters"))),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("null_use_icamp"), "use_iCAMP",
                value = FALSE, status = "warning")),
              column(3, shiny::numericInput(ns("null_nworker"), "nworker",
                value = 2, min = 1, max = 16)),
              column(6)
            )
          ),
          shiny::conditionalPanel(condition = "input.null_analysis_type == 'nri' || input.null_analysis_type == 'nti'", ns = ns,
            h4(paste0("cal_NRI / cal_NTI ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shiny::selectInput(ns("null_nri_null_model"), "null.model",
                choices = c("taxa.labels", "richness", "frequency", "sample.pool",
                  "phylogeny.pool", "independentswap", "trialswap"),
                selected = "taxa.labels")),
              column(3, shinyWidgets::materialSwitch(ns("null_nri_weighted"), "abundance.weighted",
                value = FALSE, status = "info")),
              column(3, shiny::numericInput(ns("null_nri_runs"), "runs",
                value = 999, min = 99, max = 9999)),
              column(3)
            )
          ),
          shiny::conditionalPanel(condition = "input.null_analysis_type == 'cscore'", ns = ns,
            h4(paste0("cal_Cscore ", tr("参数", "Parameters"))),
            fluidRow(
              column(4, shiny::selectInput(ns("null_cscore_group"), "by_group",
                choices = character(0))),
              column(8)
            )
          ),
          shiny::conditionalPanel(condition = "input.null_analysis_type == 'nst'", ns = ns,
            h4(paste0("cal_NST ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shiny::selectInput(ns("null_nst_method"), "method",
                choices = c("tNST", "pNST"), selected = "tNST")),
              column(3, shiny::selectInput(ns("null_nst_group"), "group",
                choices = character(0))),
              column(6)
            )
          ),
          shiny::conditionalPanel(condition = "input.null_analysis_type == 'process'", ns = ns,
            h4(paste0("cal_process ", tr("参数", "Parameters"))),
            fluidRow(
              column(4, shinyWidgets::materialSwitch(ns("null_use_betamntd"), "use_betamntd (betaNTI)",
                value = TRUE, status = "primary")),
              column(4, shiny::selectInput(ns("null_process_group"), "group",
                choices = character(0))),
              column(4)
            )
          ),
          hr(),
          h4(tr("图片保存与下载", "Save & Download Plot")),
          fluidRow(
            column(2, shiny::actionButton(ns("run_nullmodel"), tr("\U0001f4ca 执行", "\U0001f4ca Run"),
              icon = icon("play"), class = "btn-primary", width = "100%")),
            column(2, shiny::selectInput(ns("null_image_format"), tr("格式", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("null_save_width"), tr("宽", "Width"), value = 10, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("null_save_height"), tr("高", "Height"), value = 7, min = 4, max = 15)),
            column(2, shiny::numericInput(ns("null_save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(2, shiny::actionButton(ns("null_save_plot_btn"), tr("\U0001f4e5保存图片", "\U0001f4e5Save Plot"),
              icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
          ),
          fluidRow(
            column(2, shiny::selectInput(ns("null_table_format"), tr("表格", "Table"),
              choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
            column(2, shiny::downloadButton(ns("null_download_table"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
              class = "btn-outline-info", width = "100%")),
            column(8)
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
          shinycssloaders::withSpinner(shiny::plotOutput(ns("null_plot"), height = "550px"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("null_table"))
        )
      )
    )
  )
}

mod_nullmodel_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(
      plot = NULL,
      data_result = NULL,
      save_dir = NULL,
      result_obj = NULL,
      table_type = "default"
    )

    volumes <- c(
      Home      = path.expand("~"),
      Desktop   = file.path(path.expand("~"), "Desktop"),
      Documents = file.path(path.expand("~"), "Documents"),
      get_volumes_safe()
    )
    shinyFiles::shinyDirChoose(input, "null_save_dir_btn", roots = volumes,
      session = session, defaultRoot = "Desktop")

    observeEvent(input$null_save_dir_btn, {
      parsed <- shinyFiles::parseDirPath(volumes, input$null_save_dir_btn)
      if (!is.null(parsed) && length(parsed) > 0 && nchar(parsed) > 0) {
        local_rv$save_dir <- parsed
        updateTextInput(session, "null_save_dir_text", value = parsed)
      }
    })

    observeEvent(input$null_save_plot_btn, {
      default_name <- paste0("nullmodel_", input$null_analysis_type, ".", input$null_image_format)
      current_dir <- isolate(local_rv$save_dir)
      dir_display <- if (!is.null(current_dir) && nchar(current_dir) > 0) current_dir else ""

      showModal(modalDialog(
        title = "\U0001f4be \u4fdd\u5b58\u56fe\u7247",
        size = "m",
        easyClose = TRUE,
        fluidRow(
          column(12,
            tags$label(class = "control-label", style = "font-weight: bold;",
              "\U0001f4c1 \u4fdd\u5b58\u6587\u4ef6\u5939"),
            fluidRow(
              column(9,
                shiny::textInput(ns("null_save_dir_text"), label = NULL, value = dir_display,
                  placeholder = "\u8bf7\u70b9\u51fb\u53f3\u4fa7\u6309\u94ae\u9009\u62e9\u6587\u4ef6\u5939...",
                  width = "100%")
              ),
              column(3, style = "padding-top: 0;",
                shinyFiles::shinyDirButton(ns("null_save_dir_btn"), "\u6d4f\u89c8",
                  "\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939",
                  class = "btn-outline-primary", icon = icon("folder-open"),
                  style = "margin-top: 25px;")
              )
            )
          )
        ),
        tags$hr(style = "margin: 12px 0;"),
        fluidRow(
          column(6, shiny::textInput(ns("null_save_filename"), "\U0001f4dd \u6587\u4ef6\u540d\u79f0",
            value = default_name)),
          column(6, tags$div(style = "padding-top: 28px;",
            tags$p(class = "text-muted", style = "margin-bottom: 0;",
              tags$span(icon("info-circle")), " \u683c\u5f0f: ",
              tags$code(toupper(input$null_image_format)),
              " | \u5bbd\u00d7\u9ad8: ",
              tags$code(paste0(input$null_save_width, "\u00d7", input$null_save_height)),
              " | DPI: ", tags$code(input$null_save_dpi))
          ))
        ),
        footer = tagList(
          shiny::actionButton(ns("null_confirm_save"), "\u2705 \u4fdd\u5b58\u5230\u6587\u4ef6\u5939",
            icon = icon("save"), class = "btn-primary"),
          shiny::modalButton("\u53d6\u6d88")
        )
      ))
    })

    observeEvent(input$null_confirm_save, {
      req(local_rv$plot)
      save_dir <- local_rv$save_dir
      if (is.null(save_dir) || !isTRUE(nchar(save_dir) > 0)) {
        showNotification("\u8bf7\u5148\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939", type = "warning")
        return()
      }
      fname <- input$null_save_filename
      if (is.null(fname) || !isTRUE(nzchar(trimws(fname)))) {
        fname <- paste0("nullmodel_", input$null_analysis_type)
      }
      fname <- trimws(fname)
      ext <- input$null_image_format
      if (!grepl(paste0("\\.", ext, "$"), fname, ignore.case = TRUE)) {
        fname <- sub("\\.(png|pdf|svg|tiff|tif)$", "", fname, ignore.case = TRUE)
        fname <- paste0(fname, ".", ext)
      }
      if (!dir.exists(save_dir)) {
        showNotification("\u6587\u4ef6\u5939\u4e0d\u5b58\u5728\uff0c\u8bf7\u91cd\u65b0\u9009\u62e9", type = "error")
        return()
      }
      full_path <- file.path(save_dir, fname)

      tryCatch({
        ggplot2::ggsave(filename = full_path, plot = local_rv$plot,
          width = input$null_save_width, height = input$null_save_height,
          units = "in", dpi = input$null_save_dpi, scale = 1)
        removeModal()
        showNotification(paste0("\u2705 \u5df2\u4fdd\u5b58\u81f3: ", full_path), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("\u4fdd\u5b58\u5931\u8d25: ", e$message), type = "error", duration = 10)
      })
    })

    observe({
      if (!check_microtable(rv)) {
        updateSelectInput(session, "null_group", choices = character(0))
        updateSelectInput(session, "null_select_group", choices = character(0), selected = character(0))
        updatePickerInput(session, "null_env_cols", choices = character(0), selected = character(0))
        updateSelectInput(session, "null_mantel_use_env", choices = character(0))
        updateSelectInput(session, "null_cscore_group", choices = character(0))
        updateSelectInput(session, "null_nst_group", choices = character(0))
        updateSelectInput(session, "null_process_group", choices = character(0))
        return()
      }
      cols <- get_sample_cols(rv)
      updateSelectInput(session, "null_group", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "null_select_group", choices = character(0), selected = character(0))
      updatePickerInput(session, "null_env_cols", choices = cols, selected = character(0))
      updateSelectInput(session, "null_mantel_use_env", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "null_cscore_group", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "null_nst_group", choices = cols)
      updateSelectInput(session, "null_process_group", choices = c("\u65e0" = "", cols))
    })

    observe({
      if (!check_microtable(rv)) return()
      if (!isTRUE(nzchar(input$null_group))) {
        updateSelectInput(session, "null_select_group", choices = character(0), selected = character(0))
        return()
      }
      vals <- unique(rv$microtable$sample_table[, input$null_group])
      updateSelectInput(session, "null_select_group", choices = vals, selected = vals)
    })

    observeEvent(input$run_nullmodel, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      if (is.null(rv$microtable$phylo_tree) && input$null_analysis_type != "rcbray" && input$null_analysis_type != "cscore" && input$null_analysis_type != "nst") {
        showNotification("\u96f6\u6a21\u578b\u5206\u6790\u9700\u8981\u8fdb\u5316\u6811\u6570\u636e\uff08phylo_tree\uff09", type = "error")
        return()
      }

      analysis <- input$null_analysis_type
      result <- tryCatch({
        dataset_name <- rv$microtable_name %||% "tmp_microtable"

        taxa_num_val <- {
          val <- input$null_taxa_number
          if (is.null(val) || is.na(val) || val <= 0) NULL else val
        }
        filter_thres_val <- {
          val <- input$null_filter_thres
          if (is.null(val) || val <= 0) 0 else val
        }
        env_cols_val <- if (length(input$null_env_cols) > 0) input$null_env_cols else NULL
        select_group_val <- if (length(input$null_select_group) > 0) input$null_select_group else NULL

        t_null <- microeco::trans_nullmodel$new(
          dataset = rv$microtable,
          filter_thres = filter_thres_val,
          taxa_number = taxa_num_val,
          group = if (isTRUE(nzchar(input$null_group))) input$null_group else NULL,
          select_group = select_group_val,
          env_cols = env_cols_val,
          complete_na = input$null_complete_na
        )

        init_code <- paste0(
          "t_null <- microeco::trans_nullmodel$new(\n",
          "  dataset = ", dataset_name, ",\n",
          "  filter_thres = ", filter_thres_val, ",\n",
          if (!is.null(taxa_num_val)) paste0("  taxa_number = ", taxa_num_val, ",\n") else "",
          if (isTRUE(nzchar(input$null_group))) paste0("  group = \"", input$null_group, "\",\n") else "",
          "  complete_na = ", input$null_complete_na, "\n",
          ")\n"
        )

        if (analysis == "mantel_corr") {
          use_env_val <- if (isTRUE(nzchar(input$null_mantel_use_env))) input$null_mantel_use_env else NULL

          t_null$cal_mantel_corr(use_env = use_env_val)

          p <- t_null$plot_mantel_corr()

          mantel_code <- paste0(
            "t_null$cal_mantel_corr(\n",
            "  use_env = ", if (!is.null(use_env_val)) paste0("\"", use_env_val, "\"") else "NULL", "\n",
            ")\n",
            "p <- t_null$plot_mantel_corr()\n"
          )
          code <- paste0(init_code, "# Mantel\u76f8\u5173\u6811\n", mantel_code)

          list(success = TRUE, plot = p, data_result = t_null$res_mantel_corr$mantel.res,
               result_obj = t_null, code = code, step = "mantel_corr")

        } else if (analysis == "ses_betampd") {
          t_null$cal_ses_betampd(
            runs = input$null_runs,
            null.model = input$null_null_model,
            abundance.weighted = input$null_abundance_weighted
          )

          local_rv$table_type <- "ses_betampd"

          ses_code <- paste0(
            "t_null$cal_ses_betampd(\n",
            "  runs = ", input$null_runs, ",\n",
            "  null.model = \"", input$null_null_model, "\",\n",
            "  abundance.weighted = ", input$null_abundance_weighted, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# betaMPD/betaNRI\n", ses_code)

          list(success = TRUE, plot = NULL, data_result = t_null$res_ses_betampd,
               result_obj = t_null, code = code, step = "ses_betampd")

        } else if (analysis == "ses_betamntd") {
          t_null$cal_ses_betamntd(
            runs = input$null_runs,
            null.model = input$null_null_model,
            abundance.weighted = input$null_abundance_weighted,
            exclude.conspecifics = input$null_exclude_conspec,
            use_iCAMP = input$null_use_icamp,
            nworker = input$null_nworker
          )

          local_rv$table_type <- "ses_betamntd"

          ses_code <- paste0(
            "t_null$cal_ses_betamntd(\n",
            "  runs = ", input$null_runs, ",\n",
            "  null.model = \"", input$null_null_model, "\",\n",
            "  abundance.weighted = ", input$null_abundance_weighted, ",\n",
            "  exclude.conspecifics = ", input$null_exclude_conspec, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# betaMNTD/betaNTI\n", ses_code)

          list(success = TRUE, plot = NULL, data_result = t_null$res_ses_betamntd,
               result_obj = t_null, code = code, step = "ses_betamntd")

        } else if (analysis == "rcbray") {
          t_null$cal_rcbray(
            runs = input$null_runs,
            verbose = TRUE,
            null.model = input$null_null_model
          )

          local_rv$table_type <- "rcbray"

          rcbray_code <- paste0(
            "t_null$cal_rcbray(\n",
            "  runs = ", input$null_runs, ",\n",
            "  null.model = \"", input$null_null_model, "\"\n",
            ")\n"
          )
          code <- paste0(init_code, "# RCbray\n", rcbray_code)

          list(success = TRUE, plot = NULL, data_result = t_null$res_rcbray,
               result_obj = t_null, code = code, step = "rcbray")

        } else if (analysis == "nri") {
          t_null$cal_NRI(
            null.model = input$null_nri_null_model,
            abundance.weighted = input$null_nri_weighted,
            runs = input$null_nri_runs
          )

          local_rv$table_type <- "nri"

          nri_code <- paste0(
            "t_null$cal_NRI(\n",
            "  null.model = \"", input$null_nri_null_model, "\",\n",
            "  abundance.weighted = ", input$null_nri_weighted, ",\n",
            "  runs = ", input$null_nri_runs, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# NRI\n", nri_code)

          list(success = TRUE, plot = NULL, data_result = t_null$res_NRI,
               result_obj = t_null, code = code, step = "nri")

        } else if (analysis == "nti") {
          t_null$cal_NTI(
            null.model = input$null_nri_null_model,
            abundance.weighted = input$null_nri_weighted,
            runs = input$null_nri_runs
          )

          local_rv$table_type <- "nti"

          nti_code <- paste0(
            "t_null$cal_NTI(\n",
            "  null.model = \"", input$null_nri_null_model, "\",\n",
            "  abundance.weighted = ", input$null_nri_weighted, ",\n",
            "  runs = ", input$null_nri_runs, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# NTI\n", nti_code)

          list(success = TRUE, plot = NULL, data_result = t_null$res_NTI,
               result_obj = t_null, code = code, step = "nti")

        } else if (analysis == "cscore") {
          cscore_group_val <- if (nchar(input$null_cscore_group)) input$null_cscore_group else NULL
          res_cscore <- t_null$cal_Cscore(by_group = cscore_group_val)

          local_rv$table_type <- "cscore"

          cscore_code <- paste0(
            "t_null$cal_Cscore(\n",
            "  by_group = ", if (!is.null(cscore_group_val)) paste0("\"", cscore_group_val, "\"") else "NULL", "\n",
            ")\n"
          )
          code <- paste0(init_code, "# C-score\n", cscore_code)

          list(success = TRUE, plot = NULL, data_result = data.frame(C_score = res_cscore),
               result_obj = t_null, code = code, step = "cscore")

        } else if (analysis == "nst") {
          if (isTRUE(nchar(input$null_nst_group) == 0)) {
            stop("\u8bf7\u9009\u62e9NST\u7684group\u53c2\u6570")
          }

          t_null$cal_NST(
            method = input$null_nst_method,
            group = input$null_nst_group
          )

          local_rv$table_type <- "nst"

          nst_code <- paste0(
            "t_null$cal_NST(\n",
            "  method = \"", input$null_nst_method, "\",\n",
            "  group = \"", input$null_nst_group, "\"\n",
            ")\n"
          )
          code <- paste0(init_code, "# NST\n", nst_code)

          list(success = TRUE, plot = NULL, data_result = t_null$res_NST$index.pair,
               result_obj = t_null, code = code, step = "nst")

        } else if (analysis == "process") {
          if (is.null(t_null$res_ses_betamntd)) {
            t_null$cal_ses_betamntd(
              runs = input$null_runs,
              null.model = input$null_null_model,
              abundance.weighted = input$null_abundance_weighted
            )
          }
          if (is.null(t_null$res_rcbray)) {
            t_null$cal_rcbray(
              runs = input$null_runs,
              verbose = TRUE,
              null.model = input$null_null_model
            )
          }

          t_null$cal_process(
            use_betamntd = input$null_use_betamntd,
            group = if (nchar(input$null_process_group)) input$null_process_group else NULL
          )

          local_rv$table_type <- "process"

          p <- ggplot2::ggplot(t_null$res_process, ggplot2::aes(x = process, y = percentage, fill = process)) +
            ggplot2::geom_bar(stat = "identity", width = 0.7) +
            ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percentage)), vjust = -0.3, size = 4) +
            ggplot2::scale_fill_brewer(palette = "Set2") +
            ggplot2::theme_bw() +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 11),
              legend.position = "none"
            ) +
            ggplot2::labs(x = NULL, y = "Percentage (%)", title = "\U751f\u6001\u8fc7\u7a0b\u5206\u914d")

          process_code <- paste0(
            "t_null$cal_process(\n",
            "  use_betamntd = ", input$null_use_betamntd, ",\n",
            "  group = ", if (nchar(input$null_process_group)) paste0("\"", input$null_process_group, "\"") else "NULL", "\n",
            ")\n"
          )
          code <- paste0(init_code, "# \u751f\u6001\u8fc7\u7a0b\u63a8\u65ad\n", process_code)

          list(success = TRUE, plot = p, data_result = t_null$res_process,
               result_obj = t_null, code = code, step = "process")

        } else {
          stop("\u672a\u77e5\u5206\u6790\u7c7b\u578b")
        }
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      append_code(rv, result$code, paste0("\u96f6\u6a21\u578b - ", result$step))
      local_rv$plot <- result$plot
      local_rv$data_result <- result$data_result
      local_rv$result_obj <- result$result_obj
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$null_plot <- shiny::renderPlot({
      req(local_rv$plot)
      if (is(local_rv$plot, "ggplot")) {
        print(local_rv$plot)
      } else {
        print(local_rv$plot)
      }
    })

    output$null_table <- DT::renderDataTable({
      dt <- local_rv$data_result
      if (is.data.frame(dt) || is.matrix(dt)) {
        dt_df <- as.data.frame(dt)
        DT::datatable(dt_df, options = list(scrollX = TRUE, pageLength = 20),
          rownames = TRUE, filter = "top")
      }
    })

    output$null_download_table <- downloadHandler(
      filename = function() {
        ext <- ifelse(input$null_table_format == ",", ".csv", ".tsv")
        paste0("nullmodel_", local_rv$table_type, ext)
      },
      content = function(file) {
        dt <- local_rv$data_result
        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$null_table_format,
            row.names = TRUE, quote = TRUE)
        }
      }
    )
  })
}