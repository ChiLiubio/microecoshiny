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
            column(3, shiny::numericInput(ns("null_taxa_number"), tr("taxa_number (组数)", "taxa_number (group number)"),
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
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_null_mantel_corr"), tr("\U0001f3c1 执行分析", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("null_mantel_table"))
                )
              )
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("null_mantel_plot"), height = "550px"))
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "input.null_analysis_type == 'ses_betampd' || input.null_analysis_type == 'ses_betamntd' || input.null_analysis_type == 'rcbray' || input.null_analysis_type == 'nri' || input.null_analysis_type == 'nti' || input.null_analysis_type == 'cscore' || input.null_analysis_type == 'nst'", ns = ns,
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
              fluidRow(
                column(4, shiny::selectInput(ns("null_cscore_group"), "by_group",
                  choices = character(0))),
                column(8)
              )
            ),
            shiny::conditionalPanel(condition = "input.null_analysis_type == 'nst'", ns = ns,
              fluidRow(
                column(3, shiny::selectInput(ns("null_nst_method"), "method",
                  choices = c("tNST", "pNST"), selected = "tNST")),
                column(3, shiny::selectInput(ns("null_nst_group"), "group",
                  choices = character(0))),
                column(6)
              )
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_null_ses"), tr("\U0001f3c1 执行分析", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("null_ses_table"))
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "input.null_analysis_type == 'process'", ns = ns,
            h4(paste0("cal_process ", tr("参数", "Parameters"))),
            fluidRow(
              column(2, shiny::numericInput(ns("null_process_runs"), tr("runs (模拟次数)", "runs (permutations)"),
                value = 1000, min = 99, max = 9999)),
              column(3, shiny::selectInput(ns("null_process_null_model"), "null.model",
                choices = c("taxa.labels", "richness", "frequency", "sample.pool",
                  "phylogeny.pool", "independentswap", "trialswap"),
                selected = "taxa.labels")),
              column(2, shinyWidgets::materialSwitch(ns("null_process_abundance_weighted"), "abundance.weighted",
                value = TRUE, status = "primary")),
              column(2, shinyWidgets::materialSwitch(ns("null_use_betamntd"), "use_betamntd (betaNTI)",
                value = TRUE, status = "primary")),
              column(3)
            ),
            fluidRow(
              column(4, shiny::selectInput(ns("null_process_group"), "group",
                choices = character(0))),
              column(8)
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_null_process"), tr("\U0001f3c1 执行分析", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("null_process_table"))
                )
              )
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("null_process_plot"), height = "550px"))
                )
              )
            )
          ),
          hr(),
          h4(tr("图片保存与下载", "Save & Download Plot")),
          fluidRow(
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
    )
  )
}

mod_nullmodel_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(
      plot_mantel = NULL,
      plot_process = NULL,
      data_mantel = NULL,
      data_ses = NULL,
      data_process = NULL,
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
      plot_to_save <- local_rv$plot_mantel %||% local_rv$plot_process
      if (is.null(plot_to_save)) {
        showNotification("没有可保存的图片", type = "warning")
        return()
      }
      default_name <- paste0("nullmodel_", input$null_analysis_type, ".", input$null_image_format)
      current_dir <- isolate(local_rv$save_dir)
      dir_display <- if (!is.null(current_dir) && nchar(current_dir) > 0) current_dir else ""

      showModal(modalDialog(
        title = "\U0001f4be 保存图片",
        size = "m",
        easyClose = TRUE,
        fluidRow(
          column(12,
            tags$label(class = "control-label", style = "font-weight: bold;",
              "\U0001f4c1 保存文件夹"),
            fluidRow(
              column(9,
                shiny::textInput(ns("null_save_dir_text"), label = NULL, value = dir_display,
                  placeholder = "请点击右侧按钮选择文件夹...",
                  width = "100%")
              ),
              column(3, style = "padding-top: 0;",
                shinyFiles::shinyDirButton(ns("null_save_dir_btn"), "浏览",
                  "选择保存文件夹",
                  class = "btn-outline-primary", icon = icon("folder-open"),
                  style = "margin-top: 25px;")
              )
            )
          )
        ),
        tags$hr(style = "margin: 12px 0;"),
        fluidRow(
          column(6, shiny::textInput(ns("null_save_filename"), "\U0001f4dd 文件名称",
            value = default_name)),
          column(6, tags$div(style = "padding-top: 28px;",
            tags$p(class = "text-muted", style = "margin-bottom: 0;",
              tags$span(icon("info-circle")), " 格式: ",
              tags$code(toupper(input$null_image_format)),
              " | 宽×高: ",
              tags$code(paste0(input$null_save_width, "×", input$null_save_height)),
              " | DPI: ", tags$code(input$null_save_dpi))
          ))
        ),
        footer = tagList(
          shiny::actionButton(ns("null_confirm_save"), "\u2705 保存到文件夹",
            icon = icon("save"), class = "btn-primary"),
          shiny::modalButton("取消")
        )
      ))
    })

    observeEvent(input$null_confirm_save, {
      plot_to_save <- local_rv$plot_mantel %||% local_rv$plot_process
      req(plot_to_save)
      save_dir <- local_rv$save_dir
      if (is.null(save_dir) || !isTRUE(nchar(save_dir) > 0)) {
        showNotification("请先选择保存文件夹", type = "warning")
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
        showNotification("文件夹不存在，请重新选择", type = "error")
        return()
      }
      full_path <- file.path(save_dir, fname)

      tryCatch({
        ggplot2::ggsave(filename = full_path, plot = plot_to_save,
          width = input$null_save_width, height = input$null_save_height,
          units = "in", dpi = input$null_save_dpi, scale = 1)
        removeModal()
        showNotification(paste0("\u2705 已保存至: ", full_path), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("保存失败: ", e$message), type = "error", duration = 10)
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
      updateSelectInput(session, "null_group", choices = c("无" = "", cols))
      updateSelectInput(session, "null_select_group", choices = character(0), selected = character(0))
      updatePickerInput(session, "null_env_cols", choices = cols, selected = character(0))
      updateSelectInput(session, "null_mantel_use_env", choices = c("无" = "", cols))
      updateSelectInput(session, "null_cscore_group", choices = c("无" = "", cols))
      updateSelectInput(session, "null_nst_group", choices = cols)
      updateSelectInput(session, "null_process_group", choices = c("无" = "", cols))
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

    observeEvent(input$run_null_mantel_corr, {
      if (!check_microtable(rv)) {
        showNotification("请先导入数据", type = "error")
        return()
      }
      if (is.null(rv$microtable$phylo_tree)) {
        showNotification("Mantel相关分析需要进化树数据（phylo_tree）", type = "error")
        return()
      }

      result <- tryCatch({
        dataset_name <- rv$microtable_name %||% "tmp_microtable"
        taxa_num_val <- { val <- input$null_taxa_number; if (is.null(val) || is.na(val) || val <= 0) NULL else val }
        filter_thres_val <- { val <- input$null_filter_thres; if (is.null(val) || val <= 0) 0 else val }
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

        use_env_val <- if (isTRUE(nzchar(input$null_mantel_use_env))) input$null_mantel_use_env else NULL
        t_null$cal_mantel_corr(use_env = use_env_val)
        p <- t_null$plot_mantel_corr()

        mantel_code <- paste0(
          "t_null$cal_mantel_corr(\n",
          "  use_env = ", if (!is.null(use_env_val)) paste0("\"", use_env_val, "\"") else "NULL", "\n",
          ")\n",
          "p <- t_null$plot_mantel_corr()\n"
        )
        init_code <- paste0("t_null <- microeco::trans_nullmodel$new(dataset = ", dataset_name, ")\n")
        code <- paste0(init_code, "# Mantel相关树\n", mantel_code)

        list(success = TRUE, plot = p, data_result = t_null$res_mantel_corr$mantel.res, code = code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      append_code(rv, result$code, "零模型 - mantel_corr")
      local_rv$plot_mantel <- result$plot
      local_rv$data_mantel <- result$data_result
      rv$last_plot <- result$plot
      showNotification("完成", type = "message")
    })

    output$null_mantel_table <- DT::renderDataTable({
      dt <- local_rv$data_mantel
      if (is.data.frame(dt) || is.matrix(dt)) {
        DT::datatable(as.data.frame(dt), options = list(scrollX = TRUE, pageLength = 20),
          rownames = TRUE, filter = "top")
      }
    })

    output$null_mantel_plot <- shiny::renderPlot({
      req(local_rv$plot_mantel)
      local_rv$plot_mantel
    })

    observeEvent(input$run_null_ses, {
      if (!check_microtable(rv)) {
        showNotification("请先导入数据", type = "error")
        return()
      }
      if (is.null(rv$microtable$phylo_tree) && input$null_analysis_type != "rcbray" && input$null_analysis_type != "cscore" && input$null_analysis_type != "nst") {
        showNotification("零模型分析需要进化树数据（phylo_tree）", type = "error")
        return()
      }

      result <- tryCatch({
        dataset_name <- rv$microtable_name %||% "tmp_microtable"
        taxa_num_val <- { val <- input$null_taxa_number; if (is.null(val) || is.na(val) || val <= 0) NULL else val }
        filter_thres_val <- { val <- input$null_filter_thres; if (is.null(val) || val <= 0) 0 else val }
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

        init_code <- paste0("t_null <- microeco::trans_nullmodel$new(dataset = ", dataset_name, ")\n")
        data_result <- NULL

        if (input$null_analysis_type == "ses_betampd") {
          t_null$cal_ses_betampd(
            runs = input$null_runs,
            null.model = input$null_null_model,
            abundance.weighted = input$null_abundance_weighted
          )
          data_result <- t_null$res_ses_betampd
          local_rv$table_type <- "ses_betampd"
          code <- paste0(init_code, "t_null$cal_ses_betampd(runs = ", input$null_runs, ")\n")

        } else if (input$null_analysis_type == "ses_betamntd") {
          t_null$cal_ses_betamntd(
            runs = input$null_runs,
            null.model = input$null_null_model,
            abundance.weighted = input$null_abundance_weighted,
            exclude.conspecifics = input$null_exclude_conspec,
            use_iCAMP = input$null_use_icamp,
            nworker = input$null_nworker
          )
          data_result <- t_null$res_ses_betamntd
          local_rv$table_type <- "ses_betamntd"
          code <- paste0(init_code, "t_null$cal_ses_betamntd(runs = ", input$null_runs, ")\n")

        } else if (input$null_analysis_type == "rcbray") {
          t_null$cal_rcbray(
            runs = input$null_runs,
            verbose = TRUE,
            null.model = input$null_null_model
          )
          data_result <- t_null$res_rcbray
          local_rv$table_type <- "rcbray"
          code <- paste0(init_code, "t_null$cal_rcbray(runs = ", input$null_runs, ")\n")

        } else if (input$null_analysis_type == "nri") {
          t_null$cal_NRI(
            null.model = input$null_nri_null_model,
            abundance.weighted = input$null_nri_weighted,
            runs = input$null_nri_runs
          )
          data_result <- t_null$res_NRI
          local_rv$table_type <- "nri"
          code <- paste0(init_code, "t_null$cal_NRI(runs = ", input$null_nri_runs, ")\n")

        } else if (input$null_analysis_type == "nti") {
          t_null$cal_NTI(
            null.model = input$null_nri_null_model,
            abundance.weighted = input$null_nri_weighted,
            runs = input$null_nri_runs
          )
          data_result <- t_null$res_NTI
          local_rv$table_type <- "nti"
          code <- paste0(init_code, "t_null$cal_NTI(runs = ", input$null_nri_runs, ")\n")

        } else if (input$null_analysis_type == "cscore") {
          cscore_group_val <- if (nchar(input$null_cscore_group)) input$null_cscore_group else NULL
          res_cscore <- t_null$cal_Cscore(by_group = cscore_group_val)
          data_result <- data.frame(C_score = res_cscore)
          local_rv$table_type <- "cscore"
          code <- paste0(init_code, "t_null$cal_Cscore()\n")

        } else if (input$null_analysis_type == "nst") {
          if (!isTRUE(nchar(input$null_nst_group) > 0)) {
            stop("请选择NST的group参数")
          }
          t_null$cal_NST(
            method = input$null_nst_method,
            group = input$null_nst_group
          )
          data_result <- t_null$res_NST$index.pair
          local_rv$table_type <- "nst"
          code <- paste0(init_code, "t_null$cal_NST(method = \"", input$null_nst_method, "\")\n")
        }

        list(success = TRUE, data_result = data_result, code = code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      append_code(rv, result$code, paste0("零模型 - ", input$null_analysis_type))
      local_rv$data_ses <- result$data_result
      showNotification("完成", type = "message")
    })

    output$null_ses_table <- DT::renderDataTable({
      dt <- local_rv$data_ses
      if (is.data.frame(dt) || is.matrix(dt)) {
        DT::datatable(as.data.frame(dt), options = list(scrollX = TRUE, pageLength = 20),
          rownames = TRUE, filter = "top")
      }
    })

    observeEvent(input$run_null_process, {
      if (!check_microtable(rv)) {
        showNotification("请先导入数据", type = "error")
        return()
      }
      if (is.null(rv$microtable$phylo_tree)) {
        showNotification("生态过程推断需要进化树数据（phylo_tree）", type = "error")
        return()
      }

      result <- tryCatch({
        dataset_name <- rv$microtable_name %||% "tmp_microtable"
        taxa_num_val <- { val <- input$null_taxa_number; if (is.null(val) || is.na(val) || val <= 0) NULL else val }
        filter_thres_val <- { val <- input$null_filter_thres; if (is.null(val) || val <= 0) 0 else val }
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

        if (is.null(t_null$res_ses_betamntd)) {
          t_null$cal_ses_betamntd(
            runs = input$null_process_runs,
            null.model = input$null_process_null_model,
            abundance.weighted = input$null_process_abundance_weighted
          )
        }
        if (is.null(t_null$res_rcbray)) {
          t_null$cal_rcbray(
            runs = input$null_process_runs,
            verbose = TRUE,
            null.model = input$null_process_null_model
          )
        }

        t_null$cal_process(
          use_betamntd = input$null_use_betamntd,
          group = if (nchar(input$null_process_group)) input$null_process_group else NULL
        )

        p <- ggplot2::ggplot(t_null$res_process, ggplot2::aes(x = process, y = percentage, fill = process)) +
          ggplot2::geom_bar(stat = "identity", width = 0.7) +
          ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percentage)), vjust = -0.3, size = 4) +
          ggplot2::scale_fill_brewer(palette = "Set2") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 11),
            legend.position = "none"
          ) +
          ggplot2::labs(x = NULL, y = "Percentage (%)", title = "生态过程分配")

        init_code <- paste0("t_null <- microeco::trans_nullmodel$new(dataset = ", dataset_name, ")\n")
        code <- paste0(init_code, "t_null$cal_process()\np <- ggplot2::ggplot(t_null$res_process, ...)\n")

        list(success = TRUE, plot = p, data_result = t_null$res_process, code = code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      append_code(rv, result$code, "零模型 - process")
      local_rv$plot_process <- result$plot
      local_rv$data_process <- result$data_result
      rv$last_plot <- result$plot
      showNotification("完成", type = "message")
    })

    output$null_process_table <- DT::renderDataTable({
      dt <- local_rv$data_process
      if (is.data.frame(dt) || is.matrix(dt)) {
        DT::datatable(as.data.frame(dt), options = list(scrollX = TRUE, pageLength = 20),
          rownames = TRUE, filter = "top")
      }
    })

    output$null_process_plot <- shiny::renderPlot({
      req(local_rv$plot_process)
      local_rv$plot_process
    })

    output$null_download_table <- downloadHandler(
      filename = function() {
        ext <- ifelse(input$null_table_format == ",", ".csv", ".tsv")
        paste0("nullmodel_", local_rv$table_type, ext)
      },
      content = function(file) {
        analysis_type <- input$null_analysis_type
        dt <- if (analysis_type == "mantel_corr") {
          local_rv$data_mantel
        } else if (analysis_type == "process") {
          local_rv$data_process
        } else {
          local_rv$data_ses
        }
        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$null_table_format,
            row.names = TRUE, quote = TRUE)
        }
      }
    )
  })
}
