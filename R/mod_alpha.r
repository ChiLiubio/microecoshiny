#' @title Alpha Diversity Module UI
#' @description
#' Provides interface for alpha diversity analysis with complete parameter options.
#' Supports multiple diversity indices, group comparison, statistical testing, and visualization.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @importFrom shinyFiles getVolumes parseDirPath shinyDirButton shinyDirChoose
#' @keywords alpha diversity microbiome
#' @family community-analysis
mod_alpha_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(
      column(12, h2(tr("\U0001f33f α多样性 Alpha Diversity", "\U0001f33f Alpha Diversity")))
    ),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4cb 参数设置", "\U0001f4cb Parameters"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,
          h4(tr("基本参数", "Basic Parameters")),
          fluidRow(
            column(2, shiny::selectInput(ns("group_col"), tr("分组列 (group)", "group"), choices = character(0))),
            column(2, shiny::selectInput(ns("by_group"), "by_group", choices = character(0))),
            column(2, shiny::selectInput(ns("by_ID"), tr("by_ID (配对)", "by_ID (paired)"), choices = character(0))),
            column(2, shiny::textInput(ns("order_x"), "order_x", value = "")),
            column(2, shiny::selectInput(ns("measure"), tr("指标 (measure)", "measure"), choices = app_config$alpha_measures, selected = "Shannon"))
          ),
          hr(),
          fluidRow(
            column(12, shiny::radioButtons(ns("plot_type"), tr("图型 (plot_type)", "plot_type"),
              choices = c("ggboxplot", "ggdotplot", "ggviolin", "ggstripchart", "ggerrorplot", "errorbar", "barerrorbar"),
              selected = "ggboxplot", inline = TRUE))
          ),
          hr(),
          shiny::conditionalPanel(condition = "input.plot_type.startsWith('gg')", ns = ns,
            h4(tr("ggpubr 参数", "ggpubr Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("color_theme"), tr("颜色 (color_theme)", "color_theme"),
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
              column(2, shiny::selectInput(ns("add"), "add",
                choices = c("none", "jitter", "boxplot", "violin", "dot", "mean", "mean_se", "mean_sd", "median", "median_iqr"), selected = "none")),
              column(2, shinyWidgets::materialSwitch(ns("order_x_mean"), "order_x_mean", value = FALSE, status = "info")),
              column(2, shiny::numericInput(ns("point_size"), "point_size", value = 3, min = 1, max = 10)),
              column(2, shiny::numericInput(ns("point_alpha"), "point_alpha", value = 0.8, min = 0.1, max = 1, step = 0.1))
            )
          ),
          shiny::conditionalPanel(condition = "input.plot_type == 'errorbar' || input.plot_type == 'barerrorbar'", ns = ns,
            h4(tr("errorbar 参数", "errorbar Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("color_theme_errorbar"), tr("颜色", "Color"),
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
              column(2, shinyWidgets::materialSwitch(ns("plot_SE"), "plot_SE (Mean±SE)", value = TRUE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("errorbar_color_black"), "errorbar_color_black", value = FALSE, status = "warning")),
              column(2, shinyWidgets::materialSwitch(ns("errorbar_addpoint"), "errorbar_addpoint", value = TRUE, status = "info")),
              column(2, shiny::numericInput(ns("bar_width"), "bar_width", value = 0.9, min = 0.5, max = 1, step = 0.1)),
              column(2, shiny::numericInput(ns("bar_alpha"), "bar_alpha", value = 0.8, min = 0.1, max = 1, step = 0.1))
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("dodge_width"), "dodge_width", value = 0.9, min = 0.5, max = 1, step = 0.1)),
              column(2, shiny::numericInput(ns("errorbar_size"), "errorbar_size", value = 1, min = 0.5, max = 3, step = 0.1)),
              column(2, shiny::numericInput(ns("errorbar_width"), "errorbar_width", value = 0.2, min = 0.1, max = 0.5, step = 0.05)),
              column(2, shinyWidgets::materialSwitch(ns("add_line"), "add_line", value = FALSE, status = "warning")),
              column(2, shiny::numericInput(ns("line_size"), "line_size", value = 0.8, min = 0.5, max = 2, step = 0.1)),
              column(2, shiny::selectInput(ns("line_type"), "line_type", choices = c("1-solid" = 1, "2-dashed" = 2, "3-dotted" = 3, "4-dotdash" = 4), selected = 2))
            ),
            fluidRow(
              column(2, shiny::textInput(ns("line_color"), "line_color", value = "grey50")),
              column(2, shiny::numericInput(ns("line_alpha"), "line_alpha", value = 0.5, min = 0.1, max = 1, step = 0.1)),
              column(6)
            )
          ),
          hr(),
          h4(tr("显著性标注 (add_sig)", "Significance annotation (add_sig)")),
          fluidRow(
            column(2, shinyWidgets::materialSwitch(ns("add_sig"), "add_sig", value = TRUE, status = "primary")),
            column(2, shiny::selectInput(ns("add_sig_label"), "add_sig_label",
              choices = c("Significance", "P.adj", "P.unadj"), selected = "Significance")),
            column(2, shiny::numericInput(ns("add_sig_text_size"), "add_sig_text_size", value = 3.88, min = 1, max = 10, step = 0.5)),
            column(2, shiny::numericInput(ns("add_sig_label_num_dec"), "add_sig_label_num_dec", value = 4, min = 1, max = 10)),
            column(2, shiny::numericInput(ns("y_start"), "y_start", value = 0.1, min = 0, max = 1, step = 0.05)),
            column(2, shiny::numericInput(ns("y_increase"), "y_increase", value = 0.05, min = 0, max = 0.5, step = 0.01))
          ),
          hr(),
          h4(tr("布局层面参数", "Layout Parameters")),
          fluidRow(
            column(2, shiny::numericInput(ns("xtext_angle"), "xtext_angle", value = 30, min = 0, max = 90, step = 15)),
            column(2, shiny::numericInput(ns("xtext_size"), "xtext_size", value = 13, min = 8, max = 20)),
            column(2, shiny::numericInput(ns("ytitle_size"), "ytitle_size", value = 17, min = 10, max = 24)),
            column(6)
          ),
          hr(),
          h4(tr("差异检验 (cal_diff)", "Difference testing (cal_diff)")),
          fluidRow(
            column(2, shiny::selectInput(ns("diff_method"), tr("检验方法 (method)", "method"),
              choices = c("KW", "KW_dunn", "wilcox", "t.test", "anova", "scheirerRayHare", "lm", "lme", "betareg", "glmm", "glmm_beta"),
              selected = "wilcox")),
            column(2, shiny::selectInput(ns("diff_measure"), tr("检验指标", "Test measure"),
              choices = app_config$alpha_measures, selected = "Shannon")),
            column(2, shiny::selectInput(ns("p_adjust_method"), "p_adjust_method",
              choices = c("fdr", "holm", "bonferroni", "none", "BH", "BY"), selected = "fdr")),
            column(2, shiny::numericInput(ns("alpha_level"), "alpha", value = 0.05, min = 0.01, max = 0.1, step = 0.01))
          ),
          fluidRow(
            column(3, shiny::selectInput(ns("anova_post_test"), "anova_post_test",
              choices = c("duncan.test", "LSD.test", "HSD.test"), selected = "duncan.test")),
            column(3, shinyWidgets::materialSwitch(ns("anova_varequal_test"), "anova_varequal_test", value = FALSE, status = "info")),
            column(3, shinyWidgets::materialSwitch(ns("KW_dunn_letter"), "KW_dunn_letter", value = TRUE, status = "info")),
            column(3, shiny::textInput(ns("diff_formula"), tr("formula (lm/lme/多因子)", "formula (lm/lme/multi-factor)"),
              value = "~ Group", placeholder = tr("例如: ~ Group + Time", "e.g.: ~ Group + Time")))
          ),
          hr(),
          h4(tr("热图/系数图参数 (heatmap/coefplot)", "Heatmap/Coefplot Parameters")),
          fluidRow(
            column(2, shiny::selectInput(ns("heatmap_cell"), "heatmap_cell",
              choices = c("P.unadj", "P.adj", "Significance"), selected = "P.unadj")),
            column(2, shiny::selectInput(ns("heatmap_sig"), "heatmap_sig",
              choices = c("Significance", "P.adj"), selected = "Significance")),
            column(2, shiny::selectInput(ns("heatmap_x"), "heatmap_x",
              choices = setNames(c("Factors", "Measure", "Method"), c(tr("因子", "Factors"), tr("指标", "Measure"), tr("方法", "Method"))), selected = "Factors")),
            column(2, shiny::selectInput(ns("heatmap_y"), "heatmap_y",
              choices = setNames(c("Measure", "Factors", "Method"), c(tr("指标", "Measure"), tr("因子", "Factors"), tr("方法", "Method"))), selected = "Measure")),
            column(2, shiny::textInput(ns("heatmap_lab_fill"), "heatmap_lab_fill", value = "P value")),
            column(2, shiny::numericInput(ns("coefplot_sig_pos"), "coefplot_sig_pos", value = 2, min = -5, max = 5, step = 0.5))
          ),
          hr(),
          h4(tr("高级参数", "Advanced Parameters")),
          fluidRow(
            column(3, shinyWidgets::materialSwitch(ns("return_model"), "return_model", value = FALSE, status = "info")),
            column(9)
          ),
          hr(),
          fluidRow(
            column(2, shiny::actionButton(ns("run_alpha"), tr("\U0001f4ca 生成", "\U0001f4ca Generate"), icon = icon("play"), class = "btn-primary", width = "100%")),
            column(2, shiny::selectInput(ns("image_format"), tr("格式", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("save_width"), tr("宽 (width)", "Width"), value = 10, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("save_height"), tr("高 (height)", "Height"), value = 7, min = 3, max = 15)),
            column(2, shiny::numericInput(ns("save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(2, shiny::actionButton(ns("save_plot_btn"), tr("\U0001f4e5保存图片", "\U0001f4e5Save Plot"), icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
          ),
          fluidRow(
            column(2, shiny::downloadButton(ns("download_table"), tr("\U0001f4e5表格保存", "\U0001f4e5Save Table"), class = "btn-outline-info", width = "100%")),
            column(2, shiny::selectInput(ns("table_format"), tr("表格", "Table"),
              choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
            column(8)
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
          shinycssloaders::withSpinner(shiny::plotOutput(ns("alpha_plot"), height = "500px"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 差异检验结果", "\U0001f4ca Difference Test Results"), status = "info", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("alpha_diff_table"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 数据", "\U0001f4ca Data"), status = "secondary", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("alpha_table"))
        )
      )
    )
  )
}

mod_alpha_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(plot = NULL, data_alpha = NULL, save_dir = NULL)

    volumes <- c(
      Home      = path.expand("~"),
      Desktop   = file.path(path.expand("~"), "Desktop"),
      Documents = file.path(path.expand("~"), "Documents"),
      get_volumes_safe()
    )
    shinyFiles::shinyDirChoose(input, "save_dir_btn", roots = volumes,
      session = session, defaultRoot = "Desktop")

    observeEvent(input$save_dir_btn, {
      parsed <- shinyFiles::parseDirPath(volumes, input$save_dir_btn)
      if (!is.null(parsed) && length(parsed) > 0 && nchar(parsed) > 0) {
        local_rv$save_dir <- parsed
      }
    })

    observeEvent(input$save_plot_btn, {
      req(local_rv$plot)
      default_name <- paste0("alpha_", input$plot_type, ".", input$image_format)
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
                shiny::textInput(ns("save_dir_text"), label = NULL, value = dir_display,
                  placeholder = "\u8bf7\u70b9\u51fb\u53f3\u4fa7\u6309\u94ae\u9009\u62e9\u6587\u4ef6\u5939...",
                  width = "100%")
              ),
              column(3, style = "padding-top: 0;",
                shinyFiles::shinyDirButton(ns("save_dir_btn"), "\u6d4f\u89c8",
                  "\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939",
                  class = "btn-outline-primary", icon = icon("folder-open"),
                  style = "margin-top: 25px;")
              )
            )
          )
        ),
        tags$hr(style = "margin: 12px 0;"),
        fluidRow(
          column(6, shiny::textInput(ns("save_filename"), "\U0001f4dd \u6587\u4ef6\u540d\u79f0",
            value = default_name)),
          column(6, tags$div(style = "padding-top: 28px;",
            tags$p(class = "text-muted", style = "margin-bottom: 0;",
              tags$span(icon("info-circle")), " \u683c\u5f0f: ",
              tags$code(toupper(input$image_format)),
              " | \u5bbd\u00d7\u9ad8: ",
              tags$code(paste0(input$save_width, "\u00d7", input$save_height)),
              " | DPI: ", tags$code(input$save_dpi))
          ))
        ),
        footer = tagList(
          shiny::actionButton(ns("confirm_save"), "\u2705 \u4fdd\u5b58\u5230\u6587\u4ef6\u5939",
            icon = icon("save"), class = "btn-primary"),
          shiny::modalButton("\u53d6\u6d88")
        )
      ))
    })

    observeEvent(input$confirm_save, {
      req(local_rv$plot)
      save_dir <- local_rv$save_dir
      if (is.null(save_dir) || !isTRUE(nchar(save_dir) > 0)) {
        showNotification("\u8bf7\u5148\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939", type = "warning")
        return()
      }
      fname <- input$save_filename
      if (is.null(fname) || !isTRUE(nzchar(trimws(fname)))) {
        fname <- paste0("alpha_", input$plot_type)
      }
      fname <- trimws(fname)
      ext <- input$image_format
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
          width = input$save_width, height = input$save_height,
          units = "in", dpi = input$save_dpi, scale = 1)
        removeModal()
        showNotification(paste0("\u2705 \u5df2\u4fdd\u5b58\u81f3: ", full_path), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("\u4fdd\u5b58\u5931\u8d25: ", e$message), type = "error", duration = 10)
      })
    })

    observe({
      if (!check_microtable(rv)) {
        updateSelectInput(session, "group_col", choices = character(0))
        updateSelectInput(session, "by_group", choices = character(0))
        updateSelectInput(session, "by_ID", choices = character(0))
        return()
      }
      cols <- get_sample_cols(rv)
      updateSelectInput(session, "group_col", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "by_group", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "by_ID", choices = c("\u65e0" = "", cols))
    })

    observeEvent(input$measure, {
      req(input$measure)
      if (is.null(input$diff_measure) || !isTRUE(nzchar(input$diff_measure))) {
        updateSelectInput(session, "diff_measure", selected = input$measure)
      }
    })

    get_color_palette <- function(name, n = 8) {
      tryCatch({
        switch(name,
          "Dark2"    = RColorBrewer::brewer.pal(min(n, 8), "Dark2"),
          "Set1"     = RColorBrewer::brewer.pal(min(n, 8), "Set1"),
          "Set2"     = RColorBrewer::brewer.pal(min(n, 8), "Set2"),
          "Set3"     = RColorBrewer::brewer.pal(min(n, 12), "Set3"),
          "Paired"   = RColorBrewer::brewer.pal(min(n, 12), "Paired"),
          "Spectral" = RColorBrewer::brewer.pal(min(n, 11), "Spectral"),
          "Viridis"  = viridisLite::viridis(n),
          RColorBrewer::brewer.pal(min(n, 8), "Dark2")
        )
      }, error = function(e) RColorBrewer::brewer.pal(min(n, 8), "Dark2"))
    }

    observeEvent(input$run_alpha, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      group_val <- if (isTRUE(nzchar(input$group_col))) input$group_col else NULL
      by_group_val <- if (isTRUE(nzchar(input$by_group))) input$by_group else NULL
      by_ID_val <- if (isTRUE(nzchar(input$by_ID))) input$by_ID else NULL
      order_x_val <- if (isTRUE(nzchar(input$order_x))) strsplit(trimws(input$order_x), "[,\\s]+")[[1]] else NULL
      measure_val <- input$measure

      result <- tryCatch({
        mt <- rv$microtable

        t_alpha <- microeco::trans_alpha$new(
          dataset = mt,
          group = group_val,
          by_group = by_group_val,
          by_ID = by_ID_val,
          order_x = order_x_val
        )

        if (!is.null(measure_val)) {
          mt$cal_alphadiv(measure = measure_val)
        }

        dataset_name <- rv$microtable_name %||% "tmp_microtable"

        ta_code <- paste0(
          "# \u03b1\u591a\u6837\u6027\u5206\u6790\n",
          "t_alpha <- microeco::trans_alpha$new(\n",
          "  dataset = ", dataset_name, ",\n",
          if (!is.null(group_val)) paste0("  group = \"", group_val, "\",\n") else "  group = NULL,\n",
          if (!is.null(by_group_val)) paste0("  by_group = \"", by_group_val, "\",\n") else "  by_group = NULL,\n",
          if (!is.null(by_ID_val)) paste0("  by_ID = \"", by_ID_val, "\",\n") else "  by_ID = NULL,\n",
          if (!is.null(order_x_val)) paste0("  order_x = c(\"", paste0(order_x_val, collapse = "\", \""), "\"),\n") else "  order_x = NULL,\n",
          ")\n",
          if (!is.null(measure_val)) paste0(dataset_name, "$cal_alphadiv(measure = \"", measure_val, "\")\n") else ""
        )

        diff_method_val <- input$diff_method
        diff_code <- paste0(
          "t_alpha$cal_diff(\n",
          "  method = \"", diff_method_val, "\",\n",
          "  measure = \"", input$diff_measure, "\",\n",
          "  p_adjust_method = \"", input$p_adjust_method, "\",\n",
          "  alpha = ", input$alpha_level
        )

        if (diff_method_val == "KW_dunn") {
          diff_code <- paste0(diff_code, ",\n  KW_dunn_letter = ", input$KW_dunn_letter)
        }
        if (diff_method_val == "anova") {
          diff_code <- paste0(diff_code, ",\n  anova_post_test = \"", input$anova_post_test, "\",\n",
            "  anova_varequal_test = ", input$anova_varequal_test)
        }
        if (diff_method_val %in% c("lm", "lme", "betareg", "glmm", "glmm_beta")) {
          formula_val <- trimws(input$diff_formula)
          formula_val <- gsub("^['\"]|['\"]$", "", formula_val)
          diff_code <- paste0(diff_code, ",\n  formula = ", formula_val)
        }
        diff_code <- paste0(diff_code, "\n)\n")

        full_code <- paste0(ta_code, "# \u5dee\u5f02\u68c0\u9a8c\n", diff_code)

        if (diff_method_val %in% c("lm", "lme", "betareg", "glmm", "glmm_beta")) {
          t_alpha$cal_diff(
            method = diff_method_val,
            measure = input$diff_measure,
            p_adjust_method = input$p_adjust_method,
            alpha = input$alpha_level,
            formula = input$diff_formula,
            return_model = input$return_model
          )
        } else if (diff_method_val == "KW_dunn") {
          t_alpha$cal_diff(
            method = diff_method_val,
            measure = input$diff_measure,
            p_adjust_method = input$p_adjust_method,
            alpha = input$alpha_level,
            KW_dunn_letter = input$KW_dunn_letter,
            return_model = input$return_model
          )
        } else if (diff_method_val == "anova") {
          t_alpha$cal_diff(
            method = diff_method_val,
            measure = input$diff_measure,
            p_adjust_method = input$p_adjust_method,
            alpha = input$alpha_level,
            anova_post_test = input$anova_post_test,
            anova_varequal_test = input$anova_varequal_test,
            return_model = input$return_model
          )
        } else {
          t_alpha$cal_diff(
            method = diff_method_val,
            measure = input$diff_measure,
            p_adjust_method = input$p_adjust_method,
            alpha = input$alpha_level,
            return_model = input$return_model
          )
        }

        p <- t_alpha$plot_alpha(
          plot_type = input$plot_type,
          color_values = get_color_palette(input$color_theme),
          measure = measure_val,
          group = group_val,
          add = input$add,
          add_sig = input$add_sig,
          add_sig_label = input$add_sig_label,
          add_sig_text_size = input$add_sig_text_size,
          add_sig_label_num_dec = input$add_sig_label_num_dec,
          order_x_mean = input$order_x_mean,
          y_start = input$y_start,
          y_increase = input$y_increase,
          xtext_angle = input$xtext_angle,
          xtext_size = input$xtext_size,
          ytitle_size = input$ytitle_size,
          bar_width = input$bar_width,
          bar_alpha = input$bar_alpha,
          dodge_width = input$dodge_width,
          plot_SE = input$plot_SE,
          errorbar_size = input$errorbar_size,
          errorbar_width = input$errorbar_width,
          errorbar_addpoint = input$errorbar_addpoint,
          errorbar_color_black = input$errorbar_color_black,
          point_size = input$point_size,
          point_alpha = input$point_alpha,
          add_line = input$add_line,
          line_size = input$line_size,
          line_type = as.numeric(input$line_type),
          line_color = input$line_color,
          line_alpha = input$line_alpha,
          heatmap_cell = input$heatmap_cell,
          heatmap_sig = input$heatmap_sig,
          heatmap_x = input$heatmap_x,
          heatmap_y = input$heatmap_y,
          heatmap_lab_fill = input$heatmap_lab_fill,
          coefplot_sig_pos = input$coefplot_sig_pos
        )

        list(success = TRUE, plot = p, data_alpha = t_alpha$data_alpha, data_stat = t_alpha$data_stat, res_diff = t_alpha$res_diff, code = full_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      append_code(rv, result$code, "\u03b1\u591a\u6837\u6027\u5206\u6790")
      local_rv$plot <- result$plot
      local_rv$data_alpha <- result$data_alpha
      local_rv$data_stat <- result$data_stat
      local_rv$res_diff <- result$res_diff
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$alpha_plot <- shiny::renderPlot({
      req(local_rv$plot)
      local_rv$plot
    })

    output$alpha_diff_table <- DT::renderDataTable({
      req(local_rv$res_diff)
      dt <- local_rv$res_diff
      if (is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
      }
    })

    output$alpha_table <- DT::renderDataTable({
      req(local_rv$data_alpha)
      dt <- local_rv$data_alpha
      if (is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
      }
    })

    output$download_table <- downloadHandler(
      filename = function() paste0("alpha_diversity", ifelse(input$table_format == ",", ".csv", ".tsv")),
      content = function(file) {
        req(local_rv$data_alpha)
        write.table(local_rv$data_alpha, file, sep = input$table_format, row.names = FALSE, quote = TRUE)
      }
    )
  })
}