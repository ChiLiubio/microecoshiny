#' @title Functional Prediction Module UI
#' @description
#' Provides interface for functional prediction analysis including PICRUSt, FAPROTAX,
#' BugBase, and redundancy analysis.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @keywords functional prediction PICRUSt FAPROTAX microbiome
#' @family community-analysis
mod_func_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(
      column(12, h2(tr("\U0001f9ec 功能预测 Functional Prediction", "\U0001f9ec Functional Prediction")))
    ),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4cb 参数设置", "\U0001f4cb Parameters"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,
          h4(tr("\u5206\u6790\u6b65\u9aa4", "Analysis Step")),
          fluidRow(
            column(12, shiny::radioButtons(ns("func_analysis_step"), tr("\u5206\u6790\u6b65\u9aa4", "Analysis Step"),
              choices = setNames(c("func", "redundancy", "plot_fr", "bar", "heatmap"),
                                 c(tr("\U0001f4ca \u529f\u80fd\u9884\u6d4b", "\U0001f4ca Func Prediction"),
                                   tr("\U0001f4ca \u5197\u4f59\u5206\u6790", "\U0001f4ca Redundancy"),
                                   tr("\U0001f4ca \u529f\u80fd\u56fe", "\U0001f4ca Func Plot"),
                                   tr("\U0001f4ca \u67f1\u72b6\u56fe", "\U0001f4ca Bar Plot"),
                                   tr("\U0001f4ca \u70ed\u56fe", "\U0001f4ca Heatmap"))),
              selected = "func", inline = TRUE))
          ),
          hr(),
          shiny::conditionalPanel(condition = "input.func_analysis_step == 'func'", ns = ns,
            h4(tr("\u529f\u80fd\u9884\u6d4b\u53c2\u6570", "Functional Prediction Parameters")),
            fluidRow(
              column(3, shiny::selectInput(ns("func_use_measure"), "use_measure",
                choices = setNames(c("bray", "jaccard", "unifrac", "wunifrac"),
                                   c("Bray-Curtis", "Jaccard", "Unweighted UniFrac", "Weighted UniFrac")),
                selected = "bray")),
              column(3, shiny::numericInput(ns("func_filter_thres"), "filter_thres",
                value = 0, min = 0, max = 1, step = 0.001)),
              column(3, shiny::selectInput(ns("func_method"), "method",
                choices = c("anova", "kruskal", "wilcox", "t.test"), selected = "anova")),
              column(3, shiny::selectInput(ns("func_func_type"), "func_type",
                choices = c("COG", "ko"), selected = "COG"))
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_func_step"), tr("\U0001f3c1 \u6267\u884c\u5206\u6790", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca \u7ed3\u679c\u8868", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("func_step_table"))
                )
              )
            ),
            fluidRow(
              column(2, shiny::selectInput(ns("func_table_format_func"), tr("\u8868\u683c", "Table"),
                choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
              column(2, shiny::downloadButton(ns("func_download_table_func"), tr("\U0001f4e5\u8868\u683c\u4e0b\u8f7d", "\U0001f4e5Download Table"),
                class = "btn-outline-info", width = "100%")),
              column(8)
            )
          ),
          shiny::conditionalPanel(condition = "input.func_analysis_step == 'redundancy'", ns = ns,
            h4(paste0("cal_func_FR ", tr("\u53c2\u6570", "Parameters"))),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("func_fr_weighted"), tr("abundance_weighted (\u4e30\u5ea6\u52a0\u6743)", "abundance_weighted"),
                value = FALSE, status = "info")),
              column(3, shinyWidgets::materialSwitch(ns("func_fr_adj_tax"), tr("adj_tax (\u68c0\u6d4b\u8c03\u6574)", "adj_tax"),
                value = FALSE, status = "warning")),
              column(3, shiny::selectInput(ns("func_fr_adj_tax_by"), "adj_tax_by",
                choices = c("Phylum", "Class", "Order", "Family", "Genus"), selected = "Genus")),
              column(3, shinyWidgets::materialSwitch(ns("func_fr_perc"), tr("perc (100%\u5236)", "perc (100%)"),
                value = FALSE, status = "success"))
            ),
            fluidRow(
              column(3, shiny::numericInput(ns("func_fr_dec"), tr("dec (\u5c0f\u6570\u4f4d\u6570)", "dec (decimals)"),
                value = 6, min = 1, max = 10)),
              column(3, shinyWidgets::materialSwitch(ns("func_fr_remove_zero"), "remove_zero",
                value = TRUE, status = "primary")),
              column(6)
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_func_redundancy"), tr("\U0001f3c1 \u8ba1\u7b97\u5197\u4f59", "\U0001f3c1 Run Redundancy"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca \u7ed3\u679c\u8868", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("func_rda_table"))
                )
              )
            ),
            fluidRow(
              column(2, shiny::selectInput(ns("func_table_format_rda"), tr("\u8868\u683c", "Table"),
                choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
              column(2, shiny::downloadButton(ns("func_download_table_rda"), tr("\U0001f4e5\u8868\u683c\u4e0b\u8f7d", "\U0001f4e5Download Table"),
                class = "btn-outline-info", width = "100%")),
              column(8)
            )
          ),
          shiny::conditionalPanel(condition = "input.func_analysis_step == 'plot_fr'", ns = ns,
            h4(paste0("plot_func_FR ", tr("\u53c2\u6570", "Parameters"))),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("func_fr_facet"), "add_facet",
                value = TRUE, status = "info")),
              column(3, shiny::textInput(ns("func_fr_color_low"), "color_gradient_low",
                value = "#00008B")),
              column(3, shiny::textInput(ns("func_fr_color_high"), "color_gradient_high",
                value = "#9E0142")),
              column(3)
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_func_plot_fr"), tr("\U0001f3c1 \u751f\u6210\u56fe\u5f62", "\U0001f3c1 Run Plot"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca \u56fe\u533a", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("func_plot_fr_plot"), height = "550px"))
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "input.func_analysis_step == 'bar'", ns = ns,
            h4(tr("\u67f1\u72b6\u56fe\u53c2\u6570", "Bar Plot Parameters")),
            fluidRow(
              column(3, shiny::numericInput(ns("func_top_n_bar"), tr("top_n (\u663e\u793a\u524dN)", "top_n (show top N)"),
                value = 20, min = 5, max = 100)),
              column(3, shiny::selectInput(ns("func_color_theme_bar"), "color_theme",
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"),
                selected = "Spectral")),
              column(3, shiny::selectInput(ns("func_plot_group_bar"), tr("\u5206\u7ec4", "Group"),
                choices = setNames(c("", "group"), c("\u65e0", "Group")))),
              column(3)
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_func_bar"), tr("\U0001f3c1 \u751f\u6210\u56fe\u5f62", "\U0001f3c1 Run Plot"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca \u56fe\u533a", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("func_bar_plot"), height = "550px"))
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "input.func_analysis_step == 'heatmap'", ns = ns,
            h4(tr("\u70ed\u56fe\u53c2\u6570", "Heatmap Parameters")),
            fluidRow(
              column(3, shiny::numericInput(ns("func_top_n_heatmap"), tr("top_n (\u663e\u793a\u524dN)", "top_n (show top N)"),
                value = 20, min = 5, max = 100)),
              column(3, shiny::selectInput(ns("func_color_theme_heatmap"), "color_theme",
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"),
                selected = "Spectral")),
              column(3, shiny::selectInput(ns("func_plot_group_heatmap"), tr("\u5206\u7ec4", "Group"),
                choices = setNames(c("", "group"), c("\u65e0", "Group")))),
              column(3)
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_func_heatmap"), tr("\U0001f3c1 \u751f\u6210\u56fe\u5f62", "\U0001f3c1 Run Plot"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca \u56fe\u533a", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("func_heatmap_plot"), height = "550px"))
                )
              )
            )
          ),
          hr(),
          fluidRow(
            column(2, shiny::selectInput(ns("func_image_format"), tr("\u683c\u5f0f", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("func_save_width"), tr("\u5bbd (width)", "Width"), value = 10, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("func_save_height"), tr("\u9ad8 (height)", "Height"), value = 7, min = 3, max = 15)),
            column(2, shiny::numericInput(ns("func_save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(2, shiny::actionButton(ns("func_save_plot_btn"), tr("\U0001f4e5\u4fdd\u5b58\u56fe\u7247", "\U0001f4e5Save Plot"), icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
          )
        )
      )
    )
  )
}

mod_func_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(
      plot_fr = NULL,
      bar_plot = NULL,
      heatmap_plot = NULL,
      data_func = NULL,
      data_rda = NULL,
      save_dir = NULL,
      t_func = NULL
    )

    volumes <- c(
      Home      = path.expand("~"),
      Desktop   = file.path(path.expand("~"), "Desktop"),
      Documents = file.path(path.expand("~"), "Documents"),
      get_volumes_safe()
    )
    shinyFiles::shinyDirChoose(input, "func_save_dir_btn", roots = volumes,
      session = session, defaultRoot = "Desktop")

    observeEvent(input$func_save_dir_btn, {
      parsed <- shinyFiles::parseDirPath(volumes, input$func_save_dir_btn)
      if (!is.null(parsed) && length(parsed) > 0 && nchar(parsed) > 0) {
        local_rv$save_dir <- parsed
      }
    })

    observeEvent(input$func_save_plot_btn, {
      req(local_rv$plot_fr %||% local_rv$bar_plot %||% local_rv$heatmap_plot)
      default_name <- paste0("func_", input$func_analysis_step, ".", input$func_image_format)
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
                shiny::textInput(ns("func_save_dir_text"), label = NULL, value = dir_display,
                  placeholder = "\u8bf7\u70b9\u51fb\u53f3\u4fa7\u6309\u94ae\u9009\u62e9\u6587\u4ef6\u5939...",
                  width = "100%")
              ),
              column(3, style = "padding-top: 0;",
                shinyFiles::shinyDirButton(ns("func_save_dir_btn"), "\u6d4f\u89c8",
                  "\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939",
                  class = "btn-outline-primary", icon = icon("folder-open"),
                  style = "margin-top: 25px;")
              )
            )
          )
        ),
        tags$hr(style = "margin: 12px 0;"),
        fluidRow(
          column(6, shiny::textInput(ns("func_save_filename"), "\U0001f4dd \u6587\u4ef6\u540d\u79f0",
            value = default_name)),
          column(6, tags$div(style = "padding-top: 28px;",
            tags$p(class = "text-muted", style = "margin-bottom: 0;",
              tags$span(icon("info-circle")), " \u683c\u5f0f: ",
              tags$code(toupper(input$func_image_format)),
              " | \u5bbd\u00d7\u9ad8: ",
              tags$code(paste0(input$func_save_width, "\u00d7", input$func_save_height)),
              " | DPI: ", tags$code(input$func_save_dpi))
          ))
        ),
        footer = tagList(
          shiny::actionButton(ns("func_confirm_save"), "\u2705 \u4fdd\u5b58\u5230\u6587\u4ef6\u5939",
            icon = icon("save"), class = "btn-primary"),
          shiny::modalButton("\u53d6\u6d88")
        )
      ))
    })

    observeEvent(input$func_confirm_save, {
      save_dir <- local_rv$save_dir
      if (is.null(save_dir) || !isTRUE(nchar(save_dir) > 0)) {
        showNotification("\u8bf7\u5148\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939", type = "warning")
        return()
      }
      fname <- input$func_save_filename
      if (is.null(fname) || !isTRUE(nzchar(trimws(fname)))) {
        fname <- paste0("func_", input$func_analysis_step)
      }
      fname <- trimws(fname)
      ext <- input$func_image_format
      if (!grepl(paste0("\\.", ext, "$"), fname, ignore.case = TRUE)) {
        fname <- sub("\\.(png|pdf|svg|tiff|tif)$", "", fname, ignore.case = TRUE)
        fname <- paste0(fname, ".", ext)
      }
      if (!dir.exists(save_dir)) {
        showNotification("\u6587\u4ef6\u5939\u4e0d\u5b58\u5728\uff0c\u8bf7\u91cd\u65b0\u9009\u62e9", type = "error")
        return()
      }
      full_path <- file.path(save_dir, fname)

      plot_to_save <- local_rv$plot_fr %||% local_rv$bar_plot %||% local_rv$heatmap_plot
      req(plot_to_save)

      tryCatch({
        ggplot2::ggsave(filename = full_path, plot = plot_to_save,
          width = input$func_save_width, height = input$func_save_height,
          units = "in", dpi = input$func_save_dpi, scale = 1)
        removeModal()
        showNotification(paste0("\u2705 \u5df2\u4fdd\u5b58\u81f3: ", full_path), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("\u4fdd\u5b58\u5931\u8d25: ", e$message), type = "error", duration = 10)
      })
    })

    observe({
      if (!check_microtable(rv)) return()
    })

    observeEvent(input$run_func_step, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      result <- tryCatch({
        mt <- rv$microtable
        dataset_name <- rv$microtable_name %||% "tmp_microtable"
        init_code <- paste0("# \u529f\u80fd\u9884\u6d4b\u5206\u6790\n",
          "t_func <- microeco::trans_func$new(\n",
          "  dataset = ", dataset_name, ",\n",
          "  use_measure = \"", input$func_use_measure, "\"\n",
          ")\n")

        t_func <- microeco::trans_func$new(dataset = mt, use_measure = input$func_use_measure)
        t_func$cal_func(func_type = input$func_func_type, filter_thres = input$func_filter_thres)
        t_func$cal_diff(method = input$func_method)

        func_code <- paste0(
          "t_func$cal_func(\n",
          "  func_type = \"", input$func_func_type, "\",\n",
          "  filter_thres = ", input$func_filter_thres, "\n",
          ")\n",
          "t_func$cal_diff(\n",
          "  method = \"", input$func_method, "\"\n",
          ")\n"
        )
        full_code <- paste0(init_code, func_code)

        list(success = TRUE, t_func = t_func, code = full_code, data = t_func$res_diff)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      append_code(rv, result$code, "\u529f\u80fd\u9884\u6d4b - \u529f\u80fd\u9884\u6d4b")
      local_rv$t_func <- result$t_func
      local_rv$data_func <- result$data
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$func_step_table <- DT::renderDataTable({
      dt <- local_rv$data_func
      if (!is.null(dt) && is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 20), rownames = TRUE, filter = "top")
      }
    })

    output$func_download_table_func <- downloadHandler(
      filename = function() {
        ext <- ifelse(input$func_table_format_func == ",", ".csv", ".tsv")
        paste0("func_prediction", ext)
      },
      content = function(file) {
        dt <- local_rv$data_func
        if (!is.null(dt) && is.data.frame(dt)) {
          write.table(dt, file, sep = input$func_table_format_func, row.names = TRUE, quote = TRUE)
        }
      }
    )

    observeEvent(input$run_func_redundancy, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      result <- tryCatch({
        mt <- rv$microtable
        dataset_name <- rv$microtable_name %||% "tmp_microtable"
        init_code <- paste0("# \u5197\u4f59\u5206\u6790\n",
          "t_func <- microeco::trans_func$new(\n",
          "  dataset = ", dataset_name, "\n",
          ")\n")

        t_func <- microeco::trans_func$new(dataset = mt)
        t_func$cal_func_FR(
          abundance_weighted = input$func_fr_weighted,
          adj_tax = input$func_fr_adj_tax,
          adj_tax_by = input$func_fr_adj_tax_by,
          perc = input$func_fr_perc,
          dec = input$func_fr_dec,
          remove_zero = input$func_fr_remove_zero
        )

        rda_code <- paste0(
          "t_func$cal_func_FR(\n",
          "  abundance_weighted = ", input$func_fr_weighted, ",\n",
          "  adj_tax = ", input$func_fr_adj_tax, ",\n",
          "  adj_tax_by = \"", input$func_fr_adj_tax_by, "\",\n",
          "  perc = ", input$func_fr_perc, ",\n",
          "  dec = ", input$func_fr_dec, ",\n",
          "  remove_zero = ", input$func_fr_remove_zero, "\n",
          ")\n"
        )
        full_code <- paste0(init_code, rda_code)

        list(success = TRUE, t_func = t_func, code = full_code, data = t_func$res_func_FR)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      append_code(rv, result$code, "\u529f\u80fd\u9884\u6d4b - \u5197\u4f59\u5206\u6790")
      local_rv$t_func <- result$t_func
      local_rv$data_rda <- result$data
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$func_rda_table <- DT::renderDataTable({
      dt <- local_rv$data_rda
      if (!is.null(dt) && is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 20), rownames = TRUE, filter = "top")
      }
    })

    output$func_download_table_rda <- downloadHandler(
      filename = function() {
        ext <- ifelse(input$func_table_format_rda == ",", ".csv", ".tsv")
        paste0("func_redundancy", ext)
      },
      content = function(file) {
        dt <- local_rv$data_rda
        if (!is.null(dt) && is.data.frame(dt)) {
          write.table(dt, file, sep = input$func_table_format_rda, row.names = TRUE, quote = TRUE)
        }
      }
    )

    observeEvent(input$run_func_plot_fr, {
      if (!check_microtable(rv) || is.null(local_rv$t_func)) {
        showNotification("\u8bf7\u5148\u8ba1\u7b97\u5197\u4f59\u5206\u6790", type = "warning")
        return()
      }

      result <- tryCatch({
        p <- local_rv$t_func$plot_func_FR(
          add_facet = input$func_fr_facet,
          color_gradient_low = input$func_fr_color_low,
          color_gradient_high = input$func_fr_color_high
        )
        list(success = TRUE, plot = p)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      local_rv$plot_fr <- result$plot
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$func_plot_fr_plot <- shiny::renderPlot({
      req(local_rv$plot_fr)
      local_rv$plot_fr
    })

    observeEvent(input$run_func_bar, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      result <- tryCatch({
        mt <- rv$microtable
        t_func <- microeco::trans_func$new(dataset = mt)
        t_func$cal_func(func_type = input$func_func_type, filter_thres = input$func_filter_thres)

        plot_group_val <- if (nchar(input$func_plot_group_bar)) input$func_plot_group_bar else NULL
        p <- t_func$plot_func(
          plot_type = "bar",
          top_n = input$func_top_n_bar,
          color_values = get_color_palette_safe(input$func_color_theme_bar),
          plot_group = plot_group_val
        )
        list(success = TRUE, plot = p)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      local_rv$bar_plot <- result$plot
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$func_bar_plot <- shiny::renderPlot({
      req(local_rv$bar_plot)
      local_rv$bar_plot
    })

    observeEvent(input$run_func_heatmap, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      result <- tryCatch({
        mt <- rv$microtable
        t_func <- microeco::trans_func$new(dataset = mt)
        t_func$cal_func(func_type = input$func_func_type, filter_thres = input$func_filter_thres)

        plot_group_val <- if (nchar(input$func_plot_group_heatmap)) input$func_plot_group_heatmap else NULL
        p <- t_func$plot_func(
          plot_type = "heatmap",
          top_n = input$func_top_n_heatmap,
          color_values = get_color_palette_safe(input$func_color_theme_heatmap),
          plot_group = plot_group_val
        )
        list(success = TRUE, plot = p)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      local_rv$heatmap_plot <- result$plot
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$func_heatmap_plot <- shiny::renderPlot({
      req(local_rv$heatmap_plot)
      local_rv$heatmap_plot
    })

    get_color_palette_safe <- function(name) {
      get_color_palette(name, n = 8)
    }
  })
}
