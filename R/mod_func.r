#' @title Functional Prediction Module UI
#' @description
#' Provides interface for functional prediction using FAPROTAX (prokaryotes),
#' FUNGuild (fungi), Tax4Fun2, and functional redundancy analysis.
#' Supports complete parameter options from trans_func class.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @importFrom shinyFiles getVolumes parseDirPath shinyDirButton shinyDirChoose
#' @keywords functional prediction microbiome FAPROTAX FUNGuild
#' @family advanced-analysis
mod_func_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(column(12, h2(tr("\U0001f9ec 功能预测 Functional Prediction", "\U0001f9ec Functional Prediction")))),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4cb 参数设置", "\U0001f4cb Parameters"),
          status = "primary", solidHeader = TRUE, width = NULL, collapsible = FALSE,
          h4(tr("基本参数", "Basic Parameters")),
          fluidRow(
            column(3, shiny::selectInput(ns("func_type"), tr("数据类型", "Data Type"),
              choices = setNames(c("prok", "fungi"),
                                 c(tr("原核生物 (Prokaryotes)", "Prokaryotes"), tr("真菌 (Fungi)", "Fungi"))),
              selected = "prok")),
            column(3, shiny::selectInput(ns("func_prok_database"), tr("prok_database (原核生物数据库)", "prok_database"),
              choices = c("FAPROTAX", "NJC19"), selected = "FAPROTAX")),
            column(3, shiny::selectInput(ns("func_fungi_database"), tr("fungi_database (真菌数据库)", "fungi_database"),
              choices = c("FUNGuild", "FungalTraits"), selected = "FUNGuild")),
            column(3, shiny::selectInput(ns("func_group"), tr("分组列", "Group Column"),
              choices = character(0)))
          ),
          fluidRow(
            column(4, shiny::selectInput(ns("func_funguild_confidence"), tr("FUNGuild_confidence (置信度)", "FUNGuild_confidence"),
              choices = c("Highly Probable" = "Highly Probable",
                          "Probable" = "Probable",
                          "Possible" = "Possible"),
              selected = c("Highly Probable", "Probable", "Possible"),
              multiple = TRUE)),
            column(8)
          ),
          hr(),
          h4(tr("分析步骤", "Analysis Steps")),
          fluidRow(
            column(12, shiny::radioButtons(ns("func_analysis_step"), tr("分析步骤", "Analysis Steps"),
              choices = setNames(c("func", "redundancy", "plot_fr", "bar", "heatmap"),
                                 c(tr("1. 功能预测", "1. Functional Prediction"),
                                   tr("2. 功能冗余计算", "2. Functional Redundancy"),
                                   tr("3. 功能冗余绘图", "3. Functional Redundancy Plot"),
                                   tr("4. 柱状图", "4. Bar Plot"),
                                   tr("5. 热图", "5. Heatmap"))),
              selected = "func", inline = TRUE))
          ),
          shiny::conditionalPanel(condition = "input.func_analysis_step == 'redundancy' || input.func_analysis_step == 'plot_fr'", ns = ns,
            h4(paste0("cal_func_FR ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("func_fr_weighted"), tr("abundance_weighted (丰度加权)", "abundance_weighted"),
                value = FALSE, status = "info")),
              column(3, shinyWidgets::materialSwitch(ns("func_fr_adj_tax"), tr("adj_tax (检测调整)", "adj_tax"),
                value = FALSE, status = "warning")),
              column(3, shiny::selectInput(ns("func_fr_adj_tax_by"), "adj_tax_by",
                choices = c("Phylum", "Class", "Order", "Family", "Genus"), selected = "Genus")),
              column(3, shinyWidgets::materialSwitch(ns("func_fr_perc"), tr("perc (100%制)", "perc (100%)"),
                value = FALSE, status = "success"))
            ),
            fluidRow(
              column(3, shiny::numericInput(ns("func_fr_dec"), tr("dec (小数位数)", "dec (decimals)"),
                value = 6, min = 1, max = 10)),
              column(3, shinyWidgets::materialSwitch(ns("func_fr_remove_zero"), "remove_zero",
                value = TRUE, status = "primary")),
              column(6)
            )
          ),
          shiny::conditionalPanel(condition = "input.func_analysis_step == 'plot_fr'", ns = ns,
            h4(paste0("plot_func_FR ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("func_fr_facet"), "add_facet",
                value = TRUE, status = "info")),
              column(3, shiny::textInput(ns("func_fr_color_low"), "color_gradient_low",
                value = "#00008B")),
              column(3, shiny::textInput(ns("func_fr_color_high"), "color_gradient_high",
                value = "#9E0142")),
              column(3)
            )
          ),
          shiny::conditionalPanel(condition = "input.func_analysis_step == 'bar' || input.func_analysis_step == 'heatmap'", ns = ns,
            h4(tr("图形参数", "Plot Parameters")),
            fluidRow(
              column(3, shiny::numericInput(ns("func_top_n"), tr("top_n (显示前N)", "top_n (show top N)"),
                value = 20, min = 5, max = 100)),
              column(3, shiny::selectInput(ns("func_color_theme"), "color_theme",
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"),
                selected = "Spectral")),
              column(3, shiny::selectInput(ns("func_plot_group"), tr("分组", "Group"),
                choices = setNames(c("", "group"), c(tr("无", "None"), "Group")))),
              column(3)
            )
          ),
          hr(),
          fluidRow(
            column(2, shiny::actionButton(ns("run_func"), tr("\U0001f4ca 执行", "\U0001f4ca Run"),
              icon = icon("play"), class = "btn-primary", width = "100%")),
            column(2, shiny::selectInput(ns("func_image_format"), tr("格式", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("func_save_width"), tr("宽", "Width"), value = 12, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("func_save_height"), tr("高", "Height"), value = 8, min = 4, max = 15)),
            column(2, shiny::numericInput(ns("func_save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(2, shiny::actionButton(ns("func_save_plot_btn"), tr("\U0001f4e5保存图片", "\U0001f4e5Save Plot"),
              icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
          ),
          fluidRow(
            column(2, shiny::selectInput(ns("func_table_download_type"), tr("下载类型", "Download Type"),
              choices = setNames(c("func", "fr", "fr_comm"), c(tr("功能表", "Function Table"), tr("冗余表", "Redundancy Table"), tr("群体FR", "Community FR"))),
              selected = "func")),
            column(2, shiny::selectInput(ns("func_table_format"), tr("表格", "Table"),
              choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
            column(2, shiny::downloadButton(ns("func_download_table"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
              class = "btn-outline-info", width = "100%")),
            column(6)
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
          shinycssloaders::withSpinner(shiny::plotOutput(ns("func_plot"), height = "550px"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("func_table"))
        )
      )
    )
  )
}

mod_func_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(
      plot = NULL,
      data_result = NULL,
      save_dir = NULL,
      result_obj = NULL,
      table_type = "func"
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
        updateTextInput(session, "func_save_dir_text", value = parsed)
      }
    })

    observeEvent(input$func_save_plot_btn, {
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
      req(local_rv$plot)
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

      tryCatch({
        ggplot2::ggsave(filename = full_path, plot = local_rv$plot,
          width = input$func_save_width, height = input$func_save_height,
          units = "in", dpi = input$func_save_dpi, scale = 1)
        removeModal()
        showNotification(paste0("\u2705 \u5df2\u4fdd\u5b58\u81f3: ", full_path), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("\u4fdd\u5b58\u5931\u8d25: ", e$message), type = "error", duration = 10)
      })
    })

    observe({
      if (!check_microtable(rv)) {
        updateSelectInput(session, "func_group", choices = character(0))
        return()
      }
      cols <- get_sample_cols(rv)
      updateSelectInput(session, "func_group", choices = c("\u65e0" = "", cols))
    })

    get_color_palette <- function(name) {
      tryCatch({
        switch(name,
          "Dark2"    = RColorBrewer::brewer.pal(8, "Dark2"),
          "Set1"     = RColorBrewer::brewer.pal(8, "Set1"),
          "Set2"     = RColorBrewer::brewer.pal(8, "Set2"),
          "Set3"     = RColorBrewer::brewer.pal(12, "Set3"),
          "Paired"   = RColorBrewer::brewer.pal(12, "Paired"),
          "Spectral" = RColorBrewer::brewer.pal(11, "Spectral"),
          "Viridis"  = viridisLite::viridis(8),
          RColorBrewer::brewer.pal(11, "Spectral")
        )
      }, error = function(e) RColorBrewer::brewer.pal(11, "Spectral"))
    }

    observeEvent(input$run_func, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      step <- input$func_analysis_step
      result <- tryCatch({
        dataset_name <- rv$microtable_name %||% "tmp_microtable"
        t_func <- rv$func_obj

        init_code <- paste0(
          "t_func <- microeco::trans_func$new(\n",
          "  dataset = ", dataset_name, "\n",
          ")\n"
        )

        if (step == "func") {
          if (is.null(t_func)) {
            t_func <- microeco::trans_func$new(dataset = rv$microtable)
          }

          if (input$func_type == "prok") {
            t_func$cal_func(prok_database = input$func_prok_database)
            db_param <- paste0("prok_database = \"", input$func_prok_database, "\"")
          } else {
            confidence_vec <- input$func_funguild_confidence
            if (length(confidence_vec) == 0) confidence_vec <- "Highly Probable"

            t_func$cal_func(
              fungi_database = input$func_fungi_database,
              FUNGuild_confidence = confidence_vec
            )
            db_param <- paste0("fungi_database = \"", input$func_fungi_database, "\"")
          }

          rv$func_obj <- t_func
          local_rv$table_type <- "func"

          func_code <- paste0(
            "t_func$cal_func(", db_param, ")\n"
          )
          code <- paste0(init_code, "# 1. \u529f\u80fd\u9884\u6d4b\n", func_code)

          list(success = TRUE, plot = NULL, data_result = t_func$res_func,
               result_obj = t_func, code = code, step = "func")

        } else if (step == "redundancy") {
          if (is.null(t_func) || is.null(t_func$res_func)) {
            showNotification("\u8bf7\u5148\u6267\u884c\u529f\u80fd\u9884\u6d4b", type = "warning")
            return()
          }

          t_func$cal_func_FR(
            abundance_weighted = input$func_fr_weighted,
            adj_tax = input$func_fr_adj_tax,
            adj_tax_by = input$func_fr_adj_tax_by,
            perc = input$func_fr_perc,
            dec = input$func_fr_dec,
            remove_zero = input$func_fr_remove_zero
          )

          rv$func_obj <- t_func
          local_rv$table_type <- "fr"

          fr_code <- paste0(
            "t_func$cal_func_FR(\n",
            "  abundance_weighted = ", input$func_fr_weighted, ",\n",
            "  adj_tax = ", input$func_fr_adj_tax, ",\n",
            "  adj_tax_by = \"", input$func_fr_adj_tax_by, "\",\n",
            "  perc = ", input$func_fr_perc, ",\n",
            "  dec = ", input$func_fr_dec, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# 2. \u529f\u80fd\u5197\u4f59\u8ba1\u7b97\n", fr_code)

          list(success = TRUE, plot = NULL, data_result = t_func$res_func_FR,
               result_obj = t_func, code = code, step = "redundancy")

        } else if (step == "plot_fr") {
          if (is.null(t_func) || is.null(t_func$res_func_FR)) {
            showNotification("\u8bf7\u5148\u6267\u884c\u529f\u80fd\u5197\u4f59\u8ba1\u7b97", type = "warning")
            return()
          }

          t_func$trans_func_FR()

          p <- t_func$plot_func_FR(
            add_facet = input$func_fr_facet,
            order_x = NULL,
            color_gradient_low = input$func_fr_color_low,
            color_gradient_high = input$func_fr_color_high
          )

          rv$func_obj <- t_func
          local_rv$table_type <- "fr"

          plot_fr_code <- paste0(
            "t_func$trans_func_FR()\n",
            "p <- t_func$plot_func_FR(\n",
            "  add_facet = ", input$func_fr_facet, ",\n",
            "  color_gradient_low = \"", input$func_fr_color_low, "\",\n",
            "  color_gradient_high = \"", input$func_fr_color_high, "\"\n",
            ")\n"
          )
          code <- paste0(init_code, "# 3. \u529f\u80fd\u5197\u4f59\u7ed8\u56fe\n", plot_fr_code)

          list(success = TRUE, plot = p, data_result = t_func$res_func_FR_trans,
               result_obj = t_func, code = code, step = "plot_fr")

        } else if (step == "bar" || step == "heatmap") {
          if (is.null(t_func) || is.null(t_func$res_func)) {
            showNotification("\u8bf7\u5148\u6267\u884c\u529f\u80fd\u9884\u6d4b", type = "warning")
            return()
          }

          group_val <- if (nchar(input$func_plot_group)) input$func_plot_group else NULL
          color_vals <- get_color_palette(input$func_color_theme)

          trans_abund_obj <- microeco::trans_abund$new(
            dataset = rv$microtable,
            taxrank = "Genus"
          )

          func_matrix <- as.matrix(t_func$res_func)
          if (nrow(func_matrix) == 0 || ncol(func_matrix) == 0) {
            stop("\u529f\u80fd\u9884\u6d4b\u7ed3\u679c\u4e3a\u7a7a\uff0c\u8bf7\u68c0\u67e5\u6570\u636e")
          }

          func_sum <- colSums(func_matrix)
          top_funcs <- names(sort(func_sum, decreasing = TRUE))[1:input$func_top_n]
          func_matrix_top <- func_matrix[, top_funcs, drop = FALSE]

          plot_data <- as.data.frame(func_matrix_top)
          plot_data <- cbind.data.frame(sample = rownames(plot_data), plot_data)
          plot_data <- reshape2::melt(plot_data, id.vars = "sample")

          if (!is.null(group_val) && nchar(group_val) > 0) {
            sample_info <- rv$microtable$sample_table[, group_val, drop = FALSE]
            plot_data <- merge(plot_data, sample_info, by.x = "sample", by.y = "row.names")
          }

          if (step == "bar") {
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$sample, y = .data$value, fill = .data$variable)) +
              ggplot2::geom_bar(stat = "identity", position = "stack") +
              ggplot2::theme_bw() +
              ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
                legend.position = "right",
                legend.text = ggplot2::element_text(size = 8)
              ) +
              ggplot2::labs(x = NULL, y = "Abundance", fill = "Function") +
              ggplot2::scale_fill_manual(values = color_vals)
          } else {
            plot_data_wide <- reshape2::dcast(sample ~ variable, data = plot_data, fun.aggregate = sum, value.var = "value")
            rownames(plot_data_wide) <- plot_data_wide$sample
            plot_data_wide <- plot_data_wide[, -1]
            plot_data_wide <- as.matrix(plot_data_wide)

            plot_data_wide <- t(scale(t(plot_data_wide)))
            plot_data_wide[is.na(plot_data_wide)] <- 0

            p <- ggplot2::ggplot(
              reshape2::melt(plot_data_wide),
              ggplot2::aes(x = Var2, y = Var1, fill = value)
            ) +
              ggplot2::geom_tile() +
              ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b") +
              ggplot2::theme_bw() +
              ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
                axis.text.y = ggplot2::element_text(size = 9)
              ) +
              ggplot2::labs(x = NULL, y = NULL, fill = "Z-score")
          }

          local_rv$table_type <- "func"

          vis_code <- paste0(
            "# \u529f\u80fd", if (step == "bar") "\u67f1\u72b6\u56fe" else "\u70ed\u56fe", "\n",
            "p <- ggplot(...) + ... \n"
          )
          code <- paste0(init_code, "# 4/5. \u56fe\u5f62\u5316\n", vis_code)

          list(success = TRUE, plot = p, data_result = as.data.frame(func_matrix_top),
               result_obj = t_func, code = code, step = step)

        } else {
          stop("\u672a\u77e5\u5206\u6790\u6b65\u9aa4")
        }
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      append_code(rv, result$code, paste0("\u529f\u80fd\u9884\u6d4b - ", result$step))
      local_rv$plot <- result$plot
      local_rv$data_result <- result$data_result
      local_rv$result_obj <- result$result_obj
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$func_plot <- shiny::renderPlot({
      req(local_rv$plot)
      if (is(local_rv$plot, "ggplot")) {
        print(local_rv$plot)
      } else {
        print(local_rv$plot)
      }
    })

    output$func_table <- DT::renderDataTable({
      dt <- local_rv$data_result
      if (is.data.frame(dt) || is.matrix(dt)) {
        dt_df <- as.data.frame(dt)
        DT::datatable(dt_df, options = list(scrollX = TRUE, pageLength = 20),
          rownames = TRUE, filter = "top")
      }
    })

    output$func_download_table <- downloadHandler(
      filename = function() {
        type <- input$func_table_download_type
        ext <- ifelse(input$func_table_format == ",", ".csv", ".tsv")
        paste0("func_", type, ext)
      },
      content = function(file) {
        obj <- local_rv$result_obj
        if (is.null(obj)) return()

        table_type <- input$func_table_download_type
        dt <- switch(table_type,
          "func" = {
            if (!is.null(obj$res_func)) as.data.frame(obj$res_func) else NULL
          },
          "fr" = {
            if (!is.null(obj$res_func_FR)) as.data.frame(obj$res_func_FR) else NULL
          },
          "fr_comm" = {
            if (!is.null(obj$res_func_FR)) {
              geometric_mean <- function(x) {
                exp(mean(log(x)))
              }
              tmp <- obj$res_func_FR
              test_min <- unlist(tmp)
              test_min <- test_min[test_min != 0] %>% min
              test_min_low <- floor(log10(min(test_min))) - 2
              test_min_low_add <- 10^(test_min_low)
              tmp[tmp == 0] <- test_min_low_add
              res_fr <- apply(tmp, 1, geometric_mean)
              data.frame(sample = names(res_fr), FR_community = res_fr)
            } else NULL
          },
          NULL
        )

        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$func_table_format,
            row.names = TRUE, quote = TRUE)
        }
      }
    )
  })
}