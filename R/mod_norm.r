#' @title Normalization Module UI
#' @description
#' Provides interface for microbiome data normalization/standardization.
#' Supports multiple methods including rarefy, CLR, GMPR, CSS, TMM, DESeq2, etc.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash
#' @keywords normalization microbiome
#' @family data-processing
mod_norm_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h2(if (lang == "zh") "\u2696 数据标准化" else "\u2696 Normalization"),
        p(if (lang == "zh") "对 OTU/ASV 表进行标准化处理，生成新的标准化 microtable 对象。" else "Perform normalization on OTU/ASV tables, generating a new normalized microtable object.")
      )
    ),

    # Normalization method selection
    fluidRow(
      column(4,
        bs4Dash::box(
          title = if (lang == "zh") "选择标准化方法" else "Select Normalization Method",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,
          shiny::selectInput(
            inputId = ns("norm_method"),
            label = if (lang == "zh") "标准化方法" else "Normalization Method",
            choices = setNames(app_config$norm_methods, app_config$norm_methods),
            selected = "rarefy"
          ),
          shiny::uiOutput(ns("method_params")),
          br(),
          shiny::actionButton(
            inputId = ns("run_norm"),
            label = if (lang == "zh") "执行标准化" else "Run Normalization",
            icon = icon("play"),
            class = "btn-primary btn-lg"
          )
        )
      ),
      column(8,
        shiny::tabsetPanel(
          id = ns("norm_results"),
          type = "tabs",
          shiny::tabPanel(
            title = if (lang == "zh") "序列深度分布" else "Sequencing Depth Distribution",
            shinycssloaders::withSpinner(shiny::plotOutput(ns("plot_sample_sums")))
          ),
          shiny::tabPanel(
            title = if (lang == "zh") "数据预览" else "Data Preview",
            shinycssloaders::withSpinner(DT::dataTableOutput(ns("preview_norm_data")))
          ),
          shiny::tabPanel(
            title = if (lang == "zh") "已保存标准化数据" else "Saved Normalization Data",
            shiny::uiOutput(ns("saved_norm_list"))
          )
        )
      )
    ),

    # Status
    shiny::uiOutput(ns("norm_status"))
  )
}

#' Normalization Module Server
#'
#' @param id Module ID
#' @param rv Global reactiveValues
#' @import shiny
mod_norm_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamic parameter UI based on method
    output$method_params <- renderUI({
      method <- input$norm_method
      tagList(
        switch(method,
          "rarefy" = tagList(
            shiny::numericInput(
              inputId = ns("rarefy_size"),
              label = "\u62bd\u5e73\u6df1\u5ea6 (sample.size)",
              value = min(rv$microtable$sample_sums()) %||% 10000,
              min = 100
            ),
            shiny::numericInput(
              inputId = ns("rarefy_seed"),
              label = "\u968f\u673a\u79cd\u5b50 (seed)",
              value = 42
            )
          ),
          "CLR" = tagList(
            tags$p("\u4f7f\u7528\u4e2d\u5fc3\u5bf9\u6570\u6bd4\u8f6c\u6362\u5bf9\u8ba1\u6570\u6570\u636e\u8fdb\u884c\u6807\u51c6\u5316", class = "text-muted small"),
            shiny::checkboxInput(ns("clr_pseudo"), "\u6dfb\u52a0\u4f2a\u8ba1\u6570 (pseudocount)", value = TRUE),
            shiny::numericInput(ns("clr_pseudo_val"), "\u4f2a\u8ba1\u6570\u5927\u5c0f", value = 1)
          ),
          "rclr" = tagList(
            tags$p("\u7a00\u758f\u7248 CLR (Robust CLR)", class = "text-muted small")
          ),
          "GMPR" = tagList(
            tags$p("\u57fa\u4e8e\u51e0\u4f55\u5747\u503c\u7684\u591a\u6837\u6027\u6bd4\u4f8b\u56de\u5f52", class = "text-muted small")
          ),
          "CSS" = tagList(
            shiny::numericInput(
              inputId = ns("css_quantile"),
              label = "\u5206\u4f4d\u6570 (quantile)",
              value = 0.5,
              min = 0, max = 1, step = 0.1
            )
          ),
          "TMM" = tagList(
            tags$p("\u622a\u5c3e\u5747\u503c\u6807\u51c6\u5316 (Trimmed Mean of M-values)", class = "text-muted small")
          ),
          "RLE" = tagList(
            tags$p("\u76f8\u5bf9\u8868\u8fbe\u5bf9\u6570\u6807\u51c6\u5316", class = "text-muted small")
          ),
          "TSS" = tagList(
            tags$p("\u603b\u548c\u6807\u51c6\u5316 (Total Sum Scaling)", class = "text-muted small")
          ),
          "DESeq2" = tagList(
            tags$p("\u57fa\u4e8e DESeq2 \u4e2d\u4f4d\u6570\u6bd4\u4f8b\u56de\u5f52", class = "text-muted small"),
            shiny::textInput(ns("deseq2_formula"), "\u8bbe\u8ba1\u516c\u5f0f", value = "~1")
          ),
          "Wrench" = tagList(
            tags$p("Wrench \u6807\u51c6\u5316", class = "text-muted small"),
            shiny::numericInput(ns("wrench_eps"), "Epsilon", value = 0.5, step = 0.1)
          ),
          tags$p("\u8bf7\u9009\u62e9\u6807\u51c6\u5316\u65b9\u6cd5", class = "text-muted")
        )
      )
    })

    # Run normalization
    observeEvent(input$run_norm, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      result <- safe_run({
        method <- input$norm_method
        t_norm <- microeco::trans_norm$new(dataset = rv$microtable)
        norm_args <- list(method = method)
        if (method == "rarefy") {
          norm_args$sample.size <- input$rarefy_size
          norm_args$seed <- input$rarefy_seed
        }
        norm_result <- do.call(t_norm$norm, norm_args)
        norm_result
      }, "\u6807\u51c6\u5316\u5931\u8d25")

      if (result$success) {
        norm_table <- result$result
        # Store normalized data
        method_name <- input$norm_method
        if (is.null(rv$norm_data)) rv$norm_data <- list()
        rv$norm_data[[method_name]] <- norm_table
        rv$last_norm <- norm_table

        # Auto-apply: set as current microtable with formatted name
        original_name <- rv$microtable_name
        # 保存原始数据到 workspace（如果尚未保存）
        if (is.null(rv$workspace[[original_name]])) {
          rv$workspace[[original_name]] <- rv$microtable
        }
        # 保存标准化结果到 workspace
        new_name <- paste0(original_name, "_", method_name)
        rv$workspace[[new_name]] <- norm_table
        rv$workspace_active <- new_name
        # 自动应用：将标准化结果设为当前 microtable
        rv$microtable <- norm_table
        rv$microtable_name <- new_name
        rv$data_loaded <- TRUE
        rv$microtable_version <- rv$microtable_version + 1L

        showNotification(paste0("\u2705 ", method_name, " \u6807\u51c6\u5316\u5b8c\u6210"), type = "message")

        # Generate code
        code <- paste0('tmp_norm <- trans_norm$new(dataset = tmp_microtable)\n')
        if (input$norm_method == "rarefy") {
          code <- paste0(code, 'tmp_microtable_norm <- tmp_norm$norm(\n',
                         '  method = "', method_name, '",\n',
                         '  sample.size = ', input$rarefy_size, '\n)\n')
        } else {
          code <- paste0(code, 'tmp_microtable_norm <- tmp_norm$norm(method = "', method_name, '")\n')
        }
        append_code(rv, code, paste0("\u6570\u636e\u6807\u51c6\u5316 - ", method_name))
      } else {
        showNotification(result$error, type = "error", duration = 10)
      }
    })

    # Plot sample sums distribution
    output$plot_sample_sums <- shiny::renderPlot({
      mt <- if (!is.null(rv$last_norm)) rv$last_norm else rv$microtable
      if (is.null(mt)) return(NULL)

      sums <- mt$sample_sums()
      df <- data.frame(Sample = names(sums), Sum = sums)

      ggplot2::ggplot(df, ggplot2::aes(x = Sample, y = Sum)) +
        ggplot2::geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.7) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)) +
        ggplot2::labs(
          title = "\u6837\u672c\u5e8f\u5217\u6df1\u5ea6\u5206\u5e03",
          x = "\u6837\u672c",
          y = "\u5e8f\u5217\u6df1\u5ea6"
        )
    })

    # Preview normalized data
    output$preview_norm_data <- DT::renderDataTable({
      mt <- rv$last_norm
      if (is.null(mt)) return(NULL)
      DT::datatable(
        head(mt$otu_table, app_config$max_preview_rows),
        options = list(scrollX = TRUE),
        class = "compact stripe"
      )
    })

    # List saved normalized data
    output$saved_norm_list <- renderUI({
      if (is.null(rv$norm_data) || length(rv$norm_data) == 0) {
        return(tags$p("\u5c1a\u672a\u6267\u884c\u6807\u51c6\u5316", class = "text-muted"))
      }
      names_data <- names(rv$norm_data)
      tagList(
        tags$table(
          class = "table table-striped",
          tags$thead(tags$tr(
            tags$th("\u65b9\u6cd5"),
            tags$th("\u6837\u672c\u6570"),
            tags$th("\u7279\u5f81\u6570"),
            tags$th("\u64cd\u4f5c")
          )),
          tags$tbody(
            lapply(names_data, function(nm) {
              mt <- rv$norm_data[[nm]]
              tags$tr(
                tags$td(nm),
                tags$td(nrow(mt$sample_table)),
                tags$td(ncol(mt$otu_table)),
                tags$td(
                  shiny::actionButton(
                    inputId = ns(paste0("use_", nm)),
                    label = "\u4f7f\u7528\u4e3a\u5f53\u524d\u6570\u636e",
                    class = "btn-sm btn-success",
                    onclick = paste0('Shiny.onInputChange(\"', ns("use_norm"), '\", \"', nm, '\")')
                  )
                )
              )
            })
          )
        )
      )
    })

    # Use normalized data as current
    observeEvent(input$use_norm, {
      if (!is.null(input$use_norm) && isTRUE(nzchar(input$use_norm))) {
        if (!is.null(rv$norm_data[[input$use_norm]])) {
          rv$microtable <- rv$norm_data[[input$use_norm]]
          showNotification(paste0("\u2705 \u5df2\u5207\u6362\u4e3a ", input$use_norm, " \u6807\u51c6\u5316\u6570\u636e"), type = "message")
          code <- paste0('tmp_microtable <- tmp_microtable_norm # "', input$use_norm, '" \u6807\u51c6\u5316\u540e\u7684\u6570\u636e\n')
          append_code(rv, code, paste0("\u5207\u6362\u4e3a ", input$use_norm, " \u6807\u51c6\u5316\u6570\u636e"))
        }
      }
    })

    # Status
    output$norm_status <- renderUI({
      if (!is.null(rv$last_norm)) {
        tags$div(class = "alert alert-success",
                 "\u2705 \u6807\u51c6\u5316\u5b8c\u6210: ", nrow(rv$last_norm$sample_table),
                 " \u6837\u672c, ", nrow(rv$last_norm$otu_table), " \u7279\u5f91")
      }
    })
  })
}

# Null coalescing operator for older R versions
`%||%` <- function(a, b) if (is.null(a)) b else a
