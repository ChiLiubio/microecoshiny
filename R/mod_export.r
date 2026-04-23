#' @title Data Export Module UI
#' @description
#' Provides interface for exporting data and analysis results to various formats
#' including CSV, TSV, RDS, RData, and visualization plots.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash
#' @keywords export data
#' @family utility
mod_export_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tagList(
    fluidRow(column(12, h2(if (lang == "zh") "\U0001f4e5 数据导出" else "\U0001f4e5 Data Export"))),
    fluidRow(
      column(6,
        bs4Dash::box(
          title = if (lang == "zh") "导出 microtable 对象" else "Export microtable Object",
          status = "primary", solidHeader = TRUE, width = NULL,
          p(if (lang == "zh") "当前对象名: " else "Current object name: ", shiny::textOutput(ns("current_obj_name"), inline = TRUE)),
          shiny::uiOutput(ns("export_name_ui")),
          shiny::selectInput(ns("export_format"), if (lang == "zh") "导出格式" else "Export Format",
            choices = c("RData (.RData)" = "rdata", "RDS (.rds)" = "rds"), selected = "rdata"),
          shiny::downloadButton(ns("download_microtable"), if (lang == "zh") "下载对象" else "Download Object",
            icon = icon("download"), class = "btn-primary")
        )
      ),
      column(6,
        bs4Dash::box(
          title = if (lang == "zh") "导出表格数据" else "Export Table Data",
          status = "info", solidHeader = TRUE, width = NULL,
          shiny::selectInput(ns("export_table"), if (lang == "zh") "选择导出表" else "Select Table",
            choices = c("Feature Table" = "otu", "Sample Info" = "sample",
                        "Taxonomy" = "tax", "Alpha Diversity" = "alpha")),
          shiny::selectInput(ns("export_table_format"), if (lang == "zh") "导出格式" else "Export Format",
            choices = c("CSV" = "csv", "TSV" = "tsv", "Excel (.xlsx)" = "xlsx"), selected = "csv"),
          shiny::downloadButton(ns("download_table"), if (lang == "zh") "下载表格" else "Download Table")
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = if (lang == "zh") "生成代码" else "Generate Code",
          status = "warning", solidHeader = TRUE, width = 12,
          shiny::downloadButton(ns("download_codes"), if (lang == "zh") "下载生成代码 (.R)" else "Download Code (.R)", class = "btn-warning")
        )
      )
    )
  )
}

#' Data Export Module Server
#' @import shiny
mod_export_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Display current object name and export name input
    output$current_obj_name <- renderText({
      req(rv$microtable_name)
      rv$microtable_name
    })

    output$export_name_ui <- renderUI({
      default_name <- rv$microtable_name %||% "tmp_microtable"
      shiny::textInput(ns("export_name"), "\u5bfc\u51fa\u6587\u4ef6\u540d", value = default_name)
    })

    # Download microtable
    output$download_microtable <- shiny::downloadHandler(
      filename = function() {
        paste0(input$export_name, ".", input$export_format)
      },
      content = function(file) {
        if (!check_microtable(rv)) return()
        if (input$export_format == "rdata") {
          mt <- rv$microtable
          save(mt, file = file)
        } else {
          saveRDS(rv$microtable, file)
        }
      }
    )

    # Download table
    output$download_table <- shiny::downloadHandler(
      filename = function() {
        paste0(input$export_table, ".", input$export_table_format)
      },
      content = function(file) {
        if (!check_microtable(rv)) return()
        mt <- rv$microtable
        df <- switch(input$export_table,
          "otu" = as.data.frame(mt$otu_table),
          "sample" = as.data.frame(mt$sample_table),
          "tax" = as.data.frame(mt$tax_table),
          "alpha" = if (!is.null(rv$alpha_result) && !is.null(rv$alpha_result$alpha_div)) {
            as.data.frame(rv$alpha_result$alpha_div)
          } else data.frame()
        )
        if (nrow(df) == 0) return()

        switch(input$export_table_format,
          "csv" = write.csv(df, file, row.names = TRUE),
          "tsv" = write.table(df, file, sep = "\t", row.names = TRUE, quote = FALSE),
          "xlsx" = writexl::write_xlsx(df, file)
        )

        code <- paste0(
          'write.', input$export_table_format, '(\n',
          '  ', input$export_table, '_data,\n',
          '  file = "path/to/', input$export_table, '.', input$export_table_format, '"\n)\n'
        )
        append_code(rv, code, paste0("\u5bfc\u51fa ", input$export_table, " \u8868"))
      }
    )

    # Download generated codes
    output$download_codes <- shiny::downloadHandler(
      filename = function() {
        paste0("microecoshiny_codes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        code_text <- format_code_download(rv$generated_codes)
        writeLines(code_text, file)
      }
    )
  })
}
