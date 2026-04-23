#' @title Taxonomic Abundance Calculation Module UI
#' @description
#' Provides interface for calculating and saving taxonomic abundance tables
#' at different taxonomic ranks using microeco functions.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @keywords abundance taxonomy microbiome
#' @family community-analysis
mod_abund_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(
      column(12, h2(tr("\U0001f4cd 类群丰度计算 Taxonomic Abundance", "\U0001f4cd Taxonomic Abundance")))
    ),
    fluidRow(
      column(8,
        bs4Dash::box(
          title = tr("\U0001f4cb 丰度计算", "\U0001f4cb Abundance Calculation"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,
          shiny::actionButton(ns("run_abund"), tr("\U0001f4ca 计算丰度", "\U0001f4ca Calculate Abundance"), icon = icon("calculator"), class = "btn-primary", width = "100%"),
          br(), br(),
          h5(tr("\U0001f4cb 分类水平", "\U0001f4cb Taxonomic Ranks")),
          shinyWidgets::pickerInput(
            inputId = ns("select_cols"),
            label = NULL,
            choices = character(0),
            selected = character(0),
            options = list(`actions-box` = TRUE, `multiple-separator` = " | "),
            multiple = TRUE
          ),
          div(style = "font-size: 0.85em; color: #666; margin-top: -5px; margin-bottom: 15px;", 
            tr("加载数据后自动显示分类水平（如 Kingdom, Phylum, Class）", "Taxonomic ranks will appear after data loading")
          ),
          h5(tr("\U0001f4cb 计算参数", "\U0001f4cb Calculation Parameters")),
          fluidRow(
            column(6,
              shinyWidgets::materialSwitch(
                inputId = ns("rel"),
                label = tr("相对丰度 (rel)", "Relative abundance (rel)"),
                value = TRUE,
                status = "primary"
              ),
              div(style = "font-size: 0.85em; color: #666;", 
                tr("计算相对丰度而非绝对丰度", "Calculate relative abundance instead of absolute")
              )
            ),
            column(6,
              shiny::textInput(
                inputId = ns("merge_by"),
                label = tr("连接符号 (merge_by)", "Connector (merge_by)"),
                value = "|"
              ),
              div(style = "font-size: 0.85em; color: #666; margin-top: -10px;", 
                tr("将多层类名名称连接的字符，默认为 \"|\"", "Character to connect multi-level taxon names, default \"|\"")
              )
            )
          ),
          hr(),
          h5(tr("\U0001f4cb 分组参数", "\U0001f4cb Grouping Parameters")),
          div(style = "font-size: 0.85em; color: #666; margin-bottom: 10px;", 
            tr("用于处理多标签数据，如某类群同时属于多个类别", "For handling multi-label data, e.g. taxa belonging to multiple categories")
          ),
          fluidRow(
            column(6,
              shinyWidgets::materialSwitch(
                inputId = ns("split_group"),
                label = tr("分组分列 (split_group)", "Split group columns (split_group)"),
                value = FALSE,
                status = "info"
              ),
              shiny::textInput(
                inputId = ns("split_by"),
                label = tr("分组符号 (split_by)", "Group separator (split_by)"),
                value = "&"
              ),
              div(style = "font-size: 0.85em; color: #666; margin-top: -10px;", 
                tr("分组字符，默认为 \"&\"，用于将多标签分离为单独类别", "Group separator, default \"&\", used to split multi-labels into separate categories")
              )
            ),
            column(6,
              shiny::selectInput(
                inputId = ns("split_column"),
                label = tr("分组列 (split_column)", "Group column (split_column)"),
                choices = character(0)
              ),
              div(style = "font-size: 0.85em; color: #666; margin-top: -5px;", 
                tr("指定按哪一列进行分组，为空时自动选择包含 split_by 的列", "Specify which column to group by. Auto-selects column containing split_by if empty")
              ),
              br(),
              shiny::textInput(
                inputId = ns("split_special_char"),
                label = tr("特殊分组字符", "Special group character"),
                value = "&&"
              ),
              div(style = "font-size: 0.85em; color: #666; margin-top: -10px;", 
                tr("默认 \"&&\", 当 tax_table 中检测到此字符时自动启用 split_group", "Default \"&&\", auto-enables split_group when detected in tax_table")
              )
            )
          )
        )
      ),
      column(4,
        bs4Dash::box(
          title = tr("\U0001f4e6 丰度保存", "\U0001f4e6 Abundance Save"),
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,
          shiny::textInput(
            inputId = ns("dirpath"),
            label = tr("保存目录", "Save Directory"),
            value = "taxa_abund"
          ),
          shinyWidgets::materialSwitch(
            inputId = ns("merge_all"),
            label = tr("合并所有层级 (merge_all)", "Merge all ranks (merge_all)"),
            value = FALSE,
            status = "success"
          ),
          div(style = "font-size: 0.85em; color: #666;", 
            tr("将所有层级的丰度合并为一个文件", "Merge abundance from all ranks into one file")
          ),
          br(),
          shinyWidgets::materialSwitch(
            inputId = ns("rm_un"),
            label = tr("移除未分类 (rm_un)", "Remove unclassified (rm_un)"),
            value = FALSE,
            status = "warning"
          ),
          div(style = "font-size: 0.85em; color: #666;", 
            tr("移除结尾为 \"__\" 的未分类", "Remove unclassified taxa ending with \"__\"")
          ),
          br(),
          shiny::radioButtons(
            inputId = ns("sep"),
            label = tr("文件格式", "File Format"),
            choices = c("CSV" = ",", "TSV" = "\t"),
            selected = ",",
            inline = TRUE
          ),
          br(),
          shiny::downloadButton(ns("download_zip"), tr("\U0001f4e5 下载ZIP", "\U0001f4e5 Download ZIP"), class = "btn-success", width = "100%")
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4ca 结果预览", "\U0001f4ca Results Preview"),
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,
          shiny::tabsetPanel(
            id = ns("abund_tabs"),
            type = "tabs",
            shiny::tabPanel(
              title = tr("丰度表格", "Abundance Table"),
              shiny::fluidRow(
                column(3,
                  shiny::selectInput(ns("view_rank"), tr("查看层级", "View Rank"), choices = character(0))
                ),
                column(3,
                  shiny::numericInput(ns("view_ntaxa"), tr("显示行数", "Display Rows"), value = 50, min = 10, max = 500)
                )
              ),
              DT::dataTableOutput(ns("abund_table"))
            ),
            shiny::tabPanel(
              title = tr("\U0001f4cb 运行日志", "\U0001f4cb Run Log"),
              shiny::verbatimTextOutput(ns("abund_log"))
            )
          )
        )
      )
    )
  )
}

#' Taxonomic Abundance Module Server
#'
#' @param id Module ID
#' @param rv Global reactiveValues
#' @import shiny
mod_abund_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    local_rv <- reactiveValues(
      taxa_abund = NULL,
      log_messages = character(0),
      zipfile_path = NULL
    )

    observe({
      if (!check_microtable(rv)) {
        updateSelectInput(session, "split_column", choices = character(0))
        return()
      }
      ranks <- get_tax_ranks(rv)
      updatePickerInput(session, "select_cols", choices = ranks, selected = ranks)
      updateSelectInput(session, "split_column", choices = c("\u65e0" = "", ranks))
      updateSelectInput(session, "view_rank", choices = character(0))
    })

    observeEvent(input$run_abund, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      result <- safe_run({
        mt <- rv$microtable$clone()

        select_cols_val <- input$select_cols
        if (length(select_cols_val) == 0) {
          stop("\u8bf7\u9009\u62e9\u81f3\u5c11\u4e00\u4e2a\u5206\u7c7b\u6c34\u5e73")
        }

        log_msg <- c("\U0001f4cb \u5f00\u59cb\u8ba1\u7b97\u4e30\u5ea6...", "")
        log_msg <- c(log_msg, paste0("\u9009\u5b9a\u5c42\u7ea7: ", paste(select_cols_val, collapse = ", ")))
        log_msg <- c(log_msg, paste0("rel = ", input$rel))
        log_msg <- c(log_msg, paste0("merge_by = \"", input$merge_by, "\""))
        log_msg <- c(log_msg, paste0("split_group = ", input$split_group))
        if (input$split_group) {
          log_msg <- c(log_msg, paste0("split_by = \"", input$split_by, "\""))
          if (isTRUE(nzchar(input$split_column))) {
            log_msg <- c(log_msg, paste0("split_column = \"", input$split_column, "\""))
          }
          log_msg <- c(log_msg, paste0("split_special_char = \"", input$split_special_char, "\""))
        }
        local_rv$log_messages <- log_msg

        mt$cal_abund(
          select_cols = select_cols_val,
          rel = input$rel,
          merge_by = input$merge_by,
          split_group = input$split_group,
          split_by = input$split_by,
          split_column = if (isTRUE(nzchar(input$split_column))) input$split_column else NULL,
          split_special_char = input$split_special_char
        )

        local_rv$taxa_abund <- mt$taxa_abund

        log_msg <- local_rv$log_messages
        log_msg <- c(log_msg, "", paste0("\U0001f4ca \u8ba1\u7b97\u5b8c\u6210\uff1a", length(mt$taxa_abund), "\u4e2a\u5c42\u7ea7"))
        for (nm in names(mt$taxa_abund)) {
          dim_info <- paste0("  ", nm, ": ", nrow(mt$taxa_abund[[nm]]), " \u7c7b \u00d7 ", ncol(mt$taxa_abund[[nm]]), " \u6837\u672c")
          log_msg <- c(log_msg, dim_info)
        }
        local_rv$log_messages <- log_msg

        ranks <- names(mt$taxa_abund)
        updateSelectInput(session, "view_rank", choices = ranks, selected = ranks[1])

        mt
      }, "\u7c7b\u7fa9\u4e30\u5ea6\u8ba1\u7b97\u5931\u8d25")

      if (result$success) {
        showNotification("\U0001f4ca \u4e30\u5ea6\u8ba1\u7b97\u5b8c\u6210", type = "message")

        dataset_name <- rv$microtable_name %||% "tmp_microtable"
        code <- paste0(
          dataset_name, '$cal_abund(\n',
          '  select_cols = c(', paste(sprintf('"%s"', input$select_cols), collapse = ", "), '),\n',
          '  rel = ', input$rel, ',\n',
          '  merge_by = "', input$merge_by, '",\n',
          '  split_group = ', input$split_group
        )
        if (input$split_group) {
          code <- paste0(code, ',\n  split_by = "', input$split_by, '"')
          if (isTRUE(nzchar(input$split_column))) {
            code <- paste0(code, ',\n  split_column = "', input$split_column, '"')
          }
          code <- paste0(code, ',\n  split_special_char = "', input$split_special_char, '"')
        }
        code <- paste0(code, '\n)\n')
        append_code(rv, code, "\u7c7b\u7fa9\u4e30\u5ea6\u8ba1\u7b97")
      } else {
        showNotification(result$error, type = "error", duration = 10)
      }
    })

    output$abund_table <- DT::renderDataTable({
      req(local_rv$taxa_abund)
      if (length(local_rv$taxa_abund) == 0) return(NULL)

      rank <- input$view_rank
      if (is.null(rank) || rank == "") {
        ranks <- names(local_rv$taxa_abund)
        if (length(ranks) == 0) return(NULL)
        rank <- ranks[1]
      }

      dat <- local_rv$taxa_abund[[rank]]
      if (is.null(dat)) return(NULL)

      ntaxa <- input$view_ntaxa
      dat <- dat[1:min(ntaxa, nrow(dat)), , drop = FALSE]

      DT::datatable(
        dat,
        options = list(
          scrollX = TRUE,
          pageLength = 20,
          lengthMenu = c(10, 20, 50, 100),
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        caption = paste0(rank, " - ", nrow(local_rv$taxa_abund[[rank]]), " \u7c7b \u00d7 ", ncol(local_rv$taxa_abund[[rank]]), " \u6837\u672c"),
        rownames = TRUE,
        filter = "top"
      )
    })

    output$abund_log <- renderText({
      paste(local_rv$log_messages, collapse = "\n")
    })

    output$download_zip <- downloadHandler(
      filename = function() {
        paste0(input$dirpath, ".zip")
      },
      content = function(file) {
        req(local_rv$taxa_abund)

        mt <- rv$microtable$clone()
        mt$taxa_abund <- local_rv$taxa_abund

        temp_base <- tempdir()
        dirpath <- file.path(temp_base, input$dirpath)
        if (dir.exists(dirpath)) {
          unlink(dirpath, recursive = TRUE)
        }
        dir.create(dirpath)

        mt$save_abund(
          dirpath = dirpath,
          merge_all = input$merge_all,
          rm_un = input$rm_un,
          sep = input$sep
        )

        zipfile <- tempfile(fileext = ".zip")
        old_wd <- getwd()
        setwd(temp_base)
        zip(zipfile, input$dirpath)
        setwd(old_wd)

        file.copy(zipfile, file)
        unlink(dirpath, recursive = TRUE)
        unlink(zipfile)

        log_msg <- local_rv$log_messages
        log_msg <- c(log_msg, "", paste0("\U0001f4e5 \u5df2\u751f\u6210ZIP\u4e0b\u8f7d"))
        local_rv$log_messages <- log_msg

        code <- paste0(
          dataset_name, '$save_abund(\n',
          '  dirpath = "', input$dirpath, '",\n',
          '  merge_all = ', input$merge_all, ',\n',
          '  rm_un = ', input$rm_un, ',\n',
          '  sep = "', input$sep, '"\n'
        )
        code <- paste0(code, ')\n')
        append_code(rv, code, "\u4e30\u5ea6\u4fdd\u5b58")
      }
    )
  })
}