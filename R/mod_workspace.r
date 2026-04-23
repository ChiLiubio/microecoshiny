## ============================================================================
## mod_workspace.r — Data Workspace Manager
## ============================================================================
#' @title Workspace Manager Module UI
#' @description
#' Provides interface for managing multiple microtable objects in workspace,
#' including saving, loading, and switching between datasets.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash
#' @keywords workspace management microtable
#' @family utility
mod_workspace_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  bs4Dash::box(
    title = tr("\U0001f4c1 数据工作区 Workspace", "\U0001f4c1 Data Workspace"),
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    maximizable = TRUE,
    shiny::uiOutput(ns("active_obj_info")),
    DT::dataTableOutput(ns("workspace_table")),
    fluidRow(
      column(4,
        shiny::actionButton(
          inputId = ns("rename_obj"),
          label = tr("✏ 重命名", "✏ Rename"),
          icon = icon("edit"),
          class = "btn-sm btn-outline-primary"
        )
      ),
      column(4,
        shiny::actionButton(
          inputId = ns("switch_obj"),
          label = tr("\U0001f504 切换为活动", "\U0001f504 Switch to Active"),
          icon = icon("exchange-alt"),
          class = "btn-sm btn-outline-success"
        )
      ),
      column(4,
        shiny::actionButton(
          inputId = ns("delete_obj"),
          label = tr("\U0001f5d1 删除", "\U0001f5d1 Delete"),
          icon = icon("trash"),
          class = "btn-sm btn-outline-danger"
        )
      )
    ),
    shiny::uiOutput(ns("rename_ui")),
    shiny::uiOutput(ns("active_obj_detail"))
  )
}

#' Workspace Manager Module Server
#'
#' @param id Module ID
#' @param rv Global reactiveValues
#' @import shiny
#' @importFrom DT renderDataTable datatable dataTableProxy selectRows
mod_workspace_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Helper: get workspace info as data frame ----
    get_workspace_df <- function() {
      ws <- rv$workspace
      if (length(ws) == 0) {
        if (check_microtable(rv)) {
          mt <- rv$microtable
          active_name <- rv$microtable_name
          data.frame(
            Name = active_name,
            Samples = nrow(mt$sample_table),
            Features = nrow(mt$tax_table),
            TaxRanks = ncol(mt$tax_table) - 1,
            HasTree = !is.null(mt$tree),
            HasSeq = !is.null(mt$DNA),
            Active = "\u2713",
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Name = character(0), Samples = integer(0),
            Features = integer(0), TaxRanks = integer(0),
            HasTree = logical(0), HasSeq = logical(0),
            Active = character(0), stringsAsFactors = FALSE
          )
        }
      } else {
        rows <- lapply(names(ws), function(nm) {
          mt <- ws[[nm]]
          if (inherits(mt, "microtable")) {
            data.frame(
              Name = nm,
              Samples = nrow(mt$sample_table),
              Features = nrow(mt$tax_table),
              TaxRanks = ncol(mt$tax_table) - 1,
              HasTree = !is.null(mt$tree),
              HasSeq = !is.null(mt$DNA),
              Active = if (isTRUE(nm == rv$microtable_name)) "\u2713" else "",
              stringsAsFactors = FALSE,
              row.names = NULL
            )
          } else {
            NULL
          }
        })
        if (check_microtable(rv)) {
          in_ws <- rv$microtable_name %in% names(ws)
          if (!in_ws) {
            mt <- rv$microtable
            rows <- c(rows, list(data.frame(
              Name = rv$microtable_name,
              Samples = nrow(mt$sample_table),
              Features = nrow(mt$tax_table),
              TaxRanks = ncol(mt$tax_table) - 1,
              HasTree = !is.null(mt$tree),
              HasSeq = !is.null(mt$DNA),
              Active = "\u2713",
              stringsAsFactors = FALSE,
              row.names = NULL
            )))
          }
        }
        do.call(rbind, rows)
      }
    }

    # ---- Workspace table ----
    output$workspace_table <- DT::renderDataTable({
      df <- get_workspace_df()
      if (nrow(df) == 0) {
        df <- data.frame(Name = "\u6682\u65e0\u6570\u636e", Samples = 0, Features = 0,
                         TaxRanks = 0, HasTree = FALSE, HasSeq = FALSE, Active = "",
                         stringsAsFactors = FALSE)
      }
      DT::datatable(
        df,
        options = list(
          selection = "single",
          pageLength = 10,
          dom = "t",
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = "compact"
      )
    })

    # ---- Active object info ----
    output$active_obj_info <- renderUI({
      active_name <- rv$microtable_name
      if (is.null(active_name) || !check_microtable(rv)) {
        tags$div(
          style = "padding: 8px; margin-bottom: 10px; background: #fff3cd; border-left: 3px solid #ffc107; border-radius: 4px;",
          icon("info-circle"), " \u6d3b\u52a8\u5bf9\u8c61: \u6682\u65e0\u6570\u636e"
        )
      } else {
        tags$div(
          style = "padding: 8px; margin-bottom: 10px; background: #d4edda; border-left: 3px solid #28a745; border-radius: 4px;",
          icon("star", style = "color: #F39C12;"), " \u6d3b\u52a8\u5bf9\u8c61: ", strong(active_name)
        )
      }
    })

    # ---- Selected object name ----
    selected_name <- reactiveVal(NULL)
    observeEvent(input$workspace_table_rows_selected, {
      df <- get_workspace_df()
      if (!is.null(input$workspace_table_rows_selected) && nrow(df) > 0) {
        selected_name(df[input$workspace_table_rows_selected, "Name"])
      }
    }, ignoreNULL = TRUE)

    # ---- Rename UI ----
    output$rename_ui <- renderUI({
      nm <- selected_name()
      req(nm)
      tagList(
        tags$div(
          style = "margin-top: 8px; padding: 8px; background: #f8f9fa; border-radius: 4px;",
          tags$p("\u9009\u4e2d: ", strong(nm)),
          shiny::textInput(
            inputId = ns("new_name"),
            label = NULL,
            value = nm,
            placeholder = "\u8f93\u5165\u65b0\u540d\u79f0",
            width = "60%"
          )
        )
      )
    })

    # ---- Rename object ----
    observeEvent(input$rename_obj, {
      nm <- selected_name()
      req(nm)
      new_nm <- input$new_name
      req(isTRUE(nzchar(new_nm)), new_nm != nm)

      if (nm %in% names(rv$workspace)) {
        old_val <- rv$workspace[[nm]]
        rv$workspace[[new_nm]] <- old_val
        rv$workspace[[nm]] <- NULL
      }
      if (rv$microtable_name == nm) {
        rv$microtable_name <- new_nm
      }
      showNotification(paste0("\u5df2\u91cd\u547d\u540d: ", nm, " \u2192 ", new_nm), type = "message")
    })

    # ---- Switch active object ----
    observeEvent(input$switch_obj, {
      nm <- selected_name()
      req(nm)

      if (nm %in% names(rv$workspace)) {
        rv$microtable <- rv$workspace[[nm]]
        rv$microtable_name <- nm
        rv$microtable_version <- rv$microtable_version + 1
        rv$data_loaded <- TRUE
        showNotification(paste0("\u5df2\u5207\u6362\u4e3a: ", nm), type = "message")
        append_code(rv, paste0('# \u5207\u6362\u6d3b\u52a8\u5bf9\u8c61\u4e3a "', nm, '"'), "\u5de5\u4f5c\u533a - \u5207\u6362\u6d3b\u52a8\u5bf9\u8c61")
      } else if (nm == rv$microtable_name) {
        showNotification(paste0(nm, " \u5df2\u662f\u5f53\u524d\u6d3b\u52a8\u5bf9\u8c61"), type = "message")
      }
    })

    # ---- Delete object ----
    observeEvent(input$delete_obj, {
      nm <- selected_name()
      req(nm)

      if (nm %in% names(rv$workspace)) {
        rv$workspace[[nm]] <- NULL
        if (rv$microtable_name == nm) {
          rv$microtable <- NULL
          rv$microtable_name <- NULL
          rv$data_loaded <- FALSE
        }
        showNotification(paste0("\u5df2\u5220\u9664: ", nm), type = "message")
      } else {
        showNotification("\u65e0\u6cd5\u5220\u9664\u5f53\u524d\u6d3b\u52a8\u5bf9\u8c61", type = "warning")
      }
    })

    # ---- Active object detail ----
    output$active_obj_detail <- renderUI({
      mt <- rv$microtable
      if (!check_microtable(rv)) return(NULL)

      print_output <- tryCatch({
        capture.output(print(mt), type = "output")
      }, error = function(e) character(0))
      print_text <- paste(print_output, collapse = "\n")

      tags$div(
        style = "margin-top: 8px; padding: 8px; background: #f0f4f8; border-radius: 4px; font-size: 0.8rem; max-height: 200px; overflow-y: auto;",
        tags$pre(style = "white-space: pre-wrap; margin: 0; font-size: 0.8rem;", print_text)
      )
    })
  })
}

#' Helper: save current microtable to workspace with a new name
#'
#' @param rv Global reactiveValues
#' @param name Name for the new workspace entry
#' @export
workspace_save <- function(rv, name) {
  if (!check_microtable(rv)) return(invisible(NULL))
  rv$workspace[[name]] <- rv$microtable
  rv$microtable_name <- name
  rv$microtable_version <- rv$microtable_version + 1
  invisible(TRUE)
}

#' Helper: get the active microtable info string for display
#'
#' @param rv Global reactiveValues
#' @return HTML tags with current data info
#' @export
active_data_info <- function(rv) {
  if (!check_microtable(rv)) return(NULL)
  mt <- rv$microtable
  n_samples <- nrow(mt$sample_table)
  n_features <- nrow(mt$tax_table)
  tags$div(
    style = "padding: 6px 10px; background: #eaf4fe; border-left: 3px solid #3498DB; border-radius: 4px; font-size: 0.85rem; margin-bottom: 10px;",
    tags$p(icon("database"), " ", rv$microtable_name),
    tags$p(
      "\u6837\u672c: ", n_samples,
      " | \u7279\u5f81: ", n_features,
      " | \u5206\u7c7b\u5c42\u7ea7: ", ncol(mt$tax_table) - 1,
      if (!is.null(mt$tree)) " | \U0001f333 \u7cfb\u7edf\u53d1\u80b2\u6811" else "",
      if (!is.null(mt$DNA)) " | \U0001f9ec \u4ee3\u8868\u5e8f\u5217" else ""
    )
  )
}
