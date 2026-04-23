#' @title Generated Codes Module UI
#' @description
#' Provides interface for viewing and managing generated R code during analysis.
#' Displays code snippets with timestamps and allows copying/exporting.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash
#' @keywords code generation
#' @family utility
mod_codes_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(
      column(12,
        fluidRow(
          column(8,
            h2(tr("\U0001f4dd 生成代码 Generated Codes", "\U0001f4dd Generated Codes")),
            p(tr("这里显示您在应用中所有操作的 R 代码。代码可直接复制到 R 控制台执行。", "R code from all your operations in the app is shown here. Code can be directly copied to R console for execution."))
          ),
          column(4,
            div(
              style = "display: flex; gap: 8px; justify-content: flex-end; padding-top: 20px;",
              shiny::actionButton(ns("clear_codes"), tr("\U0001f5d1 清空", "\U0001f5d1 Clear"), icon = icon("trash"), class = "btn-danger"),
              shiny::actionButton(ns("copy_codes"), tr("\U0001f4cb 复制全部", "\U0001f4cb Copy All"), icon = icon("copy"), class = "btn-primary"),
              shiny::downloadButton(ns("save_codes"), tr("\U0001f4be 保存 .R", "\U0001f4be Save .R"), class = "btn-success")
            )
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4bb 代码编辑器", "\U0001f4bb Code Editor"),
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          shinyAce::aceEditor(
            outputId = ns("code_editor"),
            value = "",
            mode = "r",
            theme = "github",
            readOnly = TRUE,
            wordWrap = TRUE,
            fontSize = 13,
            showLineNumbers = TRUE,
            highlightActiveLine = FALSE,
            height = "65vh",
            autoComplete = "live",
            debounce = 300
          )
        )
      )
    )
  )
}

#' Generated Codes Module Server
#' @import shiny
#' @importFrom shinyAce updateAceEditor
mod_codes_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update code editor content
    observeEvent(rv$code_counter, {
      code_text <- paste(rv$generated_codes, collapse = "")
      theme_val <- if (rv$current_theme == "dark") "monokai" else "github"
      shinyAce::updateAceEditor(session, "code_editor", value = code_text, theme = theme_val)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Clear codes
    observeEvent(input$clear_codes, {
      clear_codes(rv)
      showNotification("\u2705 \u4ee3\u7801\u5df2\u6e05\u7a7a", type = "message")
    })

    # Copy codes to clipboard
    observeEvent(input$copy_codes, {
      code_text <- format_code_download(rv$generated_codes)
      session$sendCustomMessage("copy_to_clipboard", list(text = code_text))
      showNotification("\u2705 \u4ee3\u7801\u5df2\u590d\u5236\u5230\u526a\u8d34\u677f", type = "message")
    })

    # Save codes
    output$save_codes <- shiny::downloadHandler(
      filename = function() {
        paste0("microecoshiny_codes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        code_text <- format_code_download(rv$generated_codes)
        writeLines(code_text, file)
      }
    )

    # Pin codes panel state
    observeEvent(input$pin_codes, {
      rv$codes_pinned <- !rv$codes_pinned
    })
  })
}
