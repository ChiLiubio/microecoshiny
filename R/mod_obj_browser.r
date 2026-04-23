## ============================================================================
## mod_obj_browser.r -- Microtable Object Browser (全局共享右侧面板)
## ============================================================================
#' @title Object Browser Module UI
#' @description
#' Provides interface for browsing microtable object properties including
#' sample information, OTU table, taxonomy table, and basic statistics.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash
#' @keywords object browser microtable
#' @family utility
mod_obj_browser_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  
  # Translations
  title <- if (lang == "en") "Object Browser" else "\u5bf9\u8c61\u6d4f\u89c8\u5668"
  refresh <- if (lang == "en") "Refresh" else "\u5237\u65b0"
  collapse <- if (lang == "en") "Collapse All" else "\u5168\u90e8\u6298\u53e0"
  expand <- if (lang == "en") "Expand All" else "\u5168\u90e8\u5c55\u5f00"
  search_ph <- if (lang == "en") "Search objects..." else "\u641c\u7d22\u5bf9\u8c61..."
  
  tagList(
    tags$div(class = "obj-browser-container",

      # ---- Header ----
      tags$div(class = "obj-browser-header",
        tags$div(class = "obj-browser-title",
          icon("database"), " ", title
        ),
        shiny::actionButton(ns("refresh_btn"), icon("sync-alt"), class = "btn-sm btn-outline-secondary obj-browser-icon-btn", title = refresh),
        shiny::actionButton(ns("collapse_all_btn"), icon("compress-alt"), class = "btn-sm btn-outline-secondary obj-browser-icon-btn", title = collapse),
        shiny::actionButton(ns("expand_all_btn"), icon("expand-alt"), class = "btn-sm btn-outline-secondary obj-browser-icon-btn", title = expand)
      ),

      # ---- Search ----
      tags$div(class = "obj-browser-search",
        shiny::textInput(ns("search"), NULL, placeholder = search_ph, width = "100%")
      ),

      # ---- Object count summary ----
      shiny::uiOutput(ns("browser_summary")),

      # ---- Object list ----
      tags$div(class = "obj-browser-list", shiny::uiOutput(ns("object_list"))),

      # ---- Action buttons (Apply / Copy / Delete) ----
      tags$div(class = "obj-browser-actionbar", shiny::uiOutput(ns("action_buttons"))),

      # ---- Detail: real-time overview (only selected) ----
      tags$div(class = "obj-detail-section", shiny::uiOutput(ns("detail_section")))
    ),
    # Modal placeholder
    shiny::uiOutput(ns("obj_modal_ui"))
  )
}

#' Object Browser Module Server
#' @param id Module ID
#' @param rv Global reactiveValues
#' @import shiny
mod_obj_browser_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- State ----
    modal_purpose <- reactiveVal("copy")
    selected_obj <- reactiveVal(character(0))

    # ---- Collect all microtable objects from rv ----
    get_all_objects <- reactive({
      rv$microtable_version
      objects <- list()

      # 1. Main active microtable
      if (check_microtable(rv)) {
        nm <- if (isTRUE(nzchar(rv$microtable_name))) rv$microtable_name else "tmp_microtable"
        objects[[nm]] <- list(
          name = nm, object = rv$microtable, origin = "import",
          is_active = TRUE, timestamp = Sys.time()
        )
      }

      # 2. Workspace objects
      ws <- rv$workspace
      if (length(ws) > 0) {
        for (nm in names(ws)) {
          mt <- ws[[nm]]
          if (inherits(mt, "microtable")) {
            objects[[nm]] <- list(
              name = nm, object = mt,
              origin = attr(mt, "origin") %||% "workspace",
              is_active = isTRUE(rv$workspace_active == nm),
              timestamp = attr(mt, "timestamp") %||% Sys.time()
            )
          }
        }
      }

      # 3. Normalized data objects
      norm <- rv$norm_data
      if (length(norm) > 0) {
        for (nm in names(norm)) {
          mt <- norm[[nm]]
          if (inherits(mt, "microtable")) {
            objects[[nm]] <- list(
              name = nm, object = mt, origin = "normalize",
              is_active = FALSE, timestamp = attr(mt, "timestamp") %||% Sys.time()
            )
          }
        }
      }

      objects
    })

    # ---- Object info helper ----
    get_obj_info <- function(mt) {
      list(
        n_samples = tryCatch(nrow(mt$sample_table), error = function(e) 0L),
        n_features = tryCatch(nrow(mt$otu_table), error = function(e) 0L),
        n_tax = tryCatch(nrow(mt$tax_table), error = function(e) 0L),
        total_seqs = tryCatch(sum(mt$otu_table), error = function(e) 0L),
        n_tax_ranks = tryCatch(ncol(mt$tax_table), error = function(e) 0L),
        has_sample = !is.null(tryCatch(mt$sample_table, error = function(e) NULL)),
        has_otu = !is.null(tryCatch(mt$otu_table, error = function(e) NULL)),
        has_tax = !is.null(tryCatch(mt$tax_table, error = function(e) NULL)),
        has_tree = !is.null(tryCatch(mt$phylo_tree, error = function(e) NULL)),
        has_seq = !is.null(tryCatch(mt$rep_fasta, error = function(e) NULL))
      )
    }

    # ---- Filtered objects (by search) ----
    filtered_objects <- reactive({
      objs <- get_all_objects()
      term <- input$search
      if (isTRUE(nzchar(trimws(term)))) {
        matches <- grepl(term, names(objs), ignore.case = TRUE)
        objs <- objs[matches]
      }
      objs
    })

    # ---- Origin badge ----
    origin_badge <- function(origin) {
      colors <- list(
        import = "primary", preprocess = "warning",
        normalize = "success", filter = "info", workspace = "secondary"
      )
      tags$span(
        class = paste0("badge badge-", colors[[origin]] %||% "secondary"),
        style = "font-size:0.6rem;padding:1px 5px;border-radius:3px;",
        origin
      )
    }

    # ---- Compact component status icons ----
    comp_icons <- function(info) {
      items <- list(
        list(k = "has_sample", l = "S"),
        list(k = "has_otu", l = "O"),
        list(k = "has_tax", l = "T"),
        list(k = "has_tree", l = "Tr"),
        list(k = "has_seq", l = "Sq")
      )
      tags$div(class = "obj-comp-icons",
        lapply(items, function(it) {
          tags$span(
            class = if (isTRUE(info[[it$k]])) "comp-on" else "comp-off",
            it$l
          )
        })
      )
    }

    # ---- Summary ----
    output$browser_summary <- renderUI({
      n <- length(get_all_objects())
      lang <- rv$current_language
      tags$div(
        class = "obj-browser-summary",
        if (n == 0) tr("obj.browser.no_objects", lang) else sprintf(tr("obj.browser.total_objects", lang), n)
      )
    })

    # ---- Object list rendering ----
    output$object_list <- renderUI({
      lang <- rv$current_language
      objs <- filtered_objects()
      if (length(objs) == 0) {
        return(tags$div(
          class = "obj-browser-empty",
          tags$i(class = "fas fa-inbox"), tags$br(),
          tr("obj.browser.not_found", lang),
          tags$small(tr("obj.browser.import_hint", lang))
        ))
      }

      tagList(lapply(names(objs), function(nm) {
        item <- objs[[nm]]
        info <- get_obj_info(item$object)
        is_sel <- identical(selected_obj(), nm)
        is_act <- isTRUE(item$is_active)

        cls <- paste0("obj-card",
          if (is_act) " obj-card-active",
          if (is_sel) " obj-card-selected"
        )

        tags$div(
          class = cls,
          # Clickable object name (the card header)
          tags$div(
            class = "obj-card-name-row",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              ns("obj_click"), nm
            ),
            tags$div(class = "obj-card-name-main",
              tags$strong(class = "obj-card-name",
                if (is_act) tags$i(class = "fas fa-star", style = "color:#F39C12;font-size:0.65rem;margin-right:2px;") else NULL,
                nm
              ),
              tags$div(class = "obj-card-meta",
                origin_badge(item$origin),
                tags$span(class = "obj-card-dim",
                  if (!is.na(info$n_samples) && !is.na(info$n_features))
                    paste0(info$n_samples, " \u00d7 ", info$n_features)
                  else "\u6570\u636e\u5f02\u5e38"
                )
              )
            )
          ),
          # Component icons
          tags$div(class = "obj-card-bottom", comp_icons(info))
        )
      }))
    })

    # ---- Handle click on object name (single shared handler) ----
    observeEvent(input$obj_click, {
      nm <- input$obj_click
      if (length(nm) != 1 || !isTRUE(nzchar(nm))) return()
      prev <- selected_obj()
      selected_obj(if (identical(prev, nm)) character(0) else nm)
    })

    # ---- Action buttons (only when an object is selected) ----
    output$action_buttons <- renderUI({
      nm <- selected_obj()
      if (length(nm) != 1 || !isTRUE(nzchar(nm))) {
        return(tags$div(
          class = "action-placeholder",
          "\u70b9\u51fb\u5bf9\u8c61\u540d\u79f0\u4ee5\u67e5\u770b\u8be6\u60c5"
        ))
      }

      is_current <- identical(rv$microtable_name, nm)

      tagList(
        tags$div(class = "action-btn-group",
          shiny::actionButton(ns("apply_btn"), "\u5e94\u7528", icon = icon("play"),
            class = "btn-sm btn-primary action-btn"),
          shiny::actionButton(ns("copy_btn"), "\u590d\u5236", icon = icon("copy"),
            class = "btn-sm btn-info action-btn"),
          shiny::actionButton(ns("rename_btn"), "\u91cd\u547d\u540d", icon = icon("pencil-alt"),
            class = "btn-sm btn-secondary action-btn"),
          shiny::actionButton(ns("delete_btn"), "", icon = icon("trash"),
            class = "btn-sm btn-outline-danger action-btn action-btn-danger")
        ),
        if (is_current) {
          tags$div(class = "active-label-full",
            tags$i(class = "fas fa-check-circle"), " \u5f53\u524d\u6d3b\u8dc3"
          )
        }
      )
    })

    # ---- Apply: set as active microtable ----
    observeEvent(input$apply_btn, {
      nm <- selected_obj()
      if (length(nm) != 1 || !isTRUE(nzchar(nm))) return()

      # Save current active microtable to workspace before switching
      if (check_microtable(rv) && isTRUE(nzchar(rv$microtable_name))) {
        old_name <- rv$microtable_name
        if (old_name != nm && !old_name %in% names(rv$workspace)) {
          rv$workspace[[old_name]] <- rv$microtable
          showNotification(paste0("\U0001f4be \u5df2\u4fdd\u5b58\u5f53\u524d\u5bf9\u8c61: ", old_name), type = "message")
        }
      }

      if (nm %in% names(rv$workspace)) {
        rv$microtable <- rv$workspace[[nm]]
        rv$microtable_name <- nm
        rv$workspace_active <- nm
        rv$data_loaded <- TRUE
      } else if (nm %in% names(rv$norm_data)) {
        rv$microtable <- rv$norm_data[[nm]]
        rv$microtable_name <- nm
        rv$data_loaded <- TRUE
      }

      showNotification(paste0("\u2705 \u5df2\u5e94\u7528: ", nm), type = "message")
      append_code(rv, paste0('# \u5e94\u7528\u5bf9\u8c61: ', nm), "\u5bf9\u8c61\u6d4f\u89c8\u5668")
    })

    # ---- Copy: open modal for new name ----
    observeEvent(input$copy_btn, {
      nm <- selected_obj()
      if (length(nm) != 1 || !isTRUE(nzchar(nm))) return()

      modal_purpose("copy")
      showModal(shiny::modalDialog(
        title = "\u590d\u5236\u5bf9\u8c61",
        "\u8bf7\u8f93\u5165\u65b0\u5bf9\u8c61\u7684\u540d\u79f0\uff1a",
        shiny::textInput(
          inputId = ns("modal_new_name"),
          label = NULL,
          value = paste0(nm, "_copy"),
          placeholder = "\u65b0\u5bf9\u8c61\u540d\u79f0...",
          width = "100%"
        ),
        footer = tagList(
          shiny::modalButton("\u53d6\u6d88"),
          shiny::actionButton(ns("modal_confirm"), "\u786e\u8ba4", class = "btn-primary")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Modal confirm (copy) ----
    observeEvent(input$modal_confirm, {
      nm <- selected_obj()
      new_name <- trimws(input$modal_new_name)

      if (length(new_name) != 1 || !isTRUE(nzchar(new_name))) {
        showNotification("\u8bf7\u8f93\u5165\u540d\u79f0", type = "warning")
        return()
      }
      if (new_name %in% names(rv$workspace) && new_name != nm) {
        showNotification("\u5bf9\u8c61\u540d\u5df2\u5b58\u5728", type = "warning")
        return()
      }

      purpose <- modal_purpose()

      if (purpose == "copy") {
        result <- safe_run({
          src <- rv$workspace[[nm]] %||% rv$norm_data[[nm]] %||% rv$microtable
          if (is.null(src)) stop("\u627e\u4e0d\u5230\u6e90\u5bf9\u8c61")

          if (inherits(src, "microtable")) {
            cloned <- src$clone()
          } else {
            cloned <- src
          }
          attr(cloned, "origin") <- "copy"
          attr(cloned, "timestamp") <- Sys.time()
          rv$workspace[[new_name]] <- cloned
          new_name
        }, "\u590d\u5236\u5931\u8d25")

        if (result$success) {
          showNotification(paste0("\u2705 \u5df2\u590d\u5236\u4e3a: ", new_name), type = "message")
          append_code(rv, paste0('# \u590d\u5236\u5bf9\u8c61: ', nm, ' -> ', new_name), "\u5bf9\u8c61\u6d4f\u89c8\u5668")
        } else {
          showNotification(result$error, type = "error", duration = 10)
          return()
        }
      } else if (purpose == "rename") {
        result <- safe_run({
          if (nm %in% names(rv$workspace)) {
            rv$workspace[[new_name]] <- rv$workspace[[nm]]
            rv$workspace[[nm]] <- NULL
          } else if (nm %in% names(rv$norm_data)) {
            rv$norm_data[[new_name]] <- rv$norm_data[[nm]]
            rv$norm_data[[nm]] <- NULL
          } else if (identical(nm, rv$microtable_name)) {
            rv$microtable_name <- new_name
          }
          if (identical(rv$workspace_active, nm)) rv$workspace_active <- new_name
          new_name
        }, "\u91cd\u547d\u540d\u5931\u8d25")

        if (result$success) {
          selected_obj(new_name)
          showNotification(paste0("\u2705 \u5df2\u91cd\u547d\u540d\u4e3a: ", new_name), type = "message")
          append_code(rv, paste0('# \u91cd\u547d\u540d\u5bf9\u8c61: ', nm, ' -> ', new_name), "\u5bf9\u8c61\u6d4f\u89c8\u5668")
        } else {
          showNotification(result$error, type = "error", duration = 10)
          return()
        }
      }

      removeModal()
    })

    # ---- Rename: open modal for new name ----
    observeEvent(input$rename_btn, {
      nm <- selected_obj()
      if (length(nm) != 1 || !isTRUE(nzchar(nm))) return()

      modal_purpose("rename")
      showModal(shiny::modalDialog(
        title = "\u91cd\u547d\u540d\u5bf9\u8c61",
        "\u8bf7\u8f93\u5165\u65b0\u5bf9\u8c61\u7684\u540d\u79f0\uff1a",
        shiny::textInput(
          inputId = ns("modal_new_name"),
          label = NULL,
          value = nm,
          placeholder = "\u65b0\u5bf9\u8c61\u540d\u79f0...",
          width = "100%"
        ),
        footer = tagList(
          shiny::modalButton("\u53d6\u6d88"),
          shiny::actionButton(ns("modal_confirm"), "\u786e\u8ba4", class = "btn-primary")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Delete ----
    observeEvent(input$delete_btn, {
      nm <- selected_obj()
      if (length(nm) != 1 || !isTRUE(nzchar(nm))) return()

      if (nm %in% names(rv$workspace)) {
        rv$workspace[[nm]] <- NULL
        if (identical(rv$workspace_active, nm)) rv$workspace_active <- NULL
      } else if (nm %in% names(rv$norm_data)) {
        rv$norm_data[[nm]] <- NULL
      }

      # If deleting the active object, clear it
      if (identical(nm, rv$microtable_name)) {
        rv$microtable <- NULL
        rv$microtable_name <- "tmp_microtable"
        rv$data_loaded <- FALSE
        rv$workspace_active <- NULL
      }

      selected_obj(character(0))
      showNotification(paste0("\u5df2\u5220\u9664: ", nm), type = "message")
    })

    # ---- Detail section: only show selected object overview ----
    output$detail_section <- renderUI({
      nm <- selected_obj()
      objs <- get_all_objects()

      if (length(nm) != 1 || !isTRUE(nzchar(nm)) || !isTRUE(nm %in% names(objs))) {
        return(NULL)
      }

      mt <- objs[[nm]]$object
      if (is.null(mt)) {
        return(tags$div(class = "text-muted text-center p-3", tr("obj.browser.obj_lost", rv$current_language)))
      }

      info <- get_obj_info(mt)
      fmt_val <- function(v) if (is.null(v) || length(v) == 0 || is.na(v)) "0" else format(v, big.mark = ",")
      cls_str <- tryCatch(paste(class(mt), collapse = "/"), error = function(e) "unknown")

      tagList(
        # ---- Stats grid ----
        tags$div(class = "detail-grid",
          tags$div(class = "detail-stat",
            tags$div(class = "detail-stat-val", fmt_val(info$n_samples)),
            tags$div(class = "detail-stat-lbl", tr("obj.browser.samples", rv$current_language))
          ),
          tags$div(class = "detail-stat",
            tags$div(class = "detail-stat-val", fmt_val(info$n_features)),
            tags$div(class = "detail-stat-lbl", tr("obj.browser.features", rv$current_language))
          ),
          tags$div(class = "detail-stat",
            tags$div(class = "detail-stat-val", fmt_val(info$n_tax_ranks)),
            tags$div(class = "detail-stat-lbl", tr("obj.browser.tax_levels", rv$current_language))
          ),
          tags$div(class = "detail-stat",
            tags$div(class = "detail-stat-val", fmt_val(info$total_seqs)),
            tags$div(class = "detail-stat-lbl", tr("obj.browser.seqs", rv$current_language))
          )
        ),
        # ---- Component status ----
        tags$div(class = "detail-components",
          tags$div(class = "detail-comp-title", tr("obj.browser.comp_status", rv$current_language)),
          lapply(list(
            list(k = "has_sample", l = "Sample"),
            list(k = "has_otu", l = "OTU"),
            list(k = "has_tax", l = "Tax"),
            list(k = "has_tree", l = "Tree"),
            list(k = "has_seq", l = "Seq")
          ), function(it) {
            tags$span(
              class = if (isTRUE(info[[it$k]])) "detail-comp-on" else "detail-comp-off",
              it$l
            )
          })
        ),
        # ---- Object meta ----
        tags$div(class = "detail-meta",
          paste(nm, "|", cls_str)
        )
      )
    })

    # ---- Collapse All / Expand All ----
    observeEvent(input$collapse_all_btn, {
      selected_obj(character(0))
    })
    observeEvent(input$expand_all_btn, {
      objs <- get_all_objects()
      if (length(objs) > 0) {
        selected_obj(names(objs)[1])
      }
    })

    # ---- Refresh (reactive auto-updates, button is just feedback) ----
    observeEvent(input$refresh_btn, {
      showNotification("\u5df2\u5237\u65b0", type = "message", duration = 1)
    })
  })
}
