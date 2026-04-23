#' @title Beta Diversity Module UI
#' @description
#' Provides interface for beta diversity analysis including distance calculation,
#' ordination (PCoA, NMDS, PCA, DCA, PLS-DA, OPLS-DA), and statistical tests
#' (perMANOVA, ANOSIM, BETADISPER).
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @importFrom shinyFiles parseDirPath shinyDirButton shinyDirChoose
#' @keywords beta diversity ordination microbiome
#' @family community-analysis
mod_beta_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(
      column(12, h2(tr("\U0001f517 β多样性 Beta Diversity", "\U0001f517 Beta Diversity")))
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
            column(2, shiny::selectInput(ns("beta_measure"), tr("距离度量 (measure)", "measure"),
              choices = app_config$beta_measures, selected = "bray")),
            column(2, shiny::selectInput(ns("ord_method"), tr("排序方法 (method)", "method"),
              choices = c("PCoA", "NMDS", "PCA", "DCA", "PLS-DA", "OPLS-DA"), selected = "PCoA")),
            column(2, shiny::selectInput(ns("group_col"), tr("分组列 (group)", "group"), choices = character(0))),
            column(2, shiny::selectInput(ns("taxa_level"), "taxa_level", choices = character(0))),
            column(2, shiny::numericInput(ns("ncomp"), "ncomp", value = 2, min = 2, max = 10)),
            column(2, shiny::numericInput(ns("permutations"), "permutations", value = 999, min = 99, max = 9999, step = 100))
          ),
          hr(),
          h4(tr("排序参数", "Ordination Parameters")),
          fluidRow(
            column(2, shinyWidgets::materialSwitch(ns("NMDS_matrix"), "NMDS_matrix", value = TRUE, status = "info")),
            column(2, shinyWidgets::materialSwitch(ns("trans"), tr("trans (开方)", "trans (sqrt)"), value = FALSE, status = "warning")),
            column(2, shinyWidgets::materialSwitch(ns("scale_species"), "scale_species", value = FALSE, status = "info")),
            column(2, shiny::numericInput(ns("scale_species_ratio"), "scale_species_ratio", value = 0.8, min = 0.5, max = 1, step = 0.1)),
            column(4)
          ),
          hr(),
          h4(tr("图表类型 (plot_type)", "Plot Type (plot_type)")),
          fluidRow(
            column(3, shinyWidgets::materialSwitch(ns("add_point"), tr("点 ( point)", "point"), value = TRUE, status = "primary")),
            column(3, shinyWidgets::materialSwitch(ns("add_ellipse"), tr("椭圆 (ellipse)", "ellipse"), value = TRUE, status = "success")),
            column(3, shinyWidgets::materialSwitch(ns("add_chull"), tr("凸包 (chull)", "chull"), value = FALSE, status = "warning")),
            column(3, shinyWidgets::materialSwitch(ns("add_centroid"), tr("质心 (centroid)", "centroid"), value = FALSE, status = "info"))
          ),
          hr(),
          h4(tr("颜色和形状", "Color and Shape")),
          fluidRow(
            column(2, shiny::selectInput(ns("color_theme"), tr("颜色 (color_theme)", "color_theme"),
              choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
            column(2, shiny::selectInput(ns("plot_color"), "plot_color", choices = character(0))),
            column(2, shiny::selectInput(ns("plot_shape"), "plot_shape", choices = character(0))),
            column(2, shiny::selectInput(ns("choices_x"), tr("X轴 (choices X)", "X-axis"), choices = c(1,2,3,4,5), selected = 1)),
            column(2, shiny::selectInput(ns("choices_y"), tr("Y轴 (choices Y)", "Y-axis"), choices = c(1,2,3,4,5), selected = 2)),
            column(2, shiny::textInput(ns("shape_values"), "shape_values", value = "", placeholder = "e.g. c(16,17,15)"))
          ),
          fluidRow(
            column(3, shinyWidgets::materialSwitch(ns("order_group"), tr("分组排序 (plot_group_order)", "plot_group_order"), value = FALSE, status = "warning")),
            column(3, shiny::selectInput(ns("order_group_col"), "order_group_col", choices = character(0))),
            column(6)
          ),
          shiny::uiOutput(ns("order_group_ui")),
          hr(),
          h4(tr("点参数 (point)", "Point Parameters")),
          fluidRow(
            column(2, shiny::numericInput(ns("point_size"), "point_size", value = 3, min = 1, max = 10)),
            column(2, shiny::numericInput(ns("point_alpha"), "point_alpha", value = 0.8, min = 0.1, max = 1, step = 0.1)),
            column(2, shiny::selectInput(ns("add_sample_label"), tr("样本标签 (add_sample_label)", "add_sample_label"), choices = character(0))),
            column(2, shinyWidgets::materialSwitch(ns("point_second"), tr("第二层点 (point_second)", "point_second"), value = FALSE, status = "info")),
            column(2, shiny::numericInput(ns("point_second_size"), "point_second_size", value = 1.8, min = 0.5, max = 5)),
            column(2, shiny::numericInput(ns("point_second_alpha"), "point_second_alpha", value = 0.6, min = 0.1, max = 1, step = 0.1)),
            column(2, shiny::textInput(ns("point_second_color"), "point_second_color", value = ""))
          ),
          hr(),
          h4(tr("椭圆/凸包参数 (ellipse/chull)", "Ellipse/Chull Parameters")),
          fluidRow(
            column(2, shinyWidgets::materialSwitch(ns("ellipse_chull_fill"), tr("填充 (ellipse_chull_fill)", "fill"), value = TRUE, status = "info")),
            column(2, shiny::numericInput(ns("ellipse_chull_alpha"), "ellipse_chull_alpha", value = 0.1, min = 0, max = 1, step = 0.05)),
            column(2, shiny::numericInput(ns("ellipse_level"), "ellipse_level", value = 0.9, min = 0.5, max = 0.99, step = 0.05)),
            column(2, shiny::selectInput(ns("ellipse_type"), "ellipse_type", choices = c("t" = "t", "norm" = "norm", "euclid" = "euclid"), selected = "t")),
            column(4)
          ),
          hr(),
          h4(tr("质心参数 (centroid)", "Centroid Parameters")),
          fluidRow(
            column(2, shiny::numericInput(ns("centroid_segment_alpha"), "centroid_segment_alpha", value = 0.6, min = 0.1, max = 1, step = 0.1)),
            column(2, shiny::numericInput(ns("centroid_segment_size"), "centroid_segment_size", value = 1, min = 0.5, max = 3, step = 0.1)),
            column(2, shiny::selectInput(ns("centroid_segment_linetype"), "centroid_segment_linetype",
              choices = c("1-solid" = 1, "2-dashed" = 2, "3-dotted" = 3, "4-dotdash" = 4), selected = 3)),
            column(6)
          ),
          hr(),
          h4(tr("NMDS 应力值 (NMDS)", "NMDS Stress Value")),
          fluidRow(
            column(2, shinyWidgets::materialSwitch(ns("show_stress"), tr("显示应力值 (show_stress)", "show_stress"), value = TRUE, status = "info")),
            column(2, shiny::textInput(ns("NMDS_stress_pos"), "NMDS_stress_pos", value = "c(1, 1)")),
            column(2, shiny::textInput(ns("NMDS_stress_text_prefix"), "NMDS_stress_text_prefix", value = "")),
            column(6)
          ),
          hr(),
          h4(tr("载荷箭头 (loading_arrow)", "Loading Arrows")),
          fluidRow(
            column(2, shinyWidgets::materialSwitch(ns("loading_arrow"), "loading_arrow", value = FALSE, status = "warning")),
            column(2, shiny::numericInput(ns("loading_taxa_num"), "loading_taxa_num", value = 10, min = 1, max = 50)),
            column(2, shiny::selectInput(ns("loading_text_taxlevel"), "loading_text_taxlevel", choices = character(0))),
            column(2, shiny::numericInput(ns("loading_text_size"), "loading_text_size", value = 3, min = 1, max = 8)),
            column(2, shiny::textInput(ns("loading_text_color"), "loading_text_color", value = "black")),
            column(2, shiny::textInput(ns("loading_arrow_color"), "loading_arrow_color", value = "grey30")),
            column(2, shinyWidgets::materialSwitch(ns("loading_text_italic"), tr("标签斜体 (loading_text_italic)", "italic"), value = FALSE, status = "info"))
          ),
          fluidRow(
            column(2, shinyWidgets::materialSwitch(ns("loading_text_prefix"), tr("显示前缀 (loading_text_prefix)", "show_prefix"), value = FALSE, status = "info")),
            column(10)
          ),
          hr(),
          h4(tr("统计检验 (cal_manova/cal_anosim)", "Statistical Tests")),
          fluidRow(
            column(2, shinyWidgets::materialSwitch(ns("manova_all"), tr("manova_all (全局)", "manova_all (global)"), value = TRUE, status = "primary")),
            column(2, shiny::selectInput(ns("p_adjust_method"), "p_adjust_method",
              choices = c("fdr", "holm", "bonferroni", "none", "BH", "BY"), selected = "fdr")),
            column(2, shiny::textInput(ns("manova_formula"), tr("manova_set (公式)", "formula"),
              value = "")),
            column(2, shiny::selectInput(ns("by_group_col"), tr("by_group (配对)", "by_group (paired)"),
              choices = character(0))),
            column(4)
          ),
          fluidRow(
            column(2, shinyWidgets::materialSwitch(ns("anosim_paired"), tr("anosim_paired (配对)", "anosim_paired (paired)"), value = FALSE, status = "info")),
            column(4)
          ),
          hr(),
          fluidRow(
            column(2, shiny::actionButton(ns("run_beta"), tr("\U0001f4ca 生成", "\U0001f4ca Generate"), icon = icon("play"), class = "btn-primary", width = "100%")),
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
          shinycssloaders::withSpinner(shiny::plotOutput(ns("ord_plot"), height = "500px"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 统计检验结果", "\U0001f4ca Statistical Test Results"), status = "info", solidHeader = TRUE, width = NULL,
          fluidRow(
            column(4, shiny::actionButton(ns("run_manova"), "perMANOVA", icon = icon("chart-bar"), class = "btn-warning", width = "100%")),
            column(4, shiny::actionButton(ns("run_anosim"), "ANOSIM", icon = icon("chart-area"), class = "btn-danger", width = "100%")),
            column(4, shiny::actionButton(ns("run_betadisper"), "BETADISPER", icon = icon("circle"), class = "btn-secondary", width = "100%"))
          ),
          hr(),
          DT::dataTableOutput(ns("beta_test_table"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 数据", "\U0001f4ca Data"), status = "secondary", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("ordination_table"))
        )
      )
    )
  )
}

mod_beta_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(plot = NULL, data_ordination = NULL, save_dir = NULL)

    volumes <- c(
      Home      = path.expand("~"),
      Desktop   = file.path(path.expand("~"), "Desktop"),
      Documents = file.path(path.expand("~"), "Documents")
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
      default_name <- paste0("beta_", input$ord_method, ".", input$image_format)
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
        fname <- paste0("beta_", input$ord_method)
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
        updateSelectInput(session, "taxa_level", choices = character(0))
        updateSelectInput(session, "plot_color", choices = character(0))
        updateSelectInput(session, "plot_shape", choices = character(0))
        updateSelectInput(session, "add_sample_label", choices = character(0))
        updateSelectInput(session, "loading_text_taxlevel", choices = character(0))
        updateSelectInput(session, "by_group_col", choices = character(0))
        updateSelectInput(session, "order_group_col", choices = character(0))
        return()
      }
      cols <- get_sample_cols(rv)
      ranks <- get_tax_ranks(rv)
      updateSelectInput(session, "group_col", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "taxa_level", choices = c("\u65e0" = "", ranks))
      updateSelectInput(session, "plot_color", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "plot_shape", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "add_sample_label", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "loading_text_taxlevel", choices = c("\u65e0" = "", ranks))
      updateSelectInput(session, "by_group_col", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "order_group_col", choices = c("\u65e0" = "", cols))
      
      # Initialize the group order reactive value
      if (!is.null(cols) && length(cols) > 0) {
        local_rv$group_order <- NULL
      }
    })

    output$order_group_ui <- renderUI({
      if (!isTRUE(input$order_group) || !isTRUE(nzchar(input$order_group_col))) {
        return(NULL)
      }
      
      col_name <- input$order_group_col
      st <- rv$microtable$sample_table
      if (is.null(st) || !(col_name %in% names(st))) return(NULL)
      
      vals <- as.character(unique(st[[col_name]]))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) return(NULL)
      
      shiny::fluidRow(
        shiny::column(12,
          shiny::tags$label("Drag to reorder group levels:", class = "control-label"),
          shinyjqui::jqui_sortable(
            shiny::tags$div(
              id = ns("group_order_items"),
              lapply(vals, function(v) {
                shiny::tags$div(
                  class = "group-order-item",
                  style = "background: #4e73df; color: white; padding: 5px 10px; margin: 3px; border-radius: 4px; cursor: move; display: inline-block;",
                  `data-value` = v,
                  v
                )
              })
            ),
            options = list(update = shiny::JS(sprintf("function(event, ui) {
              var order = [];
              $('#%s .group-order-item').each(function() {
                order.push($(this).attr('data-value'));
              });
              Shiny.setInputValue('%s', order);
            }", ns("group_order_items"), ns("group_order"))))
          )
        )
      )
    })

    get_color_palette <- function(name, n = 8) {
      palettes <- list(
        "Dark2"    = RColorBrewer::brewer.pal(min(n, 8), "Dark2"),
        "Set1"     = RColorBrewer::brewer.pal(min(n, 8), "Set1"),
        "Set2"     = RColorBrewer::brewer.pal(min(n, 8), "Set2"),
        "Set3"     = RColorBrewer::brewer.pal(min(n, 12), "Set3"),
        "Paired"   = RColorBrewer::brewer.pal(min(n, 12), "Paired"),
        "Spectral" = RColorBrewer::brewer.pal(min(n, 11), "Spectral"),
        "Viridis"  = viridisLite::viridis(n)
      )
      if (name %in% names(palettes)) {
        return(palettes[[name]])
      }
      RColorBrewer::brewer.pal(min(n, 8), "Dark2")
    }

    get_group_order <- function() {
      if (!isTRUE(input$order_group) || !isTRUE(nzchar(input$order_group_col))) return(NULL)
      
      order_from_ui <- input$group_order
      if (!is.null(order_from_ui) && length(order_from_ui) > 0) {
        return(order_from_ui)
      }
      
      col_name <- input$order_group_col
      st <- rv$microtable$sample_table
      if (is.null(st) || !(col_name %in% names(st))) return(NULL)
      vals <- as.character(unique(st[[col_name]]))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) return(NULL)
      vals
    }

    get_plot_type <- function() {
      types <- character(0)
      if (isTRUE(input$add_point)) types <- c(types, "point")
      if (isTRUE(input$add_ellipse)) types <- c(types, "ellipse")
      if (isTRUE(input$add_chull)) types <- c(types, "chull")
      if (isTRUE(input$add_centroid)) types <- c(types, "centroid")
      if (length(types) == 0) types <- "point"
      types
    }

    observeEvent(input$run_beta, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      group_val <- if (isTRUE(nzchar(input$group_col))) input$group_col else NULL
      taxa_level_val <- if (isTRUE(nzchar(input$taxa_level))) input$taxa_level else NULL

      if (is.null(rv$microtable$beta_diversity)) {
        showNotification("正在计算 Beta 多样性...", type = "message", duration = 3)
        rv$microtable$cal_betadiv()
      }

      result <- tryCatch({
        t_beta <- microeco::trans_beta$new(
          dataset = rv$microtable,
          group = group_val,
          measure = input$beta_measure
        )

        t_beta$cal_ordination(
          method = input$ord_method,
          ncomp = input$ncomp,
          taxa_level = taxa_level_val,
          NMDS_matrix = input$NMDS_matrix,
          trans = input$trans,
          scale_species = input$scale_species,
          scale_species_ratio = input$scale_species_ratio,
          orthoI = NA
        )

        dataset_name <- rv$microtable_name %||% "tmp_microtable"
        ta_code <- paste0(
          "# Beta \u591a\u6837\u6027\u5206\u6790\n",
          "t_beta <- microeco::trans_beta$new(\n",
          "  dataset = ", dataset_name, ",\n",
          "  measure = \"", input$beta_measure, "\",\n",
          if (!is.null(group_val)) paste0("  group = \"", group_val, "\",\n") else "  group = NULL,\n",
          ")\n\n",
          "t_beta$cal_ordination(\n",
          "  method = \"", input$ord_method, "\",\n",
          "  ncomp = ", input$ncomp, ",\n",
          if (!is.null(taxa_level_val)) paste0("  taxa_level = \"", taxa_level_val, "\",\n") else "  taxa_level = NULL,\n",
          "  NMDS_matrix = ", input$NMDS_matrix, ",\n",
          "  trans = ", input$trans, ",\n",
          "  scale_species = ", input$scale_species, ",\n",
          "  scale_species_ratio = ", input$scale_species_ratio, "\n",
          ")\n"
        )

        plot_code <- paste0(
          "t_beta$plot_ordination(\n",
          "  plot_type = c(\"", paste(get_plot_type(), collapse = "\", \""), "\"),\n",
          "  color_values = ", "RColorBrewer::brewer.pal(8, \"", input$color_theme, "\"),\n",
          "  plot_color = ", if (isTRUE(nzchar(input$plot_color))) paste0("\"", input$plot_color, "\"") else "NULL", ",\n",
          "  plot_shape = ", if (isTRUE(nzchar(input$plot_shape))) paste0("\"", input$plot_shape, "\"") else "NULL", ",\n",
          "  choices = c(", input$choices_x, ", ", input$choices_y, "),\n",
          "  point_size = ", input$point_size, ",\n",
          "  point_alpha = ", input$point_alpha, ",\n",
          "  add_sample_label = ", if (isTRUE(nzchar(input$add_sample_label))) paste0("\"", input$add_sample_label, "\"") else "NULL", ",\n",
          "  point_second = ", input$point_second, ",\n",
          "  point_second_size = ", input$point_second_size, ",\n",
          "  point_second_alpha = ", input$point_second_alpha, ",\n",
          "  point_second_color = ", if (isTRUE(nzchar(input$point_second_color))) paste0("\"", input$point_second_color, "\"") else "NULL", ",\n",
          "  ellipse_chull_fill = ", input$ellipse_chull_fill, ",\n",
          "  ellipse_chull_alpha = ", input$ellipse_chull_alpha, ",\n",
          "  ellipse_level = ", input$ellipse_level, ",\n",
          "  ellipse_type = \"", input$ellipse_type, "\",\n",
          "  centroid_segment_alpha = ", input$centroid_segment_alpha, ",\n",
          "  centroid_segment_size = ", input$centroid_segment_size, ",\n",
          "  centroid_segment_linetype = ", input$centroid_segment_linetype, ",\n",
          if (input$ord_method == "NMDS") {
            paste0(
              "  NMDS_stress_pos = ", input$NMDS_stress_pos, ",\n",
              "  NMDS_stress_text_prefix = \"", input$NMDS_stress_text_prefix, "\",\n"
            )
          } else "",
          "  loading_arrow = ", input$loading_arrow, ",\n",
          "  loading_taxa_num = ", input$loading_taxa_num, ",\n",
          "  loading_text_taxlevel = ", if (isTRUE(nzchar(input$loading_text_taxlevel))) paste0("\"", input$loading_text_taxlevel, "\"") else "NULL", ",\n",
          "  loading_text_size = ", input$loading_text_size, ",\n",
          "  loading_text_color = \"", input$loading_text_color, "\",\n",
          "  loading_arrow_color = \"", input$loading_arrow_color, "\",\n",
          "  loading_text_italic = ", input$loading_text_italic, ",\n",
          "  loading_text_prefix = ", input$loading_text_prefix, "\n",
          ")\n"
        )

        full_code <- paste0(ta_code, "# \u7ed8\u56fe\n", plot_code)

        p <- t_beta$plot_ordination(
          plot_type = get_plot_type(),
          color_values = get_color_palette(input$color_theme, n = if (isTRUE(nzchar(input$plot_color))) length(unique(rv$microtable$sample_table[[input$plot_color]])) else 8),
          shape_values = if (isTRUE(nzchar(input$shape_values))) tryCatch(eval(parse(text = input$shape_values)), error = function(e) NULL) else NULL,
          plot_color = if (isTRUE(nzchar(input$plot_color))) input$plot_color else NULL,
          plot_shape = if (isTRUE(nzchar(input$plot_shape))) input$plot_shape else NULL,
          plot_group_order = get_group_order(),
          choices = c(input$choices_x, input$choices_y),
          point_size = input$point_size,
          point_alpha = input$point_alpha,
          add_sample_label = if (isTRUE(nzchar(input$add_sample_label))) input$add_sample_label else NULL,
          point_second = input$point_second,
          point_second_size = input$point_second_size,
          point_second_alpha = input$point_second_alpha,
          point_second_color = if (isTRUE(nzchar(input$point_second_color))) input$point_second_color else NULL,
          ellipse_chull_fill = input$ellipse_chull_fill,
          ellipse_chull_alpha = input$ellipse_chull_alpha,
          ellipse_level = input$ellipse_level,
          ellipse_type = input$ellipse_type,
          centroid_segment_alpha = input$centroid_segment_alpha,
          centroid_segment_size = input$centroid_segment_size,
          centroid_segment_linetype = as.numeric(input$centroid_segment_linetype),
          NMDS_stress_pos = if (isTRUE(input$show_stress) && input$ord_method == "NMDS") {
            tryCatch(eval(parse(text = input$NMDS_stress_pos)), error = function(e) c(1, 1))
          } else NULL,
          NMDS_stress_text_prefix = if (input$ord_method == "NMDS") input$NMDS_stress_text_prefix else "",
          loading_arrow = input$loading_arrow,
          loading_taxa_num = input$loading_taxa_num,
          loading_text_taxlevel = if (isTRUE(nzchar(input$loading_text_taxlevel))) input$loading_text_taxlevel else NULL,
          loading_text_size = input$loading_text_size,
          loading_text_color = input$loading_text_color,
          loading_arrow_color = input$loading_arrow_color,
          loading_text_italic = input$loading_text_italic,
          loading_text_prefix = input$loading_text_prefix
        )

        list(success = TRUE, plot = p, t_beta = t_beta, code = full_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      append_code(rv, result$code, paste0("\u03b2\u591a\u6837\u6027 - ", input$beta_measure, " + ", input$ord_method))
      local_rv$plot <- result$plot
      local_rv$t_beta <- result$t_beta
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$ord_plot <- shiny::renderPlot({
      req(local_rv$plot)
      local_rv$plot
    })

    observeEvent(input$run_manova, {
      if (!check_microtable(rv) || is.null(local_rv$t_beta)) {
        showNotification("\u8bf7\u5148\u8ba1\u7b97\u03b2\u591a\u6837\u6027", type = "error")
        return()
      }

      result <- tryCatch({
        if (isTRUE(nzchar(input$manova_formula))) {
          local_rv$t_beta$cal_manova(
            manova_all = input$manova_all,
            manova_set = input$manova_formula,
            p_adjust_method = input$p_adjust_method,
            by = "terms",
            by_auto_set = TRUE,
            permutations = input$permutations
          )
        } else {
          local_rv$t_beta$cal_manova(
            manova_all = input$manova_all,
            group = if (isTRUE(nzchar(input$group_col))) input$group_col else NULL,
            by_group = if (isTRUE(nzchar(input$by_group_col))) input$by_group_col else NULL,
            p_adjust_method = input$p_adjust_method,
            by = "terms",
            by_auto_set = TRUE,
            permutations = input$permutations
          )
        }
        TRUE
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (is.list(result) && !isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
      } else {
        showNotification("\u2705 perMANOVA \u5b8c\u6210", type = "message")
      }
    })

    observeEvent(input$run_anosim, {
      if (!check_microtable(rv) || is.null(local_rv$t_beta)) {
        showNotification("\u8bf7\u5148\u8ba1\u7b97\u03b2\u591a\u6837\u6027", type = "error")
        return()
      }

      result <- tryCatch({
        local_rv$t_beta$cal_anosim(
          paired = input$anosim_paired,
          group = if (isTRUE(nzchar(input$group_col))) input$group_col else NULL,
          by_group = if (isTRUE(nzchar(input$by_group_col))) input$by_group_col else NULL,
          p_adjust_method = input$p_adjust_method,
          permutations = input$permutations
        )
        TRUE
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (is.list(result) && !isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
      } else {
        showNotification("\u2705 ANOSIM \u5b8c\u6210", type = "message")
      }
    })

    observeEvent(input$run_betadisper, {
      if (!check_microtable(rv) || is.null(local_rv$t_beta)) {
        showNotification("\u8bf7\u5148\u8ba1\u7b97\u03b2\u591a\u6837\u6027", type = "error")
        return()
      }

      result <- tryCatch({
        local_rv$t_beta$cal_betadisper()
        TRUE
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (is.list(result) && !isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
      } else {
        showNotification("\u2705 BETADISPER \u5b8c\u6210", type = "message")
      }
    })

    output$beta_test_table <- DT::renderDataTable({
      req(local_rv$t_beta)
      rows <- list()
      if (!is.null(local_rv$t_beta$res_manova)) {
        df_manova <- local_rv$t_beta$res_manova
        if (is.data.frame(df_manova)) {
          rows <- c(rows, list(perMANOVA = df_manova))
        }
      }
      if (!is.null(local_rv$t_beta$res_anosim)) {
        df_anosim <- local_rv$t_beta$res_anosim
        if (is.data.frame(df_anosim)) {
          rows <- c(rows, list(ANOSIM = df_anosim))
        }
      }
      if (length(rows) == 0) {
        return(DT::datatable(data.frame(Message = "\u6682\u65e0\u7edf\u8ba1\u68c0\u9a8c\u7ed3\u679c\uff0c\u8bf7\u70b9\u51fb\u4e0a\u65b9\u6309\u94ae\u8fdb\u884c\u68c0\u9a8c"),
          options = list(dom = "t"), rownames = FALSE))
      }
      df <- dplyr::bind_rows(rows, .id = "Test")
      DT::datatable(df, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
    })

    output$ordination_table <- DT::renderDataTable({
      req(local_rv$t_beta)
      if (is.null(local_rv$t_beta$res_ordination)) return(NULL)
      dt <- local_rv$t_beta$res_ordination$scores
      if (is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
      }
    })

    output$download_table <- downloadHandler(
      filename = function() paste0("beta_ordination", ifelse(input$table_format == ",", ".csv", ".tsv")),
      content = function(file) {
        req(local_rv$t_beta)
        dt <- local_rv$t_beta$res_ordination$scores
        if (!is.null(dt) && is.data.frame(dt)) {
          write.table(dt, file, sep = input$table_format, row.names = FALSE, quote = TRUE)
        }
      }
    )
  })
}