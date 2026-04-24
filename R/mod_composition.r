#' @title Composition Visualization Module UI
#' @description
#' Provides interface for visualizing taxonomic composition at different levels.
#' Supports bar plots, heatmaps, and pie charts with complete parameter options.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @keywords composition visualization microbiome
#' @family community-analysis
mod_composition_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(
      column(12, h2(tr("\U0001f4ca 组成可视化 Composition", "\U0001f4ca Composition")))
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
            column(2, shiny::selectInput(ns("taxrank"), tr("分类水平 (taxrank)", "taxrank"), choices = app_config$taxranks, selected = "Phylum")),
            column(2, shinyWidgets::materialSwitch(ns("limit_ntaxa"), tr("限制 Taxa 数量", "Limit Taxa number"), value = TRUE, status = "primary")),
            column(2, shiny::uiOutput(ns("ntaxa_ui"))),
            column(2, shiny::numericInput(ns("show"), tr("相对丰度阈值 (show)", "RA threshold (show)"), value = 0, min = 0, max = 1, step = 0.0001)),
            column(2, shiny::selectInput(ns("group_col"), tr("按组求均 (groupmean)", "groupmean"), choices = character(0))),
            column(2, shinyWidgets::materialSwitch(ns("use_percentage"), tr("相对 (use_percentage)", "relative (use_percentage)"), value = TRUE, status = "primary"))
          ),
          fluidRow(
            column(4, shinyWidgets::materialSwitch(ns("delete_taxonomy_lineage"), tr("mod.composition.delete_taxonomy_lineage", lang), value = TRUE, status = "success")),
            column(4, shinyWidgets::materialSwitch(ns("delete_taxonomy_prefix"), tr("mod.composition.delete_taxonomy_prefix", lang), value = TRUE, status = "success")),
            column(4, shiny::textInput(ns("prefix"), tr("mod.composition.prefix", lang), value = ""))
          ),
          fluidRow(
            column(4, shiny::selectInput(ns("high_level"), tr("mod.composition.high_level", lang), choices = character(0))),
            column(4, shiny::numericInput(ns("high_level_fix_nsub"), tr("mod.composition.high_level_fix_nsub", lang), value = NA, min = 1, max = 20)),
            column(4, shinyWidgets::materialSwitch(ns("group_morestats"), tr("mod.composition.group_morestats", lang), value = FALSE, status = "info"))
          ),
          hr(),
          fluidRow(
            column(12, shiny::radioButtons(ns("plot_type"), tr("图型", "Plot type"),
              choices = setNames(c("bar", "heatmap", "pie", "box", "line", "donut"),
                                 c(tr("柱状图", "Bar"), tr("热图", "Heatmap"), tr("饼图", "Pie"), tr("箱形图", "Box"), tr("线图", "Line"), tr("甜甜圈图", "Donut"))),
              selected = "bar", inline = TRUE))
          ),
          hr(),
          shiny::conditionalPanel(condition = "input.plot_type == 'bar'", ns = ns,
            h4(tr("柱状图参数", "Bar Plot Parameters")),
            fluidRow(
              column(2, shinyWidgets::materialSwitch(ns("bar_full"), tr("完整柱状 (bar_full)", "bar_full"), value = TRUE, status = "info")),
              column(2, shiny::selectInput(ns("color_theme"), tr("颜色 (color_theme)", "color_theme"),
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
              column(2, shiny::textInput(ns("others_color"), tr("其他色 (others_color)", "others_color"), value = "grey90")),
              column(2, shiny::numericInput(ns("barwidth"), tr("柱宽 (barwidth)", "barwidth"), value = 0.9, min = 0.5, max = 1, step = 0.1)),
              column(2, shinyWidgets::materialSwitch(ns("use_alluvium"), tr("alluvium图 (use_alluvium)", "use_alluvium"), value = FALSE, status = "warning")),
              column(2, shinyWidgets::materialSwitch(ns("coord_flip"), tr("翻转 (coord_flip)", "coord_flip"), value = FALSE, status = "success"))
            ),
            fluidRow(
              column(2, shiny::actionButton(ns("order_x_btn"), tr("▬ X排序", "▬ X Order"), class = "btn-outline-secondary", width = "100%")),
              column(2, shiny::actionButton(ns("facet_btn"), tr("▬ 分面 Facet", "▬ Facet"), class = "btn-outline-secondary", width = "100%")),
              column(8, uiOutput(ns("facet_display")))
            ),
            fluidRow(
              column(2, shinyWidgets::materialSwitch(ns("clustering"), tr("增加聚类 (clustering)", "clustering"), value = FALSE, status = "warning")),
              column(2, shinyWidgets::materialSwitch(ns("clustering_plot"), tr("增加聚图 (clustering_plot)", "clustering_plot"), value = FALSE, status = "warning")),
              column(2, shiny::numericInput(ns("cluster_plot_width"), tr("聚图宽 (cluster_plot_width)", "cluster_plot_width"), value = 0.2, min = 0.1, max = 0.5, step = 0.05)),
              column(2, shiny::textInput(ns("facet_color"), tr("分面色 (facet_color)", "facet_color"), value = "grey95")),
              column(2, shiny::numericInput(ns("strip_text"), tr("分面字大 (strip_text)", "strip_text"), value = 11, min = 8, max = 16)),
              column(2, shinyWidgets::materialSwitch(ns("xtext_keep"), tr("X字 (xtext_keep)", "xtext_keep"), value = TRUE, status = "info"))
            ),
            fluidRow(
              column(2, shinyWidgets::materialSwitch(ns("xtitle_keep"), tr("X标题 (xtitle_keep)", "xtitle_keep"), value = TRUE, status = "info")),
              column(2, shiny::numericInput(ns("xtext_angle"), tr("X字角度 (xtext_angle)", "xtext_angle"), value = 0, min = 0, max = 90, step = 15)),
              column(2, shiny::numericInput(ns("xtext_size"), tr("X字大小 (xtext_size)", "xtext_size"), value = 10, min = 8, max = 16)),
              column(2, shiny::numericInput(ns("ytitle_size"), tr("Y标题大小 (ytitle_size)", "ytitle_size"), value = 17, min = 10, max = 24)),
              column(2, shinyWidgets::materialSwitch(ns("legend_text_italic"), tr("图例斜体 (legend_text_italic)", "legend_text_italic"), value = FALSE, status = "warning")),
              column(2, shinyWidgets::materialSwitch(ns("ggnested"), tr("ggnested嵌套 (ggnested)", "ggnested"), value = FALSE, status = "danger"))
            ),
            fluidRow(
              column(2, shinyWidgets::materialSwitch(ns("high_level_add_other"), tr("高层增加Others (high_level_add_other)", "high_level_add_other"), value = FALSE, status = "info"))
            )
          ),
          shiny::conditionalPanel(condition = "input.plot_type == 'heatmap'", ns = ns,
            h4(tr("热图参数", "Heatmap Parameters")),
            fluidRow(
              column(2, shinyWidgets::materialSwitch(ns("withmargin"), tr("边框 (withmargin)", "withmargin"), value = TRUE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("plot_numbers"), tr("显示数值 (plot_numbers)", "plot_numbers"), value = FALSE, status = "warning")),
              column(2, shiny::numericInput(ns("plot_text_size"), tr("数值字大 (plot_text_size)", "plot_text_size"), value = 4, min = 2, max = 10)),
              column(2, shiny::selectInput(ns("plot_colorscale"), tr("色度 (plot_colorscale)", "plot_colorscale"),
                choices = c("log10", "identity", "sqrt"), selected = "log10")),
              column(2, shiny::numericInput(ns("min_abundance"), tr("最小丰度 (min_abundance)", "min_abundance"), value = 0.01, min = 0, max = 100, step = 0.01)),
              column(2, shiny::numericInput(ns("max_abundance_hm"), tr("最大丰度 (0=自动)", "max_abundance (0=auto)"), value = 0, min = 0, max = 100, step = 0.01))
            ),
            fluidRow(
              column(2, shiny::actionButton(ns("order_x_btn_hm"), tr("▬ X排序", "▬ X Order"), class = "btn-outline-secondary", width = "100%")),
              column(2, shiny::actionButton(ns("facet_btn_hm"), tr("▬ 分面 Facet", "▬ Facet"), class = "btn-outline-secondary", width = "100%")),
              column(5, uiOutput(ns("facet_display_hm"))),
              column(3, shiny::selectInput(ns("x_axis_name_hm"), tr("X轴名称 (x_axis_name)", "x_axis_name"), choices = character(0)))
            ),
            fluidRow(
              column(2, shiny::selectInput(ns("facet_switch"), tr("分面位 (facet_switch)", "facet_switch"),
                choices = c("y", "x", "both"), selected = "y")),
              column(2, shiny::textInput(ns("margincolor"), tr("边色 (margincolor)", "margincolor"), value = "white")),
              column(2, shiny::textInput(ns("legend_title"), tr("图例标题 (legend_title)", "legend_title"), value = "% Relative\nAbundance")),
              column(2, shiny::numericInput(ns("strip_text_hm"), tr("分面字大 (strip_text)", "strip_text"), value = 11, min = 8, max = 16)),
              column(2, shinyWidgets::materialSwitch(ns("grid_clean"), tr("清网 (grid_clean)", "grid_clean"), value = TRUE, status = "success")),
              column(2, shiny::textInput(ns("plot_breaks_hm"), tr("图例断点 (plot_breaks)", "plot_breaks"), value = "0.01, 0.1, 1, 10"))
            ),
            fluidRow(
              column(2, shinyWidgets::materialSwitch(ns("xtitle_keep_hm"), tr("X标题 (xtitle_keep)", "xtitle_keep"), value = TRUE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("xtext_keep_hm"), tr("X字 (xtext_keep)", "xtext_keep"), value = TRUE, status = "info")),
              column(2, shiny::numericInput(ns("xtext_angle_hm"), tr("X字角度 (xtext_angle)", "xtext_angle"), value = 0, min = 0, max = 90, step = 15)),
              column(2, shiny::numericInput(ns("xtext_size_hm"), tr("X字大小 (xtext_size)", "xtext_size"), value = 10, min = 8, max = 16)),
              column(2, shiny::numericInput(ns("ytext_size_hm"), tr("Y字大小 (ytext_size)", "ytext_size"), value = 11, min = 8, max = 16)),
              column(2, shinyWidgets::materialSwitch(ns("pheatmap"), tr("pheatmap包 (pheatmap)", "pheatmap"), value = FALSE, status = "danger"))
            )
          ),
          shiny::conditionalPanel(condition = "input.plot_type == 'pie'", ns = ns,
            h4(tr("饼图参数", "Pie Chart Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("color_theme_pie"), tr("颜色 (color_theme)", "color_theme"),
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral"), selected = "Dark2")),
              column(2, shiny::numericInput(ns("facet_nrow_pie"), tr("行数 (facet_nrow)", "facet_nrow"), value = 1, min = 1, max = 10)),
              column(2, shiny::numericInput(ns("strip_text_pie"), tr("分面字大 (strip_text)", "strip_text"), value = 11, min = 8, max = 16)),
              column(2, shinyWidgets::materialSwitch(ns("add_label_pie"), tr("百分比标签 (add_label)", "add_label"), value = FALSE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("legend_text_italic_pie"), tr("图例斜体 (legend_text_italic)", "legend_text_italic"), value = FALSE, status = "warning"))
            )
          ),
          shiny::conditionalPanel(condition = "input.plot_type == 'box'", ns = ns,
            h4(tr("箱形图参数", "Box Plot Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("color_theme_box"), tr("颜色 (color_theme)", "color_theme"),
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
              column(2, shiny::selectInput(ns("group_col_box"), tr("分组 (group)", "group"),
                choices = character(0), selected = "")),
              column(2, shinyWidgets::materialSwitch(ns("show_point_box"), tr("显示点 (show_point)", "show_point"), value = FALSE, status = "info")),
              column(2, shiny::textInput(ns("point_color_box"), tr("点颜色 (point_color)", "point_color"), value = "black")),
              column(2, shiny::numericInput(ns("point_size_box"), tr("点大小 (point_size)", "point_size"), value = 3, min = 1, max = 10)),
              column(2, shiny::numericInput(ns("point_alpha_box"), tr("点透明度 (point_alpha)", "point_alpha"), value = 0.3, min = 0, max = 1, step = 0.1))
            ),
            fluidRow(
              column(2, shinyWidgets::materialSwitch(ns("plot_flip_box"), tr("翻转 (plot_flip)", "plot_flip"), value = FALSE, status = "success")),
              column(2, shinyWidgets::materialSwitch(ns("boxfill_box"), tr("填充 (boxfill)", "boxfill"), value = TRUE, status = "info")),
              column(2, shiny::textInput(ns("middlecolor_box"), tr("中线颜色 (middlecolor)", "middlecolor"), value = "grey95")),
              column(2, shiny::numericInput(ns("middlesize_box"), tr("中线大小 (middlesize)", "middlesize"), value = 1, min = 0.5, max = 3)),
              column(2, shiny::numericInput(ns("xtext_angle_box"), tr("X字角度 (xtext_angle)", "xtext_angle"), value = 0, min = 0, max = 90, step = 15)),
              column(2, shiny::numericInput(ns("xtext_size_box"), tr("X字大小 (xtext_size)", "xtext_size"), value = 10, min = 8, max = 16))
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("ytitle_size_box"), tr("Y标题大小 (ytitle_size)", "ytitle_size"), value = 17, min = 10, max = 24))
            )
          ),
          shiny::conditionalPanel(condition = "input.plot_type == 'line'", ns = ns,
            h4(tr("线图参数", "Line Plot Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("color_theme_line"), tr("颜色 (color_theme)", "color_theme"),
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
              column(2, shinyWidgets::materialSwitch(ns("plot_SE_line"), tr("显示标准误 (plot_SE)", "plot_SE"), value = TRUE, status = "info")),
              column(2, shiny::numericInput(ns("errorbar_size_line"), tr("误差线大小 (errorbar_size)", "errorbar_size"), value = 1, min = 0.5, max = 3)),
              column(2, shiny::numericInput(ns("errorbar_width_line"), tr("误差线宽度 (errorbar_width)", "errorbar_width"), value = 0.1, min = 0.05, max = 0.5)),
              column(2, shiny::numericInput(ns("point_size_line"), tr("点大小 (point_size)", "point_size"), value = 3, min = 1, max = 10)),
              column(2, shiny::numericInput(ns("point_alpha_line"), tr("点透明度 (point_alpha)", "point_alpha"), value = 0.8, min = 0, max = 1, step = 0.1))
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("line_size_line"), tr("线大小 (line_size)", "line_size"), value = 0.8, min = 0.5, max = 3)),
              column(2, shiny::numericInput(ns("line_alpha_line"), tr("线透明度 (line_alpha)", "line_alpha"), value = 0.8, min = 0, max = 1, step = 0.1)),
              column(2, shiny::numericInput(ns("line_type_line"), tr("线类型 (line_type)", "line_type"), value = 1, min = 1, max = 6)),
              column(2, shiny::numericInput(ns("xtext_angle_line"), tr("X字角度 (xtext_angle)", "xtext_angle"), value = 0, min = 0, max = 90, step = 15)),
              column(2, shiny::numericInput(ns("xtext_size_line"), tr("X字大小 (xtext_size)", "xtext_size"), value = 10, min = 8, max = 16)),
              column(2, shiny::numericInput(ns("ytitle_size_line"), tr("Y标题大小 (ytitle_size)", "ytitle_size"), value = 17, min = 10, max = 24))
            )
          ),
          shiny::conditionalPanel(condition = "input.plot_type == 'donut'", ns = ns,
            h4(tr("甜甜圈图参数", "Donut Chart Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("color_theme_donut"), tr("颜色 (color_theme)", "color_theme"),
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
              column(2, shinyWidgets::materialSwitch(ns("label_donut"), tr("显示标签 (label)", "label"), value = TRUE, status = "info")),
              column(2, shiny::numericInput(ns("facet_nrow_donut"), tr("行数 (facet_nrow)", "facet_nrow"), value = 1, min = 1, max = 10)),
              column(2, shinyWidgets::materialSwitch(ns("legend_text_italic_donut"), tr("图例斜体 (legend_text_italic)", "legend_text_italic"), value = FALSE, status = "warning"))
            )
          ),
          hr(),
          fluidRow(
            column(2, shiny::actionButton(ns("run_composition"), tr("\U0001f4ca 生成", "\U0001f4ca Generate"), icon = icon("play"), class = "btn-primary", width = "100%")),
            column(2, shiny::selectInput(ns("image_format"), tr("格式", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("save_width"), tr("宽 (width)", "Width"), value = 10, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("save_height"), tr("高 (height)", "Height"), value = 7, min = 3, max = 15)),
            column(2, shiny::numericInput(ns("save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(2, shiny::downloadButton(ns("save_plot"), tr("\U0001f4e5保存图片", "\U0001f4e5Save Plot"), icon = icon("download"), class = "btn-outline-secondary", width = "100%"))
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
          shinycssloaders::withSpinner(shiny::plotOutput(ns("composition_plot"), height = "500px"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 数据", "\U0001f4ca Data"), status = "secondary", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("abund_table"))
        )
      )
    ),
    shiny::uiOutput(ns("order_x_modal"))
  )
}

mod_composition_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(plot = NULL, data_abund = NULL, sample_order = NULL, sample_order_hm = NULL, redraw_trigger = 0, facet_cols = NULL, facet_cols_hm = NULL)

    observe({
      if (!check_microtable(rv)) {
        updateSelectInput(session, "group_col", choices = character(0))
        updateSelectInput(session, "high_level", choices = character(0))
        updateSelectInput(session, "group_col_box", choices = character(0))
        return()
      }
      cols <- c("\u65e0" = "", get_sample_cols(rv))
      updateSelectInput(session, "group_col", choices = cols)
      updateSelectInput(session, "group_col_box", choices = cols)
      ranks <- get_tax_ranks(rv)
      if (length(ranks) > 0) {
        updateSelectInput(session, "taxrank", choices = ranks)
        updateSelectInput(session, "high_level", choices = c("\u65e0" = "", ranks))
      }
    })

    output$ntaxa_ui <- renderUI({
      if (isTRUE(input$limit_ntaxa)) {
        shiny::numericInput(ns("ntaxa"), "Top N (ntaxa)", value = 10, min = 3, max = 50)
      } else {
        NULL
      }
    })

    output$order_x_modal <- renderUI(NULL)

    observeEvent(input$order_x_btn, {
      if (!check_microtable(rv)) {
        showNotification("请先导入数据", type = "warning")
        return()
      }

      samples <- rownames(rv$microtable$sample_table)
      current_order <- local_rv$sample_order

      if (is.null(current_order)) {
        current_order <- samples
      }

      modal_content <- tags$div(
        class = "sample-sort-modal-content",
        tags$p(class = "text-muted small", "拖拽调整样本排列顺序"),
        shinyjqui::orderInput(
          inputId = ns("sample_order"),
          label = NULL,
          items = current_order,
          width = "100%"
        )
      )

      showModal(modalDialog(
        title = tagList(icon("sort-alpha-down"), " X排序 (拖拽排序)"),
        modal_content,
        easyClose = FALSE,
        size = "m",
        footer = tagList(
          actionButton(ns("clear_order"), "清空", class = "btn-warning"),
          actionButton(ns("apply_order"), "应用", class = "btn-primary"),
          modalButton("关闭")
        )
      ))
    })

    observeEvent(input$sample_order, {
      local_rv$sample_order <- input$sample_order
    })

    observeEvent(input$apply_order, {
      if (!is.null(local_rv$sample_order) && length(local_rv$sample_order) > 0) {
        showNotification(paste0("已应用排序: ", length(local_rv$sample_order), "个样本，正在重新绑图..."), type = "message", duration = 3)
        local_rv$redraw_trigger <- local_rv$redraw_trigger + 1
      }
      removeModal()
    })

    observeEvent(input$clear_order, {
      local_rv$sample_order <- NULL
      showNotification("已清空排序，点击应用后关闭", type = "message", duration = 2)
      removeModal()
    })

    observeEvent(input$facet_btn, {
      if (!check_microtable(rv)) {
        showNotification("请先导入数据", type = "warning")
        return()
      }
      cols <- get_sample_cols(rv)
      current_selection <- local_rv$facet_cols %||% character(0)
      available_cols <- setdiff(cols, current_selection)

      showModal(modalDialog(
        title = tagList(icon("th"), "选择并排序分面列"),
        size = "l",
        tags$p(class = "text-muted small", "拖拽调整顺序，先选的在最外层分面"),
        fluidRow(
          column(6,
            tags$label("可选分组列"),
            shinyjqui::orderInput(
              inputId = ns("facet_available"),
              label = NULL,
              items = available_cols,
              connect = c(ns("facet_selected")),
              width = "100%"
            )
          ),
          column(6,
            tags$label("已选分面列（拖拽排序）"),
            shinyjqui::orderInput(
              inputId = ns("facet_selected"),
              label = NULL,
              items = current_selection,
              connect = c(ns("facet_available")),
              width = "100%"
            )
          )
        ),
        footer = tagList(
          actionButton(ns("apply_facet"), "应用", class = "btn-primary"),
          modalButton("关闭")
        )
      ))
    })

    output$facet_display <- renderUI({
      lang <- rv$current_language
      if (is.null(local_rv$facet_cols) || length(local_rv$facet_cols) == 0) {
        return(tags$span(class = "text-muted", tr("mod.composition.facet.not_selected", lang)))
      }
      tagList(lapply(seq_along(local_rv$facet_cols), function(i) {
        col <- local_rv$facet_cols[i]
        span(
          class = "badge bg-info me-2 mb-1",
          style = "cursor:pointer; font-size: 0.9rem;",
          title = paste0("顺序: ", i),
          paste0(i, ". ", col),
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                           ns("remove_facet"), col),
          " \u2716"
        )
      }))
    })

    observeEvent(input$apply_facet, {
      local_rv$facet_cols <- input$facet_selected
      removeModal()
    })

    observeEvent(input$remove_facet, {
      req(input$remove_facet)
      local_rv$facet_cols <- setdiff(local_rv$facet_cols, input$remove_facet)
    })

    observeEvent(input$facet_btn_hm, {
      if (!check_microtable(rv)) {
        showNotification("请先导入数据", type = "warning")
        return()
      }
      cols <- get_sample_cols(rv)
      current_selection <- local_rv$facet_cols_hm %||% character(0)
      available_cols <- setdiff(cols, current_selection)

      showModal(modalDialog(
        title = tagList(icon("th"), "选择并排序分面列(热图)"),
        size = "l",
        tags$p(class = "text-muted small", "拖拽调整顺序，先选的在最外层分面"),
        fluidRow(
          column(6,
            tags$label("可选分组列"),
            shinyjqui::orderInput(
              inputId = ns("facet_available_hm"),
              label = NULL,
              items = available_cols,
              connect = c(ns("facet_selected_hm")),
              width = "100%"
            )
          ),
          column(6,
            tags$label("已选分面列（拖拽排序）"),
            shinyjqui::orderInput(
              inputId = ns("facet_selected_hm"),
              label = NULL,
              items = current_selection,
              connect = c(ns("facet_available_hm")),
              width = "100%"
            )
          )
        ),
        footer = tagList(
          actionButton(ns("apply_facet_hm"), "应用", class = "btn-primary"),
          modalButton("关闭")
        )
      ))
    })

    observeEvent(input$order_x_btn_hm, {
      if (!check_microtable(rv)) {
        showNotification("请先导入数据", type = "warning")
        return()
      }
      samples <- rownames(rv$microtable$sample_table)
      current_order <- local_rv$sample_order_hm
      if (is.null(current_order)) {
        current_order <- samples
      }

      showModal(modalDialog(
        title = tagList(icon("sort-alpha-down"), " X排序 (拖拽排序)"),
        tags$p(class = "text-muted small", "拖拽调整样本排列顺序（热图）"),
        shinyjqui::orderInput(
          inputId = ns("sample_order_hm"),
          label = NULL,
          items = current_order,
          width = "100%"
        ),
        easyClose = FALSE,
        size = "m",
        footer = tagList(
          actionButton(ns("clear_order_hm"), "清空", class = "btn-warning"),
          actionButton(ns("apply_order_hm"), "应用", class = "btn-primary"),
          modalButton("关闭")
        )
      ))
    })

    observeEvent(input$sample_order_hm, {
      local_rv$sample_order_hm <- input$sample_order_hm
    })

    observeEvent(input$apply_order_hm, {
      if (!is.null(local_rv$sample_order_hm) && length(local_rv$sample_order_hm) > 0) {
        showNotification(paste0("已应用排序: ", length(local_rv$sample_order_hm), "个样本"), type = "message", duration = 3)
      }
      removeModal()
    })

    observeEvent(input$clear_order_hm, {
      local_rv$sample_order_hm <- NULL
      showNotification("已清空排序", type = "message", duration = 2)
      removeModal()
    })

    output$facet_display_hm <- renderUI({
      lang <- rv$current_language
      if (is.null(local_rv$facet_cols_hm) || length(local_rv$facet_cols_hm) == 0) {
        return(tags$span(class = "text-muted", tr("mod.composition.facet.not_selected", lang)))
      }
      tagList(lapply(seq_along(local_rv$facet_cols_hm), function(i) {
        col <- local_rv$facet_cols_hm[i]
        span(
          class = "badge bg-info me-2 mb-1",
          style = "cursor:pointer; font-size: 0.9rem;",
          title = paste0("顺序: ", i),
          paste0(i, ". ", col),
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                           ns("remove_facet_hm"), col),
          " \u2716"
        )
      }))
    })

    observeEvent(input$apply_facet_hm, {
      local_rv$facet_cols_hm <- input$facet_selected_hm
      removeModal()
    })

    observeEvent(input$remove_facet_hm, {
      req(input$remove_facet_hm)
      local_rv$facet_cols_hm <- setdiff(local_rv$facet_cols_hm, input$remove_facet_hm)
    })

    observe({
      if (!check_microtable(rv)) {
        updateSelectInput(session, "x_axis_name_hm", choices = character(0))
        return()
      }
      cols <- c("\u65e0" = "", get_sample_cols(rv))
      updateSelectInput(session, "x_axis_name_hm", choices = cols)
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
          RColorBrewer::brewer.pal(8, "Dark2")
        )
      }, error = function(e) RColorBrewer::brewer.pal(8, "Dark2"))
    }

    get_color_palette_code <- function(name) {
      switch(name,
        "Dark2"    = "RColorBrewer::brewer.pal(8, \"Dark2\")",
        "Set1"     = "RColorBrewer::brewer.pal(8, \"Set1\")",
        "Set2"     = "RColorBrewer::brewer.pal(8, \"Set2\")",
        "Set3"     = "RColorBrewer::brewer.pal(12, \"Set3\")",
        "Paired"   = "RColorBrewer::brewer.pal(12, \"Paired\")",
        "Spectral" = "RColorBrewer::brewer.pal(11, \"Spectral\")",
        "Viridis"  = "viridisLite::viridis(8)",
        "RColorBrewer::brewer.pal(8, \"Dark2\")"
      )
    }

    # Triggered by either the "生成" button or the redraw_trigger (X排序应用后自动重绘)
    observeEvent(c(input$run_composition, local_rv$redraw_trigger), {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      groupmean_val <- if (isTRUE(nzchar(input$group_col))) input$group_col else NULL
      prefix_val <- if (isTRUE(nzchar(input$prefix))) input$prefix else NULL
      high_level_val <- if (isTRUE(nzchar(input$high_level))) input$high_level else NULL
      order_x_val <- if (!is.null(local_rv$sample_order) && length(local_rv$sample_order) > 0) local_rv$sample_order else NULL
      facet_val <- if (!is.null(local_rv$facet_cols) && length(local_rv$facet_cols) > 0) local_rv$facet_cols else NULL

      high_level_fix_nsub_val <- {
        val <- input$high_level_fix_nsub
        if (is.null(val) || is.na(val)) NULL else val
      }

      dataset_name <- rv$microtable_name %||% "dataset"
      plot_type <- input$plot_type

      ta_code <- paste0(
        "# \u7ec4\u6210\u53ef\u89c6\u5316 - ", plot_type, "\n",
        "t_abund <- microeco::trans_abund$new(\n",
        "  dataset = ", dataset_name, ",\n",
        "  taxrank = \"", input$taxrank, "\",\n",
        "  show = ", input$show, ",\n",
        if (isTRUE(input$limit_ntaxa)) paste0("  ntaxa = ", input$ntaxa, ",\n") else "  ntaxa = NULL,\n",
        if (!is.null(groupmean_val)) paste0("  groupmean = \"", groupmean_val, "\",\n") else "  groupmean = NULL,\n",
        if (isTRUE(input$group_morestats) && !is.null(groupmean_val)) "  group_morestats = TRUE,\n",
        "  delete_taxonomy_lineage = ", input$delete_taxonomy_lineage, ",\n",
        "  delete_taxonomy_prefix = ", input$delete_taxonomy_prefix, ",\n",
        if (!is.null(prefix_val)) paste0("  prefix = \"", prefix_val, "\",\n") else "  prefix = NULL,\n",
        "  use_percentage = ", input$use_percentage, ",\n",
        if (!is.null(high_level_val)) paste0("  high_level = \"", high_level_val, "\",\n") else "  high_level = NULL,\n",
        if (!is.null(high_level_fix_nsub_val)) paste0("  high_level_fix_nsub = ", high_level_fix_nsub_val, ",\n"),
        ")\n"
      )

      plot_code <- if (plot_type == "bar") {
        palette_code <- get_color_palette_code(input$color_theme)
        paste0(
          "p <- t_abund$plot_bar(\n",
          "  color_values = ", palette_code, ",\n",
          "  bar_full = ", input$bar_full, ",\n",
          "  others_color = \"", input$others_color, "\",\n",
          if (!is.null(facet_val)) paste0("  facet = c(\"", paste0(facet_val, collapse = "\", \""), "\"),\n") else "  facet = NULL,\n",
          if (!is.null(order_x_val)) paste0("  order_x = c(\"", paste0(order_x_val, collapse = "\", \""), "\"),\n") else "  order_x = NULL,\n",
          "  barwidth = ", input$barwidth, ",\n",
          "  use_alluvium = ", input$use_alluvium, ",\n",
          "  clustering = ", input$clustering, ",\n",
          "  clustering_plot = ", input$clustering_plot, ",\n",
          "  cluster_plot_width = ", input$cluster_plot_width, ",\n",
          "  facet_color = \"", input$facet_color, "\",\n",
          "  strip_text = ", input$strip_text, ",\n",
          "  legend_text_italic = ", input$legend_text_italic, ",\n",
          "  xtext_angle = ", input$xtext_angle, ",\n",
          "  xtext_size = ", input$xtext_size, ",\n",
          "  xtext_keep = ", input$xtext_keep, ",\n",
          "  xtitle_keep = ", input$xtitle_keep, ",\n",
          "  ytitle_size = ", input$ytitle_size, ",\n",
          "  coord_flip = ", input$coord_flip, ",\n",
          "  ggnested = ", input$ggnested, ",\n",
          "  high_level_add_other = ", input$high_level_add_other, "\n",
          ")\n"
        )
      } else if (plot_type == "heatmap") {
        palette_code_hm <- "rev(RColorBrewer::brewer.pal(n = 11, name = \"RdYlBu\"))"
        facet_hm_val <- if (!is.null(local_rv$facet_cols_hm) && length(local_rv$facet_cols_hm) > 0) local_rv$facet_cols_hm else NULL
        order_x_hm_val <- if (!is.null(local_rv$sample_order_hm) && length(local_rv$sample_order_hm) > 0) local_rv$sample_order_hm else NULL
        x_axis_name_val <- if (isTRUE(nzchar(input$x_axis_name_hm))) input$x_axis_name_hm else NULL
        plot_breaks_val <- {
          if (isTRUE(nzchar(input$plot_breaks_hm))) {
            tryCatch({
              vals <- as.numeric(strsplit(input$plot_breaks_hm, ",\\s*")[[1]])
              if (length(vals) > 0 && !any(is.na(vals))) vals else NULL
            }, error = function(e) NULL)
          } else NULL
        }
        paste0(
          "p <- t_abund$plot_heatmap(\n",
          "  color_values = ", palette_code_hm, ",\n",
          "  facet = ", if (!is.null(facet_hm_val)) paste0("c(\"", paste0(facet_hm_val, collapse = "\", \""), "\")") else "NULL", ",\n",
          "  facet_switch = \"", input$facet_switch, "\",\n",
          "  x_axis_name = ", if (!is.null(x_axis_name_val)) paste0("\"", x_axis_name_val, "\"") else "NULL", ",\n",
          if (!is.null(order_x_hm_val)) paste0("  order_x = c(\"", paste0(order_x_hm_val, collapse = "\", \""), "\"),\n") else "  order_x = NULL,\n",
          "  withmargin = ", input$withmargin, ",\n",
          "  plot_numbers = ", input$plot_numbers, ",\n",
          "  plot_text_size = ", input$plot_text_size, ",\n",
          if (!is.null(plot_breaks_val)) paste0("  plot_breaks = c(", paste0(plot_breaks_val, collapse = ", "), "),\n") else "  plot_breaks = NULL,\n",
          "  plot_colorscale = \"", input$plot_colorscale, "\",\n",
          "  min_abundance = ", input$min_abundance, ",\n",
          "  max_abundance = ", if (!is.null(input$max_abundance_hm) && input$max_abundance_hm > 0) input$max_abundance_hm else "NULL", ",\n",
          "  strip_text = ", input$strip_text_hm, ",\n",
          "  xtext_keep = ", input$xtext_keep_hm, ",\n",
          "  xtext_angle = ", input$xtext_angle_hm, ",\n",
          "  xtext_size = ", input$xtext_size_hm, ",\n",
          "  ytext_size = ", input$ytext_size_hm, ",\n",
          "  xtitle_keep = ", input$xtitle_keep_hm, ",\n",
          "  grid_clean = ", input$grid_clean, ",\n",
          "  legend_title = \"", input$legend_title, "\",\n",
          "  margincolor = \"", input$margincolor, "\",\n",
          "  pheatmap = ", input$pheatmap, "\n",
          ")\n"
        )
      } else if (plot_type == "box") {
        palette_code_box <- get_color_palette_code(input$color_theme_box)
        group_box_val <- if (isTRUE(nzchar(input$group_col_box))) input$group_col_box else NULL
        paste0(
          "p <- t_abund$plot_box(\n",
          "  color_values = ", palette_code_box, ",\n",
          "  group = ", if (!is.null(group_box_val)) paste0("\"", group_box_val, "\"") else "NULL", ",\n",
          "  show_point = ", input$show_point_box, ",\n",
          "  point_color = \"", input$point_color_box, "\",\n",
          "  point_size = ", input$point_size_box, ",\n",
          "  point_alpha = ", input$point_alpha_box, ",\n",
          "  plot_flip = ", input$plot_flip_box, ",\n",
          "  boxfill = ", input$boxfill_box, ",\n",
          "  middlecolor = \"", input$middlecolor_box, "\",\n",
          "  middlesize = ", input$middlesize_box, ",\n",
          "  xtext_angle = ", input$xtext_angle_box, ",\n",
          "  xtext_size = ", input$xtext_size_box, ",\n",
          "  ytitle_size = ", input$ytitle_size_box, "\n",
          ")\n"
        )
      } else if (plot_type == "line") {
        palette_code_line <- get_color_palette_code(input$color_theme_line)
        paste0(
          "p <- t_abund$plot_line(\n",
          "  color_values = ", palette_code_line, ",\n",
          "  plot_SE = ", input$plot_SE_line, ",\n",
          "  errorbar_size = ", input$errorbar_size_line, ",\n",
          "  errorbar_width = ", input$errorbar_width_line, ",\n",
          "  point_size = ", input$point_size_line, ",\n",
          "  point_alpha = ", input$point_alpha_line, ",\n",
          "  line_size = ", input$line_size_line, ",\n",
          "  line_alpha = ", input$line_alpha_line, ",\n",
          "  line_type = ", input$line_type_line, ",\n",
          "  xtext_angle = ", input$xtext_angle_line, ",\n",
          "  xtext_size = ", input$xtext_size_line, ",\n",
          "  ytitle_size = ", input$ytitle_size_line, "\n",
          ")\n"
        )
      } else if (plot_type == "donut") {
        palette_code_donut <- get_color_palette_code(input$color_theme_donut)
        paste0(
          "p <- t_abund$plot_donut(\n",
          "  color_values = ", palette_code_donut, ",\n",
          "  label = ", input$label_donut, ",\n",
          "  facet_nrow = ", input$facet_nrow_donut, ",\n",
          "  legend_text_italic = ", input$legend_text_italic_donut, "\n",
          ")\n"
        )
      } else {
        palette_code <- get_color_palette_code(input$color_theme_pie)
        paste0(
          "p <- t_abund$plot_pie(\n",
          "  color_values = ", palette_code, ",\n",
          "  facet_nrow = ", input$facet_nrow_pie, ",\n",
          "  strip_text = ", input$strip_text_pie, ",\n",
          "  add_label = ", input$add_label_pie, ",\n",
          "  legend_text_italic = ", input$legend_text_italic_pie, "\n",
          ")\n"
        )
      }

      code <- paste0(ta_code, "# \u751f\u6210", plot_type, "\u56fe\n", plot_code)

      result <- tryCatch({
        mt <- rv$microtable

        ntaxa_val <- if (isTRUE(input$limit_ntaxa)) input$ntaxa else NULL

        t_abund <- microeco::trans_abund$new(
          dataset = mt,
          taxrank = input$taxrank,
          show = input$show,
          ntaxa = ntaxa_val,
          groupmean = groupmean_val,
          group_morestats = if (isTRUE(input$group_morestats) && !is.null(groupmean_val)) TRUE else FALSE,
          delete_taxonomy_lineage = input$delete_taxonomy_lineage,
          delete_taxonomy_prefix = input$delete_taxonomy_prefix,
          prefix = prefix_val,
          use_percentage = input$use_percentage,
          high_level = high_level_val,
          high_level_fix_nsub = high_level_fix_nsub_val
        )

        p <- if (plot_type == "bar") {
          t_abund$plot_bar(
            color_values = get_color_palette(input$color_theme),
            bar_full = input$bar_full,
            others_color = input$others_color,
            facet = facet_val,
            order_x = order_x_val,
            barwidth = input$barwidth,
            use_alluvium = input$use_alluvium,
            clustering = input$clustering,
            clustering_plot = input$clustering_plot,
            cluster_plot_width = input$cluster_plot_width,
            facet_color = input$facet_color,
            strip_text = input$strip_text,
            legend_text_italic = input$legend_text_italic,
            xtext_angle = input$xtext_angle,
            xtext_size = input$xtext_size,
            xtext_keep = input$xtext_keep,
            xtitle_keep = input$xtitle_keep,
            ytitle_size = input$ytitle_size,
            coord_flip = input$coord_flip,
            ggnested = input$ggnested,
            high_level_add_other = input$high_level_add_other
          )
        } else if (plot_type == "heatmap") {
          max_abund_val_hm <- if (is.null(input$max_abundance_hm) || input$max_abundance_hm <= 0) NULL else input$max_abundance_hm
          plot_breaks_val <- {
            if (isTRUE(nzchar(input$plot_breaks_hm))) {
              tryCatch({
                vals <- as.numeric(strsplit(input$plot_breaks_hm, ",\\s*")[[1]])
                if (length(vals) > 0 && !any(is.na(vals))) vals else NULL
              }, error = function(e) NULL)
            } else NULL
          }
          x_axis_name_val <- if (isTRUE(nzchar(input$x_axis_name_hm))) input$x_axis_name_hm else NULL
          order_x_hm_val <- if (!is.null(local_rv$sample_order_hm) && length(local_rv$sample_order_hm) > 0) local_rv$sample_order_hm else NULL
          t_abund$plot_heatmap(
            color_values = rev(RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")),
            facet = local_rv$facet_cols_hm,
            facet_switch = input$facet_switch,
            x_axis_name = x_axis_name_val,
            order_x = order_x_hm_val,
            withmargin = input$withmargin,
            plot_numbers = input$plot_numbers,
            plot_text_size = input$plot_text_size,
            plot_breaks = plot_breaks_val,
            plot_colorscale = input$plot_colorscale,
            min_abundance = input$min_abundance,
            max_abundance = max_abund_val_hm,
            strip_text = input$strip_text_hm,
            xtext_keep = input$xtext_keep_hm,
            xtext_angle = input$xtext_angle_hm,
            xtext_size = input$xtext_size_hm,
            ytext_size = input$ytext_size_hm,
            xtitle_keep = input$xtitle_keep_hm,
            grid_clean = input$grid_clean,
            legend_title = input$legend_title,
            margincolor = input$margincolor,
            pheatmap = input$pheatmap
          )
        } else if (plot_type == "box") {
          group_box_val <- if (isTRUE(nzchar(input$group_col_box))) input$group_col_box else NULL
          t_abund$plot_box(
            color_values = get_color_palette(input$color_theme_box),
            group = group_box_val,
            show_point = input$show_point_box,
            point_color = input$point_color_box,
            point_size = input$point_size_box,
            point_alpha = input$point_alpha_box,
            plot_flip = input$plot_flip_box,
            boxfill = input$boxfill_box,
            middlecolor = input$middlecolor_box,
            middlesize = input$middlesize_box,
            xtext_angle = input$xtext_angle_box,
            xtext_size = input$xtext_size_box,
            ytitle_size = input$ytitle_size_box
          )
        } else if (plot_type == "line") {
          t_abund$plot_line(
            color_values = get_color_palette(input$color_theme_line),
            plot_SE = input$plot_SE_line,
            errorbar_size = input$errorbar_size_line,
            errorbar_width = input$errorbar_width_line,
            point_size = input$point_size_line,
            point_alpha = input$point_alpha_line,
            line_size = input$line_size_line,
            line_alpha = input$line_alpha_line,
            line_type = input$line_type_line,
            xtext_angle = input$xtext_angle_line,
            xtext_size = input$xtext_size_line,
            ytitle_size = input$ytitle_size_line
          )
        } else if (plot_type == "donut") {
          t_abund$plot_donut(
            color_values = get_color_palette(input$color_theme_donut),
            label = input$label_donut,
            facet_nrow = input$facet_nrow_donut,
            legend_text_italic = input$legend_text_italic_donut
          )
        } else {
          t_abund$plot_pie(
            color_values = get_color_palette(input$color_theme_pie),
            facet_nrow = input$facet_nrow_pie,
            strip_text = input$strip_text_pie,
            add_label = input$add_label_pie,
            legend_text_italic = input$legend_text_italic_pie
          )
        }

        list(success = TRUE, plot = p, data_abund = t_abund$data_abund)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        append_code(rv, paste0("# \u9519\u8bef: ", result$error, "\n", code), "\u7ec4\u6210\u53ef\u89c6\u5316 - \u9519\u8bef")
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      append_code(rv, code, paste0("组成可视化 - ", plot_type))

      local_rv$plot <- result$plot
      local_rv$data_abund <- result$data_abund
      rv$last_plot <- result$plot
      showNotification("完成", type = "message")
    })

    output$composition_plot <- shiny::renderPlot({ req(local_rv$plot); local_rv$plot })

    output$abund_table <- DT::renderDataTable({
      req(local_rv$data_abund)
      DT::datatable(local_rv$data_abund, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
    })

    output$save_plot <- downloadHandler(
      filename = function() {
        fname <- paste0("composition_", input$plot_type, ".", input$image_format)
      },
      content = function(file) {
        req(local_rv$plot)
        ggplot2::ggsave(
          filename = file,
          plot = local_rv$plot,
          width = input$save_width,
          height = input$save_height,
          units = "in",
          dpi = input$save_dpi,
          scale = 1
        )
      },
      contentType = switch(input$image_format,
        "png" = "image/png",
        "pdf" = "application/pdf",
        "svg" = "image/svg+xml",
        "tiff" = "image/tiff",
        "image/png"
      )
    )

    output$download_table <- downloadHandler(
      filename = function() paste0("composition_", input$plot_type, ifelse(input$table_format == ",", ".csv", ".tsv")),
      content = function(file) {
        req(local_rv$data_abund)
        write.table(local_rv$data_abund, file, sep = input$table_format, row.names = FALSE, quote = TRUE)
      }
    )
  })
}