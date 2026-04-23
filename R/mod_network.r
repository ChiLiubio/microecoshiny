#' @title Network Analysis Module UI
#' @description
#' Provides interface for microbial co-occurrence network analysis including
#' correlation calculation, network construction, module detection, topology
#' analysis, node/edge roles, and various visualization methods.
#' Supports complete parameter options from trans_network class.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @importFrom shinyFiles getVolumes parseDirPath shinyDirButton shinyDirChoose
#' @keywords network co-occurrence microbiome
#' @family advanced-analysis
mod_network_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(column(12, h2(tr("\U0001f310 网络分析 Network Analysis", "\U0001f310 Network Analysis")))),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4cb 参数设置", "\U0001f4cb Parameters"),
          status = "primary", solidHeader = TRUE, width = NULL, collapsible = FALSE,
          h4(tr("基本参数", "Basic Parameters")),
          fluidRow(
            column(2, shiny::selectInput(ns("net_cor_method"), tr("cor_method (相关方法)", "cor_method"),
              choices = c("spearman", "pearson", "bray", "sparcc", "bicor", "cclasso", "ccrepe", "NULL"),
              selected = "spearman")),
            column(2, shiny::selectInput(ns("net_taxa_level"), tr("taxa_level (分类水平)", "taxa_level"),
              choices = c("Genus", "OTU", "Phylum", "Class", "Order", "Family"),
              selected = "Genus")),
            column(2, shiny::numericInput(ns("net_filter_thres"), tr("filter_thres (相对丰度阈值)", "filter_thres (RA threshold)"),
              value = 0, min = 0, max = 1, step = 0.0001)),
            column(2, shinyWidgets::pickerInput(ns("net_env_cols"), tr("env_cols (环境因子)", "env_cols"),
              choices = character(0), selected = character(0), multiple = TRUE,
              options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE))),
            column(2, shiny::numericInput(ns("net_nThreads"), "nThreads", value = 1, min = 1, max = 16)),
            column(2)
          ),
          fluidRow(
            column(3, shinyWidgets::materialSwitch(ns("net_use_WGCNA"), "use_WGCNA_pearson_spearman", value = FALSE, status = "info")),
            column(3, shinyWidgets::materialSwitch(ns("net_use_NetCoMi"), "use_NetCoMi_pearson_spearman", value = FALSE, status = "warning")),
            column(3, shinyWidgets::materialSwitch(ns("net_use_sparcc_method"), "use_sparcc_method = SpiecEasi", value = FALSE, status = "success")),
            column(3)
          ),
          hr(),
          h4(tr("分析类型", "Analysis Type")),
          fluidRow(
            column(12, shiny::radioButtons(ns("net_analysis_type"), tr("分析类型", "Analysis Type"),
              choices = setNames(c("cor", "spieceasi", "gcoda", "roles", "sumlinks", "attr"),
                                 c(tr("COR网络", "COR Network"), tr("SpiecEasi网络", "SpiecEasi Network"),
                                   tr("gcoda网络", "gcoda Network"), tr("节点角色分析", "Node Roles"),
                                   tr("链接分析", "Link Analysis"), tr("网络属性", "Network Attributes"))),
              selected = "cor", inline = TRUE))
          ),
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'cor' || input.net_analysis_type == 'spieceasi' || input.net_analysis_type == 'gcoda'", ns = ns,
            h4(paste0("cal_network ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shiny::selectInput(ns("net_network_method"), "network_method",
                choices = c("COR", "SpiecEasi", "gcoda"), selected = "COR")),
              column(3, shiny::selectInput(ns("net_plot_method"), tr("plot_network method", "plot_network method"),
                choices = c("igraph" = "igraph", "ggraph" = "ggraph", "networkD3" = "networkD3"),
                selected = "igraph")),
              column(2, shiny::selectInput(ns("net_node_label"), "node_label",
                choices = c("name", "Abundance", "Phylum", "Class", "Order", "Family", "Genus", "module"),
                selected = "name")),
              column(2, shiny::selectInput(ns("net_node_color"), "node_color",
                choices = c("none" = "", "name" = "name", "Phylum" = "Phylum", "Class" = "Class",
                  "Order" = "Order", "Family" = "Family", "Genus" = "Genus", "module" = "module"),
                selected = "")),
              column(2)
            ),
            fluidRow(
              column(2, shinyWidgets::materialSwitch(ns("net_ggraph_text"), "ggraph_node_text", value = TRUE, status = "info")),
              column(2, shiny::numericInput(ns("net_ggraph_size"), "ggraph_node_size", value = 2, min = 0.5, max = 6)),
              column(2, shiny::numericInput(ns("net_ggraph_text_size"), "ggraph_text_size", value = 3, min = 1, max = 8)),
              column(2, shiny::selectInput(ns("net_ggraph_layout"), "ggraph_layout",
                choices = c("fr", "kk", "lgl", "graphopt", "drl", "mds"), selected = "fr")),
              column(2, shinyWidgets::materialSwitch(ns("net_d3_zoom"), "networkD3_zoom", value = TRUE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("net_d3_legend"), "networkD3_node_legend", value = TRUE, status = "info"))
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("net_cor_cut"), tr("COR_cut (相关系数阈值)", "COR_cut (correlation threshold)"),
                value = 0.6, min = 0, max = 1, step = 0.05)),
              column(2, shiny::numericInput(ns("net_cor_p_thres"), tr("COR_p_thres (P值阈值)", "COR_p_thres (P-value threshold)"),
                value = 0.01, min = 0, max = 0.1, step = 0.001)),
              column(2, shiny::selectInput(ns("net_cor_p_adjust"), "COR_p_adjust",
                choices = c("fdr", "holm", "bonferroni", "none", "BH", "BY"), selected = "fdr")),
              column(2, shinyWidgets::materialSwitch(ns("net_cor_return_padjust"), "COR_return_padjust", value = FALSE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("net_cor_weight"), tr("COR_weight (使用相关系数作为边权重)", "COR_weight (use correlation as edge weight)"), value = TRUE, status = "primary"))
            ),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("net_cor_optimization"), tr("COR_optimization (RMT优化)", "COR_optimization (RMT optimization)"), value = FALSE, status = "warning")),
              column(3, shiny::numericInput(ns("net_cor_opt_low"), "COR_optimization_low", value = 0.01, min = 0, max = 1, step = 0.01)),
              column(3, shiny::numericInput(ns("net_cor_opt_high"), "COR_optimization_high", value = 0.8, min = 0, max = 1, step = 0.01)),
              column(3, shiny::numericInput(ns("net_cor_opt_seq"), "COR_optimization_seq", value = 0.01, min = 0.001, max = 0.1, step = 0.001))
            ),
            fluidRow(
              column(3, shiny::selectInput(ns("net_spieceasi_method"), "SpiecEasi_method",
                choices = c("mb", "glasso"), selected = "mb")),
              column(3, shinyWidgets::materialSwitch(ns("net_add_taxa_name"), "add_taxa_name", value = TRUE, status = "info")),
              column(3, shiny::selectInput(ns("net_taxa_name_level"), "add_taxa_name_level",
                choices = c("Phylum", "Class", "Order", "Family", "Genus"), selected = "Phylum")),
              column(3, shinyWidgets::materialSwitch(ns("net_delete_unlinked"), "delete_unlinked_nodes", value = TRUE, status = "success"))
            )
          ),
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'roles'", ns = ns,
            h4(paste0("cal_module + plot_taxa_roles ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shiny::selectInput(ns("roles_cal_method"), "cal_module method",
                choices = c("cluster_fast_greedy", "cluster_walktrap", "cluster_edge_betweenness",
                  "cluster_infomap", "cluster_label_prop", "cluster_leading_eigen",
                  "cluster_louvain", "cluster_spinglass", "cluster_optimal"),
                selected = "cluster_fast_greedy")),
              column(3, shiny::textInput(ns("roles_module_prefix"), "module_name_prefix", value = "M")),
              column(6)
            ),
            fluidRow(
              column(2, shiny::selectInput(ns("roles_plot_type"), "use_type",
                choices = setNames(c("1", "2"), c(tr("Zi-Pi散点图", "Zi-Pi Scatter"), tr("分层图", "Hierarchical Plot"))),
                selected = "1")),
              column(2, shinyWidgets::materialSwitch(ns("roles_color_bg"), "roles_color_background", value = FALSE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("roles_add_label"), tr("add_label (添加标签)", "add_label"), value = FALSE, status = "warning")),
              column(2, shinyWidgets::pickerInput(ns("roles_label_group"), "add_label_group",
                choices = c("Network hubs", "Module hubs", "Connectors"),
                selected = "Network hubs", multiple = TRUE,
                options = shinyWidgets::pickerOptions(actionsBox = TRUE))),
              column(2, shiny::numericInput(ns("roles_label_size"), "label_text_size", value = 4, min = 2, max = 8)),
              column(2, shiny::textInput(ns("roles_label_color"), "label_text_color", value = "grey50"))
            ),
            fluidRow(
              column(2, shinyWidgets::materialSwitch(ns("roles_plot_module"), "plot_module", value = FALSE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("roles_label_italic"), "label_text_italic", value = FALSE, status = "primary")),
              column(4, shiny::selectInput(ns("roles_use_level"), "use_level (type=2)",
                choices = c("Phylum", "Class", "Order", "Family", "Genus"), selected = "Phylum")),
              column(4)
            )
          ),
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'sumlinks'", ns = ns,
            h4(paste0("cal_sum_links + plot_sum_links ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("sumlinks_plot_pos"), tr("plot_pos (正相关)", "plot_pos (positive correlation)"), value = TRUE, status = "primary")),
              column(3, shiny::numericInput(ns("sumlinks_plot_num"), "plot_num", value = NULL, min = 1)),
              column(3, shiny::selectInput(ns("sumlinks_color_theme"), "color_values",
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"),
                selected = "Dark2")),
              column(3, shiny::selectInput(ns("sumlinks_method"), "method",
                choices = c("chorddiag", "circlize"), selected = "chorddiag"))
            ),
            fluidRow(
              column(3, shiny::selectInput(ns("sumlinks_taxa_level"), "taxa_level",
                choices = c("Phylum", "Class", "Order", "Family", "Genus"), selected = "Phylum")),
              column(9)
            )
          ),
          hr(),
          h4(tr("图片保存与下载", "Save & Download Plot")),
          fluidRow(
            column(2, shiny::actionButton(ns("run_network"), tr("\U0001f4ca 构建网络", "\U0001f4ca Build Network"),
              icon = icon("play"), class = "btn-primary", width = "100%")),
            column(2, shiny::selectInput(ns("net_image_format"), tr("格式", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("net_save_width"), tr("宽 (width)", "Width"), value = 12, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("net_save_height"), tr("高 (height)", "Height"), value = 10, min = 4, max = 15)),
            column(2, shiny::numericInput(ns("net_save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(2, shiny::actionButton(ns("net_save_plot_btn"), tr("\U0001f4e5保存图片", "\U0001f4e5Save Plot"),
              icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
          ),
          fluidRow(
            column(2, shiny::selectInput(ns("net_table_download_type"), tr("下载类型", "Download Type"),
              choices = setNames(c("topo", "nodes", "edges"), c(tr("拓扑指标", "Topology"), tr("节点表", "Nodes"), tr("边表", "Edges"))),
              selected = "topo")),
            column(2, shiny::selectInput(ns("net_table_format"), tr("表格", "Table"),
              choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
            column(2, shiny::downloadButton(ns("net_download_table"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
              class = "btn-outline-info", width = "100%")),
            column(6)
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] !== 'networkD3'", ns("net_plot_method")),
            shinycssloaders::withSpinner(shiny::plotOutput(ns("network_plot"), height = "600px"))
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] === 'networkD3'", ns("net_plot_method")),
            shinycssloaders::withSpinner(networkD3::forceNetworkOutput(ns("network_plot_html"), height = "600px"))
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("network_table"))
        )
      )
    )
  )
}

mod_network_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(
      plot = NULL,
      data_result = NULL,
      save_dir = NULL,
      result_obj = NULL,
      table_type = "topo"
    )

    volumes <- c(
      Home      = path.expand("~"),
      Desktop   = file.path(path.expand("~"), "Desktop"),
      Documents = file.path(path.expand("~"), "Documents"),
      get_volumes_safe()
    )
    shinyFiles::shinyDirChoose(input, "net_save_dir_btn", roots = volumes,
      session = session, defaultRoot = "Desktop")

    observeEvent(input$net_save_dir_btn, {
      parsed <- shinyFiles::parseDirPath(volumes, input$net_save_dir_btn)
      if (!is.null(parsed) && length(parsed) > 0 && nchar(parsed) > 0) {
        local_rv$save_dir <- parsed
      }
    })

    observeEvent(input$net_save_plot_btn, {
      default_name <- paste0("network_", input$net_analysis_type, ".", input$net_image_format)
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
                shiny::textInput(ns("net_save_dir_text"), label = NULL, value = dir_display,
                  placeholder = "\u8bf7\u70b9\u51fb\u53f3\u4fa7\u6309\u94ae\u9009\u62e9\u6587\u4ef6\u5939...",
                  width = "100%")
              ),
              column(3, style = "padding-top: 0;",
                shinyFiles::shinyDirButton(ns("net_save_dir_btn"), "\u6d4f\u89c8",
                  "\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939",
                  class = "btn-outline-primary", icon = icon("folder-open"),
                  style = "margin-top: 25px;")
              )
            )
          )
        ),
        tags$hr(style = "margin: 12px 0;"),
        fluidRow(
          column(6, shiny::textInput(ns("net_save_filename"), "\U0001f4dd \u6587\u4ef6\u540d\u79f0",
            value = default_name)),
          column(6, tags$div(style = "padding-top: 28px;",
            tags$p(class = "text-muted", style = "margin-bottom: 0;",
              tags$span(icon("info-circle")), " \u683c\u5f0f: ",
              tags$code(toupper(input$net_image_format)),
              " | \u5bbd\u00d7\u9ad8: ",
              tags$code(paste0(input$net_save_width, "\u00d7", input$net_save_height)),
              " | DPI: ", tags$code(input$net_save_dpi))
          ))
        ),
        footer = tagList(
          shiny::actionButton(ns("net_confirm_save"), "\u2705 \u4fdd\u5b58\u5230\u6587\u4ef6\u5939",
            icon = icon("save"), class = "btn-primary"),
          shiny::modalButton("\u53d6\u6d88")
        )
      ))
    })

    observeEvent(input$net_confirm_save, {
      req(local_rv$plot)
      save_dir <- local_rv$save_dir
      if (is.null(save_dir) || !isTRUE(nchar(save_dir) > 0)) {
        showNotification("\u8bf7\u5148\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939", type = "warning")
        return()
      }
      fname <- input$net_save_filename
      if (is.null(fname) || !isTRUE(nzchar(trimws(fname)))) {
        fname <- paste0("network_", input$net_analysis_type)
      }
      fname <- trimws(fname)
      ext <- input$net_image_format
      if (!grepl(paste0("\\.", ext, "$"), fname, ignore.case = TRUE)) {
        fname <- sub("\\.(png|pdf|svg|tiff|tif)$", "", fname, ignore.case = TRUE)
        fname <- paste0(fname, ".", ext)
      }
      if (!dir.exists(save_dir)) {
        showNotification("\u6587\u4ef6\u5939\u4e0d\u5b58\u5728\uff0c\u8bf7\u91cd\u65b0\u9009\u62e9", type = "error")
        return()
      }
      full_path <- file.path(save_dir, fname)
      ext <- input$net_image_format

      tryCatch({
        plot_obj <- local_rv$plot
        if (inherits(plot_obj, "ggplot") || inherits(plot_obj, "ggplot2")) {
          ggplot2::ggsave(filename = full_path, plot = plot_obj,
            width = input$net_save_width, height = input$net_save_height,
            units = "in", dpi = input$net_save_dpi, scale = 1)
        } else if (inherits(plot_obj, "htmlwidget")) {
          if (ext == "png") {
            if (requireNamespace("webshot2", quietly = TRUE)) {
              tmp_html <- tempfile(fileext = ".html")
              htmlwidgets::saveWidget(plot_obj, file = tmp_html)
              webshot2::webshot(tmp_html, file = full_path,
                vwidth = as.integer(input$net_save_width * 96),
                vheight = as.integer(input$net_save_height * 96))
              file.remove(tmp_html)
            } else {
              htmlwidgets::saveWidget(plot_obj, file = sub("\\.png$", ".html", full_path))
              full_path <- sub("\\.png$", ".html", full_path)
            }
          } else {
            htmlwidgets::saveWidget(plot_obj, file = sub(paste0("\\.", ext), ".html", full_path))
            full_path <- sub(paste0("\\.", ext), ".html", full_path)
          }
        } else if (inherits(plot_obj, "igraph")) {
          if (ext == "png") {
            png(full_path, width = input$net_save_width, height = input$net_save_height,
              units = "in", res = input$net_save_dpi)
          } else if (ext == "pdf") {
            pdf(full_path, width = input$net_save_width, height = input$net_save_height)
          } else if (ext == "svg") {
            svg(full_path, width = input$net_save_width, height = input$net_save_height)
          } else if (ext %in% c("tiff", "tif")) {
            tiff(full_path, width = input$net_save_width, height = input$net_save_height,
              units = "in", res = input$net_save_dpi)
          } else {
            png(full_path, width = input$net_save_width, height = input$net_save_height,
              units = "in", res = input$net_save_dpi)
          }
          igraph::plot.igraph(plot_obj)
          dev.off()
        } else {
          if (ext == "png") {
            png(full_path, width = input$net_save_width, height = input$net_save_height,
              units = "in", res = input$net_save_dpi)
          } else if (ext == "pdf") {
            pdf(full_path, width = input$net_save_width, height = input$net_save_height)
          } else if (ext == "svg") {
            svg(full_path, width = input$net_save_width, height = input$net_save_height)
          } else if (ext %in% c("tiff", "tif")) {
            tiff(full_path, width = input$net_save_width, height = input$net_save_height,
              units = "in", res = input$net_save_dpi)
          } else {
            png(full_path, width = input$net_save_width, height = input$net_save_height,
              units = "in", res = input$net_save_dpi)
          }
          if (is.function(print(plot_obj, newpage = FALSE))) {
            print(plot_obj, newpage = FALSE)
          } else {
            print(plot_obj)
          }
          dev.off()
        }
        removeModal()
        showNotification(paste0("\u2705 \u5df2\u4fdd\u5b58\u81f3: ", full_path), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("\u4fdd\u5b58\u5931\u8d25: ", e$message), type = "error", duration = 10)
      })
    })

    observe({
      if (!check_microtable(rv)) {
        updatePickerInput(session, "net_env_cols", choices = character(0), selected = character(0))
        return()
      }
      cols <- get_sample_cols(rv)
      updatePickerInput(session, "net_env_cols", choices = cols, selected = character(0))
    })

    get_color_palette <- function(name, n = 8) {
      tryCatch({
        switch(name,
          "Dark2"    = RColorBrewer::brewer.pal(min(n, 8), "Dark2"),
          "Set1"     = RColorBrewer::brewer.pal(min(n, 8), "Set1"),
          "Set2"     = RColorBrewer::brewer.pal(min(n, 8), "Set2"),
          "Set3"     = RColorBrewer::brewer.pal(min(n, 12), "Set3"),
          "Paired"   = RColorBrewer::brewer.pal(min(n, 12), "Paired"),
          "Spectral" = RColorBrewer::brewer.pal(min(n, 11), "Spectral"),
          "Viridis"  = viridisLite::viridis(n),
          RColorBrewer::brewer.pal(min(n, 8), "Dark2")
        )
      }, error = function(e) RColorBrewer::brewer.pal(min(n, 8), "Dark2"))
    }

    observeEvent(input$run_network, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      analysis <- input$net_analysis_type
      result <- tryCatch({
        mt <- rv$microtable
        dataset_name <- rv$microtable_name %||% "tmp_microtable"

        cor_method_val <- if (input$net_cor_method == "NULL") NULL else input$net_cor_method
        env_cols_val <- if (length(input$net_env_cols) > 0) input$net_env_cols else NULL

        t_net <- microeco::trans_network$new(
          dataset = mt,
          cor_method = cor_method_val,
          use_WGCNA_pearson_spearman = input$net_use_WGCNA,
          use_NetCoMi_pearson_spearman = input$net_use_NetCoMi,
          use_sparcc_method = if (input$net_use_sparcc_method) "SpiecEasi" else "NetCoMi",
          taxa_level = input$net_taxa_level,
          filter_thres = input$net_filter_thres,
          nThreads = input$net_nThreads,
          env_cols = env_cols_val
        )

        init_code <- paste0(
          "t_net <- microeco::trans_network$new(\n",
          "  dataset = ", dataset_name, ",\n",
          if (!is.null(cor_method_val)) paste0("  cor_method = \"", cor_method_val, "\",\n") else "  cor_method = NULL,\n",
          "  taxa_level = \"", input$net_taxa_level, "\",\n",
          "  filter_thres = ", input$net_filter_thres, "\n",
          ")\n"
        )

        if (analysis == "cor" || analysis == "spieceasi" || analysis == "gcoda") {
          network_method_val <- switch(analysis,
            "cor" = "COR",
            "spieceasi" = "SpiecEasi",
            "gcoda" = "gcoda"
          )

          t_net$cal_network(
            network_method = network_method_val,
            COR_p_thres = input$net_cor_p_thres,
            COR_p_adjust = input$net_cor_p_adjust,
            COR_return_padjust = input$net_cor_return_padjust,
            COR_weight = input$net_cor_weight,
            COR_cut = input$net_cor_cut,
            COR_optimization = input$net_cor_optimization,
            COR_optimization_low_high = c(input$net_cor_opt_low, input$net_cor_opt_high),
            COR_optimization_seq = input$net_cor_opt_seq,
            SpiecEasi_method = input$net_spieceasi_method,
            add_taxa_name = if (input$net_add_taxa_name) input$net_taxa_name_level else "Phylum",
            delete_unlinked_nodes = input$net_delete_unlinked
          )

          t_net$get_node_table()
          t_net$get_edge_table()
          t_net$cal_network_attr()

          plot_method_val <- input$net_plot_method
          node_label_val <- input$net_node_label
          node_color_val <- if (isTRUE(nzchar(input$net_node_color))) input$net_node_color else NULL

          if (plot_method_val == "igraph") {
            plot_obj <- t_net$plot_network(method = "igraph")
          } else if (plot_method_val == "ggraph") {
            plot_obj <- t_net$plot_network(
              method = "ggraph",
              node_label = node_label_val,
              node_color = node_color_val,
              ggraph_layout = input$net_ggraph_layout,
              ggraph_node_size = input$net_ggraph_size,
              ggraph_node_text = input$net_ggraph_text,
              ggraph_text_size = input$net_ggraph_text_size
            )
          } else {
            if (is.null(node_color_val)) {
              node_color_val <- "module"
              if (!"module" %in% names(t_net$res_node_table)) {
                t_net$cal_module()
              }
            }
            plot_obj <- t_net$plot_network(
              method = "networkD3",
              node_label = node_label_val,
              node_color = node_color_val,
              networkD3_zoom = input$net_d3_zoom,
              networkD3_node_legend = input$net_d3_legend
            )
          }
          local_rv$table_type <- "topo"

          cal_code <- paste0(
            "t_net$cal_network(\n",
            "  network_method = \"", network_method_val, "\",\n",
            "  COR_p_thres = ", input$net_cor_p_thres, ",\n",
            "  COR_p_adjust = \"", input$net_cor_p_adjust, "\",\n",
            "  COR_cut = ", input$net_cor_cut, "\n",
            ")\n",
            "t_net$get_node_table()\n",
            "t_net$get_edge_table()\n",
            "t_net$cal_network_attr()\n"
          )
          code <- paste0(init_code, "# ", analysis, " \u7f51\u7edc\u5206\u6790\n", cal_code)

          list(success = TRUE, plot = plot_obj, data_result = t_net$res_network_attr,
               result_obj = t_net, code = code)

        } else if (analysis == "roles") {
          t_net$cal_network(
            network_method = "COR",
            COR_p_thres = input$net_cor_p_thres,
            COR_p_adjust = input$net_cor_p_adjust,
            COR_weight = input$net_cor_weight,
            COR_cut = input$net_cor_cut,
            add_taxa_name = if (input$net_add_taxa_name) input$net_taxa_name_level else "Phylum",
            delete_unlinked_nodes = input$net_delete_unlinked
          )

          t_net$cal_module(
            method = input$roles_cal_method,
            module_name_prefix = input$roles_module_prefix
          )

          t_net$get_node_table(node_roles = TRUE)

          use_type_val <- as.integer(input$roles_plot_type)

          if (use_type_val == 1) {
            add_label_group_val <- if (input$roles_add_label && length(input$roles_label_group) > 0) input$roles_label_group else NULL
          plot_obj <- t_net$plot_taxa_roles(
            use_type = use_type_val,
            roles_color_background = input$roles_color_bg,
            add_label = input$roles_add_label,
            add_label_group = add_label_group_val,
            label_text_size = input$roles_label_size,
            label_text_color = input$roles_label_color,
            label_text_italic = input$roles_label_italic,
            plot_module = input$roles_plot_module,
            x_lim = c(0, 1)
          )
          } else {
            color_values_val <- get_color_palette(input$roles_color_theme, n = 12)
            plot_obj <- t_net$plot_taxa_roles(
              use_type = use_type_val,
              use_level = input$roles_use_level,
              show_value = c("z", "p"),
              show_number = 1:10,
              plot_color = "Phylum",
              plot_shape = "taxa_roles",
              plot_size = "Abundance",
              color_values = color_values_val,
              shape_values = c(16, 17, 7, 8, 15, 18, 11, 10, 12, 13, 9, 3, 4, 0, 1, 2, 14)
            )
          }

          local_rv$table_type <- "nodes"

          roles_code <- paste0(
            "t_net$cal_network(\n",
            "  network_method = \"COR\",\n",
            "  COR_p_thres = ", input$net_cor_p_thres, ",\n",
            "  COR_p_adjust = \"", input$net_cor_p_adjust, "\",\n",
            "  COR_cut = ", input$net_cor_cut, "\n",
            ")\n",
            "t_net$cal_module(\n",
            "  method = \"", input$roles_cal_method, "\",\n",
            "  module_name_prefix = \"", input$roles_module_prefix, "\"\n",
            ")\n",
            "t_net$get_node_table(node_roles = TRUE)\n",
            "p <- t_net$plot_taxa_roles(use_type = ", use_type_val, ")\n"
          )
          code <- paste0(init_code, "# \u8282\u70b9\u89d2\u8272\u5206\u6790\n", roles_code)

          list(success = TRUE, plot = plot_obj, data_result = t_net$res_node_table,
               result_obj = t_net, code = code)

        } else if (analysis == "sumlinks") {
          t_net$cal_network(
            network_method = "COR",
            COR_p_thres = input$net_cor_p_thres,
            COR_p_adjust = input$net_cor_p_adjust,
            COR_weight = input$net_cor_weight,
            COR_cut = input$net_cor_cut,
            add_taxa_name = if (input$net_add_taxa_name) input$net_taxa_name_level else "Phylum",
            delete_unlinked_nodes = input$net_delete_unlinked
          )

          t_net$cal_sum_links(
            taxa_level = input$sumlinks_taxa_level
          )

          plot_obj <- t_net$plot_sum_links(
            plot_pos = input$sumlinks_plot_pos,
            plot_num = if (!is.null(input$sumlinks_plot_num) && input$sumlinks_plot_num > 0) input$sumlinks_plot_num else NULL,
            color_values = get_color_palette(input$sumlinks_color_theme, n = 8),
            method = input$sumlinks_method
          )

          local_rv$table_type <- "topo"

          sumlinks_code <- paste0(
            "t_net$cal_network(\n",
            "  network_method = \"COR\",\n",
            "  COR_p_thres = ", input$net_cor_p_thres, ",\n",
            "  COR_cut = ", input$net_cor_cut, "\n",
            ")\n",
            "t_net$cal_sum_links(taxa_level = \"", input$sumlinks_taxa_level, "\")\n",
            "p <- t_net$plot_sum_links(\n",
            "  plot_pos = ", input$sumlinks_plot_pos, ",\n",
            "  color_values = get_color_palette(\"", input$sumlinks_color_theme, "\"),\n",
            "  method = \"", input$sumlinks_method, "\"\n",
            ")\n"
          )
          code <- paste0(init_code, "# \u94fe\u63a5\u5206\u6790\n", sumlinks_code)

          list(success = TRUE, plot = plot_obj, data_result = t_net$res_sum_links_pos,
               result_obj = t_net, code = code)

        } else if (analysis == "attr") {
          t_net$cal_network(
            network_method = "COR",
            COR_p_thres = input$net_cor_p_thres,
            COR_p_adjust = input$net_cor_p_adjust,
            COR_weight = input$net_cor_weight,
            COR_cut = input$net_cor_cut,
            add_taxa_name = if (input$net_add_taxa_name) input$net_taxa_name_level else "Phylum",
            delete_unlinked_nodes = input$net_delete_unlinked
          )

          t_net$cal_network_attr()
          t_net$get_node_table()
          t_net$get_edge_table()

          local_rv$table_type <- "topo"

          attr_code <- paste0(
            "t_net$cal_network(\n",
            "  network_method = \"COR\",\n",
            "  COR_p_thres = ", input$net_cor_p_thres, ",\n",
            "  COR_p_adjust = \"", input$net_cor_p_adjust, "\",\n",
            "  COR_cut = ", input$net_cor_cut, "\n",
            ")\n",
            "t_net$cal_network_attr()\n",
            "t_net$get_node_table()\n",
            "t_net$get_edge_table()\n"
          )
          code <- paste0(init_code, "# \u7f51\u7edc\u5c5e\u6027\u5206\u6790\n", attr_code)

          list(success = TRUE, plot = NULL, data_result = t_net$res_network_attr,
               result_obj = t_net, code = code)

        } else {
          stop("\u672a\u77e5\u5206\u6790\u7c7b\u578b")
        }
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      append_code(rv, result$code, paste0("\u7f51\u7edc\u5206\u6790 - ", analysis))
      local_rv$plot <- result$plot
      local_rv$data_result <- result$data_result
      local_rv$result_obj <- result$result_obj
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$network_plot <- shiny::renderPlot({
      req(local_rv$plot)
      if (is(local_rv$plot, "ggplot")) {
        print(local_rv$plot)
      } else if (is(local_rv$plot, "igraph")) {
        igraph::plot.igraph(local_rv$plot)
      } else {
        print(local_rv$plot)
      }
    })

    output$network_plot_html <- networkD3::renderForceNetwork({
      req(local_rv$plot)
      if (inherits(local_rv$plot, "forceNetwork") || inherits(local_rv$plot, "htmlwidget")) {
        local_rv$plot
      }
    })

    output$network_table <- DT::renderDataTable({
      table_type <- local_rv$table_type
      obj <- local_rv$result_obj

      if (is.null(obj) || !is(obj, "trans_network")) return(NULL)

      dt <- switch(table_type,
        "topo" = {
          if (!is.null(obj$res_network_attr)) obj$res_network_attr else NULL
        },
        "nodes" = {
          if (!is.null(obj$res_node_table)) obj$res_node_table else NULL
        },
        "edges" = {
          if (!is.null(obj$res_edge_table)) obj$res_edge_table else NULL
        },
        NULL
      )

      if (is.data.frame(dt) || is.matrix(dt)) {
        dt_df <- as.data.frame(dt)
        DT::datatable(dt_df, options = list(scrollX = TRUE, pageLength = 20),
          rownames = FALSE, filter = "top")
      }
    })

    output$net_download_table <- downloadHandler(
      filename = function() {
        type <- input$net_table_download_type
        ext <- ifelse(input$net_table_format == ",", ".csv", ".tsv")
        paste0("network_", type, ext)
      },
      content = function(file) {
        obj <- local_rv$result_obj
        if (is.null(obj) || !is(obj, "trans_network")) return()

        table_type <- input$net_table_download_type
        dt <- switch(table_type,
          "topo" = obj$res_network_attr,
          "nodes" = obj$res_node_table,
          "edges" = obj$res_edge_table,
          NULL
        )

        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$net_table_format,
            row.names = FALSE, quote = TRUE)
        }
      }
    )
  })
}