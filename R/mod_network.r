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
          h4(tr("网络构建", "Network Construction")),
          fluidRow(
            column(12, shiny::radioButtons(ns("net_network_method"), tr("网络方法", "Network Method"),
              choices = setNames(c("spearman", "sparcc", "SpiecEasi", "FlashWeave", "beemStatic"),
                                 c("spearman", "sparcc", "SpiecEasi", "FlashWeave", "beemStatic")),
              selected = "spearman", inline = TRUE))
          ),
          hr(),
          h5(tr("通用参数", "General Parameters")),
          fluidRow(
            column(2, shiny::selectInput(ns("net_taxa_level"), tr("taxa_level", "taxa_level"),
              choices = c("OTU", "Genus", "Phylum", "Class", "Order", "Family"),
              selected = "OTU")),
            column(2, shiny::numericInput(ns("net_filter_thres"), tr("filter_thres", "filter_thres"),
              value = 0.0001, min = 0, max = 1, step = 0.0001)),
            column(2, shinyWidgets::materialSwitch(ns("net_add_taxa_name"), "add_taxa_name", value = TRUE, status = "info")),
            column(2, shiny::selectInput(ns("net_taxa_name_level"), "add_taxa_name_level",
              choices = c("Phylum", "Class", "Order", "Family", "Genus"), selected = "Phylum")),
            column(2, shinyWidgets::materialSwitch(ns("net_delete_unlinked"), "delete_unlinked_nodes", value = TRUE, status = "success"))
          ),
          hr(),
          shiny::conditionalPanel(condition = "input.net_network_method == 'spearman' || input.net_network_method == 'sparcc'", ns = ns,
            h5(tr("COR 参数", "COR Parameters")),
            fluidRow(
              column(2, shiny::numericInput(ns("net_cor_cut"), tr("COR_cut", "COR_cut"),
                value = 0.6, min = 0, max = 1, step = 0.05)),
              column(2, shiny::numericInput(ns("net_cor_p_thres"), tr("COR_p_thres", "COR_p_thres"),
                value = 0.01, min = 0, max = 0.1, step = 0.001)),
              column(2, shiny::selectInput(ns("net_cor_p_adjust"), "COR_p_adjust",
                choices = c("fdr", "holm", "bonferroni", "none", "BH", "BY"), selected = "fdr")),
              column(2, shinyWidgets::materialSwitch(ns("net_cor_return_padjust"), "COR_return_padjust", value = FALSE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("net_cor_weight"), tr("COR_weight", "COR_weight"), value = TRUE, status = "primary"))
            ),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("net_cor_optimization"), tr("COR_optimization (RMT)", "COR_optimization (RMT)"), value = FALSE, status = "warning")),
              column(3, shiny::numericInput(ns("net_cor_opt_low"), "COR_optimization_low", value = 0.01, min = 0, max = 1, step = 0.01)),
              column(3, shiny::numericInput(ns("net_cor_opt_high"), "COR_optimization_high", value = 0.8, min = 0, max = 1, step = 0.01)),
              column(3, shiny::numericInput(ns("net_cor_opt_seq"), "COR_optimization_seq", value = 0.01, min = 0.001, max = 0.1, step = 0.001))
            )
          ),
          shiny::conditionalPanel(condition = "input.net_network_method == 'SpiecEasi'", ns = ns,
            h5(tr("SpiecEasi 参数", "SpiecEasi Parameters")),
            fluidRow(
              column(3, shiny::selectInput(ns("net_spieceasi_method"), tr("method", "method"),
                choices = c("mb", "glasso"), selected = "mb"))
            )
          ),
          shiny::conditionalPanel(condition = "input.net_network_method == 'FlashWeave'", ns = ns,
            h5(tr("FlashWeave 参数", "FlashWeave Parameters")),
            fluidRow(
              column(2, shiny::numericInput(ns("net_flash_alpha"), "alpha", value = 0.01, min = 0, max = 1, step = 0.001)),
              column(2, shinyWidgets::materialSwitch(ns("net_flash_sensitive"), "sensitive", value = TRUE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("net_flash_heterogeneous"), "heterogeneous", value = TRUE, status = "warning"))
            )
          ),
          shiny::conditionalPanel(condition = "input.net_network_method == 'beemStatic'", ns = ns,
            h5(tr("beemStatic 参数", "beemStatic Parameters")),
            fluidRow(
              column(4, p(tr("beemStatic 参数待确认", "beemStatic parameters to be confirmed")))
            )
          ),

          hr(),
          fluidRow(
            column(3, shiny::actionButton(ns("run_network"), tr("\U0001f4ca 构建网络", "\U0001f4ca Build Network"),
              icon = icon("play"), class = "btn-success", width = "100%")),
            column(9)
          ),
          hr(),
          h4(tr("分析类型", "Analysis Type")),
          fluidRow(
            column(12, shiny::radioButtons(ns("net_analysis_type"), tr("分析类型", "Analysis Type"),
              choices = setNames(c("module", "attr", "nodes", "edges", "eigen", "sumlinks", "network", "save"),
                                 c(tr("模块划分", "Module Detection"),
                                   tr("网络属性", "Network Attributes"),
                                   tr("节点属性", "Node Attributes"),
                                   tr("边属性", "Edge Attributes"),
                                   tr("特征分析", "Eigen Analysis"),
                                   tr("边连接求和", "Edge Link Sum"),
                                   tr("网络图", "Network Plot"),
                                   tr("网络保存", "Network Save"))),
              selected = "module", inline = TRUE))
          ),

          # 1. 模块划分
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'module'", ns = ns,
            h5(tr("模块划分参数", "Module Parameters")),
            fluidRow(
              column(3, shiny::selectInput(ns("module_method"), tr("method (模块检测算法)", "method"),
                choices = c("cluster_fast_greedy", "cluster_walktrap", "cluster_edge_betweenness",
                  "cluster_infomap", "cluster_label_prop", "cluster_leading_eigen",
                  "cluster_louvain", "cluster_spinglass", "cluster_optimal"),
                selected = "cluster_fast_greedy")),
              column(3, shiny::textInput(ns("module_prefix"), "module_name_prefix", value = "M"))
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_module"), tr("\U0001f3c1 开始分析", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("module_table"))
                )
              )
            ),
            fluidRow(
              column(2, shiny::selectInput(ns("net_table_format_mod"), tr("表格", "Table"),
                choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
              column(2, shiny::downloadButton(ns("net_download_table_mod"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
                class = "btn-outline-info", width = "100%")),
              column(8)
            )
          ),

          # 2. 网络属性
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'attr'", ns = ns,
            h5(tr("网络属性参数", "Network Attributes Parameters")),
            fluidRow(
              column(12, shiny::actionButton(ns("run_attr"), tr("\U0001f3c1 开始分析", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("attr_table"))
                )
              )
            ),
            fluidRow(
              column(2, shiny::selectInput(ns("net_table_format_attr"), tr("表格", "Table"),
                choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
              column(2, shiny::downloadButton(ns("net_download_table_attr"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
                class = "btn-outline-info", width = "100%")),
              column(8)
            )
          ),

          # 3. 节点属性 - 有图形
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'nodes'", ns = ns,
            h5(tr("节点属性参数", "Node Attributes Parameters")),
            fluidRow(
              column(12, shiny::actionButton(ns("run_nodes"), tr("\U0001f3c1 计算表格", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("nodes_table"))
                )
              )
            ),
            hr(),
            h4(tr("图形参数", "Plot Parameters")),
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
              column(2, shinyWidgets::materialSwitch(ns("roles_plot_module_name"), "plot_module_name", value = TRUE, status = "success")),
              column(4)
            ),
            fluidRow(
              column(4, shiny::selectInput(ns("roles_use_level"), "use_level (type=2)",
                choices = c("Phylum", "Class", "Order", "Family", "Genus"), selected = "Phylum")),
              column(4, shiny::selectInput(ns("roles_show_value"), "show_value",
                choices = setNames(list("z", "p", c("z", "p")), c("z", "p", "z, p")),
                selected = c("z", "p"), multiple = TRUE)),
              column(2, shiny::numericInput(ns("roles_show_number"), "show_number", value = 10, min = 1, max = 50)),
              column(2, shiny::selectInput(ns("roles_plot_color"), "plot_color",
                choices = c("Phylum", "Class", "Order", "Family", "Genus"), selected = "Phylum")),
              column(2, shiny::selectInput(ns("roles_plot_shape"), "plot_shape",
                choices = c("taxa_roles", "module"), selected = "taxa_roles")),
              column(2, shiny::selectInput(ns("roles_plot_size"), "plot_size",
                choices = c("Abundance", "degree", "betweenness"), selected = "Abundance"))
            ),
            fluidRow(
              column(12, shiny::actionButton(ns("run_nodes_plot"), tr("\U0001f3c1 开始作图", "\U0001f3c1 Run Plot"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("nodes_plot"), height = "600px"))
                )
              )
            ),
            hr(),
            h4(tr("图片保存与下载", "Save & Download Plot")),
            fluidRow(
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
                choices = setNames(c("nodes"), c(tr("节点表", "Nodes"))),
                selected = "nodes")),
              column(2, shiny::selectInput(ns("net_table_format"), tr("表格", "Table"),
                choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
              column(2, shiny::downloadButton(ns("net_download_table"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
                class = "btn-outline-info", width = "100%")),
              column(6)
            )
          ),

          # 4. 边属性
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'edges'", ns = ns,
            h5(tr("边属性参数", "Edge Attributes Parameters")),
            fluidRow(
              column(12, shiny::actionButton(ns("run_edges"), tr("\U0001f3c1 开始分析", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("edges_table"))
                )
              )
            ),
            fluidRow(
              column(2, shiny::selectInput(ns("net_table_format_edges"), tr("表格", "Table"),
                choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
              column(2, shiny::downloadButton(ns("net_download_table_edges"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
                class = "btn-outline-info", width = "100%")),
              column(8)
            )
          ),

          # 5. 特征分析
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'eigen'", ns = ns,
            h5(tr("特征分析参数", "Eigen Analysis Parameters")),
            fluidRow(
              column(12, shiny::actionButton(ns("run_eigen"), tr("\U0001f3c1 开始分析", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("eigen_table"))
                )
              )
            ),
            fluidRow(
              column(2, shiny::selectInput(ns("net_table_format_eigen"), tr("表格", "Table"),
                choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
              column(2, shiny::downloadButton(ns("net_download_table_eigen"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
                class = "btn-outline-info", width = "100%")),
              column(8)
            )
          ),

          # 6. 边连接求和 - 有图形
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'sumlinks'", ns = ns,
            h5(tr("边连接求和参数", "Edge Link Sum Parameters")),
            fluidRow(
              column(12, shiny::actionButton(ns("run_sumlinks"), tr("\U0001f3c1 开始分析", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
                  DT::dataTableOutput(ns("sumlinks_table"))
                )
              )
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("sumlinks_plot"), height = "600px"))
                )
              )
            ),
            hr(),
            h4(tr("图形参数", "Plot Parameters")),
            fluidRow(
              column(3, shiny::numericInput(ns("sumlinks_taxa_level_plot"), "taxa_level", value = 1, min = 1, max = 5, step = 1)),
              column(3, shinyWidgets::materialSwitch(ns("sumlinks_plot_pos"), tr("plot_pos (正相关)", "plot_pos (positive correlation)"), value = TRUE, status = "primary")),
              column(3, shiny::numericInput(ns("sumlinks_plot_num"), "plot_num", value = NULL, min = 1)),
              column(3, shiny::selectInput(ns("sumlinks_color_theme"), "color_values",
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"),
                selected = "Dark2"))
            ),
            fluidRow(
              column(3, shiny::selectInput(ns("sumlinks_method"), "method",
                choices = c("chorddiag", "circlize"), selected = "chorddiag"))
            ),
            hr(),
            h4(tr("图片保存与下载", "Save & Download Plot")),
            fluidRow(
              column(2, shiny::selectInput(ns("net_image_format_sl"), tr("格式", "Format"),
                choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
              column(1, shiny::numericInput(ns("net_save_width_sl"), tr("宽 (width)", "Width"), value = 12, min = 4, max = 20)),
              column(1, shiny::numericInput(ns("net_save_height_sl"), tr("高 (height)", "Height"), value = 10, min = 4, max = 15)),
              column(2, shiny::numericInput(ns("net_save_dpi_sl"), "DPI", value = 300, min = 72, max = 600, step = 72)),
              column(2, shiny::actionButton(ns("net_save_plot_btn_sl"), tr("\U0001f4e5保存图片", "\U0001f4e5Save Plot"),
                icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
            ),
            fluidRow(
              column(2, shiny::selectInput(ns("net_table_download_type_sl"), tr("下载类型", "Download Type"),
                choices = setNames(c("sumlinks"), c(tr("连接求和表", "Sum Links"))),
                selected = "sumlinks")),
              column(2, shiny::selectInput(ns("net_table_format_sl"), tr("表格", "Table"),
                choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
              column(2, shiny::downloadButton(ns("net_download_table_sl"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
                class = "btn-outline-info", width = "100%")),
              column(6)
            )
          ),

          # 7. 网络图
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'network'", ns = ns,
            h5(tr("网络图参数", "Network Plot Parameters")),
            fluidRow(
              column(12, shiny::actionButton(ns("run_network_plot"), tr("\U0001f3c1 开始分析", "\U0001f3c1 Run Analysis"),
                icon = icon("play"), class = "btn-success", width = "200px"))
            ),
            fluidRow(
              column(12,
                bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
                  shinycssloaders::withSpinner(shiny::plotOutput(ns("network_plot"), height = "600px"))
                )
              )
            ),
            hr(),
            h4(tr("图形参数", "Plot Parameters")),
            fluidRow(
              column(3, shiny::selectInput(ns("net_plot_layout"), "layout",
                choices = c("fr", "kk", "circle", "random", "lgl", "graphopt", "mds", "sugiyama"),
                selected = "fr")),
              column(3, shiny::numericInput(ns("net_plot_vertex_size"), "vertex_size", value = 4, min = 1, max = 20)),
              column(3, shiny::numericInput(ns("net_plot_edge_width"), "edge_width", value = 0.5, min = 0.1, max = 5, step = 0.1)),
              column(3, shinyWidgets::materialSwitch(ns("net_plot_label"), "show_label", value = FALSE, status = "info"))
            ),
            hr(),
            h4(tr("图片保存与下载", "Save & Download Plot")),
            fluidRow(
              column(2, shiny::selectInput(ns("net_image_format_np"), tr("格式", "Format"),
                choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
              column(1, shiny::numericInput(ns("net_save_width_np"), tr("宽 (width)", "Width"), value = 12, min = 4, max = 20)),
              column(1, shiny::numericInput(ns("net_save_height_np"), tr("高 (height)", "Height"), value = 10, min = 4, max = 15)),
              column(2, shiny::numericInput(ns("net_save_dpi_np"), "DPI", value = 300, min = 72, max = 600, step = 72)),
              column(2, shiny::actionButton(ns("net_save_plot_btn_np"), tr("\U0001f4e5保存图片", "\U0001f4e5Save Plot"),
                icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
            )
          ),

          # 8. 网络保存
          shiny::conditionalPanel(condition = "input.net_analysis_type == 'save'", ns = ns,
            h5(tr("网络保存参数", "Network Save Parameters")),
            fluidRow(
              column(4,
                shiny::radioButtons(ns("net_save_format_btn"), tr("保存格式", "Save Format"),
                  choices = c(tr("gexf 格式", "gexf"), tr("igraph RData", "igraph"), tr("trans_network RData", "trans_network")),
                  selected = "gexf", inline = TRUE)
              ),
              column(4,
                shiny::textInput(ns("net_save_filename"), tr("文件名", "Filename"),
                  value = "network", placeholder = "\u6587\u4ef6\u540d\u79f0...")
              ),
              column(4,
                shiny::selectInput(ns("net_save_dir_text"), tr("\U0001f4c1 保存位置", "Save Location"),
                  choices = c("Desktop", "Documents", "Home"), selected = "Desktop")
              )
            ),
            fluidRow(
              column(12,
                shiny::actionButton(ns("run_save_network"), tr("\U0001f4e5 保存网络", "\U0001f4e5 Save Network"),
                  icon = icon("save"), class = "btn-primary", width = "200px")
              )
            )
          )
        )
      )
    )
  )
}

mod_network_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(
      save_dir = NULL
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
      req(rv$last_plot)
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
        plot_obj <- rv$last_plot
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
        updateSelectInput(session, "net_taxa_level", choices = c("OTU", "Genus"), selected = "OTU")
        return()
      }

      mt <- rv$microtable
      if (!is.null(mt) && !is.null(mt$tax_table)) {
        tax_cols <- colnames(mt$tax_table)
        tax_ranks <- c("OTU", intersect(tax_cols, c("Genus", "Phylum", "Class", "Order", "Family")))
        if (length(tax_ranks) == 0) tax_ranks <- c("OTU", "Genus")
        updateSelectInput(session, "net_taxa_level", choices = tax_ranks, selected = "OTU")
      }
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

      network_method_val <- input$net_network_method
      dataset_name <- rv$microtable_name %||% "tmp_microtable"

      cor_method_val <- switch(network_method_val,
        "spearman" = "spearman",
        "sparcc" = "sparcc",
        "SpiecEasi" = NULL,
        "FlashWeave" = NULL,
        "beemStatic" = NULL
      )

      use_sparcc_method_val <- if (network_method_val == "sparcc") "SpiecEasi" else NULL

      nThreads_val <- if (network_method_val == "SpiecEasi") {
        floor(parallel::detectCores() / 2)
      } else {
        1
      }

      init_code <- paste0(
        "t_net <- microeco::trans_network$new(\n",
        "  dataset = ", dataset_name, ",\n",
        if (!is.null(cor_method_val)) paste0("  cor_method = \"", cor_method_val, "\",\n") else "",
        if (!is.null(use_sparcc_method_val)) paste0("  use_sparcc_method = \"", use_sparcc_method_val, "\",\n") else "",
        "  taxa_level = \"", input$net_taxa_level, "\",\n",
        "  filter_thres = ", input$net_filter_thres, "\n",
        ")\n"
      )

      add_taxa_name_val <- if (input$net_add_taxa_name) input$net_taxa_name_level else "Phylum"

      cal_network_code <- switch(network_method_val,
        "spearman" = paste0(
          "  network_method = \"COR\",\n",
          "  add_taxa_name = \"", add_taxa_name_val, "\",\n",
          "  delete_unlinked_nodes = ", input$net_delete_unlinked, ",\n",
          "  COR_p_thres = ", input$net_cor_p_thres, ",\n",
          "  COR_p_adjust = \"", input$net_cor_p_adjust, "\",\n",
          "  COR_return_padjust = ", input$net_cor_return_padjust, ",\n",
          "  COR_weight = ", input$net_cor_weight, ",\n",
          "  COR_cut = ", input$net_cor_cut, ",\n",
          "  COR_optimization = ", input$net_cor_optimization, ",\n",
          "  COR_optimization_low_high = c(", input$net_cor_opt_low, ", ", input$net_cor_opt_high, "),\n",
          "  COR_optimization_seq = ", input$net_cor_opt_seq, "\n"
        ),
        "sparcc" = paste0(
          "  network_method = \"COR\",\n",
          "  add_taxa_name = \"", add_taxa_name_val, "\",\n",
          "  delete_unlinked_nodes = ", input$net_delete_unlinked, ",\n",
          "  COR_p_thres = ", input$net_cor_p_thres, ",\n",
          "  COR_p_adjust = \"", input$net_cor_p_adjust, "\",\n",
          "  COR_return_padjust = ", input$net_cor_return_padjust, ",\n",
          "  COR_weight = ", input$net_cor_weight, ",\n",
          "  COR_cut = ", input$net_cor_cut, ",\n",
          "  COR_optimization = ", input$net_cor_optimization, ",\n",
          "  COR_optimization_low_high = c(", input$net_cor_opt_low, ", ", input$net_cor_opt_high, "),\n",
          "  COR_optimization_seq = ", input$net_cor_opt_seq, "\n"
        ),
        "SpiecEasi" = paste0(
          "  network_method = \"SpiecEasi\",\n",
          "  add_taxa_name = \"", add_taxa_name_val, "\",\n",
          "  delete_unlinked_nodes = ", input$net_delete_unlinked, ",\n",
          "  SpiecEasi_method = \"", input$net_spieceasi_method, "\"\n"
        ),
        "FlashWeave" = paste0(
          "  network_method = \"FlashWeave\",\n",
          "  add_taxa_name = \"", add_taxa_name_val, "\",\n",
          "  delete_unlinked_nodes = ", input$net_delete_unlinked, ",\n",
          "  FlashWeave_other_para = \"alpha=", input$net_flash_alpha,
          ",sensitive=", tolower(input$net_flash_sensitive),
          ",heterogeneous=", tolower(input$net_flash_heterogeneous), "\"\n"
        ),
        "beemStatic" = paste0(
          "  network_method = \"beemStatic\",\n",
          "  add_taxa_name = \"", add_taxa_name_val, "\",\n",
          "  delete_unlinked_nodes = ", input$net_delete_unlinked, "\n"
        )
      )

      network_method_code <- switch(network_method_val,
        "spearman" = "COR",
        "sparcc" = "COR",
        "SpiecEasi" = "SpiecEasi",
        "FlashWeave" = "FlashWeave",
        "beemStatic" = "beemStatic"
      )

      cal_code <- paste0(
        "t_net$cal_network(\n",
        cal_network_code,
        ")\n"
      )

      full_code <- paste0(init_code, cal_code)

      append_code(rv, full_code, "\u7f51\u7edc\u6784\u5efa - \u9884\u5199\u4ee3\u7801")

      result <- shiny::withProgress(
        message = "\u7f51\u7edc\u6784\u5efa\u4e2d...",
        value = 0,
        {
          shiny::incProgress(0.1, detail = "\u6b63\u5728\u521b\u5efa trans_network \u5bf9\u8c61...")

          mt <- rv$microtable

          t_net <- microeco::trans_network$new(
            dataset = mt,
            cor_method = cor_method_val,
            use_sparcc_method = use_sparcc_method_val,
            taxa_level = input$net_taxa_level,
            filter_thres = input$net_filter_thres,
            nThreads = nThreads_val
          )

          shiny::incProgress(0.3, detail = paste0("\u6b63\u5728\u8ba1\u7b97\u7f51\u7edc (", network_method_val, ")..."))

          cal_network_params <- switch(network_method_val,
            "spearman" = list(
              network_method = "COR",
              add_taxa_name = add_taxa_name_val,
              delete_unlinked_nodes = input$net_delete_unlinked,
              COR_p_thres = input$net_cor_p_thres,
              COR_p_adjust = input$net_cor_p_adjust,
              COR_return_padjust = input$net_cor_return_padjust,
              COR_weight = input$net_cor_weight,
              COR_cut = input$net_cor_cut,
              COR_optimization = input$net_cor_optimization,
              COR_optimization_low_high = c(input$net_cor_opt_low, input$net_cor_opt_high),
              COR_optimization_seq = input$net_cor_opt_seq
            ),
            "sparcc" = list(
              network_method = "COR",
              add_taxa_name = add_taxa_name_val,
              delete_unlinked_nodes = input$net_delete_unlinked,
              COR_p_thres = input$net_cor_p_thres,
              COR_p_adjust = input$net_cor_p_adjust,
              COR_return_padjust = input$net_cor_return_padjust,
              COR_weight = input$net_cor_weight,
              COR_cut = input$net_cor_cut,
              COR_optimization = input$net_cor_optimization,
              COR_optimization_low_high = c(input$net_cor_opt_low, input$net_cor_opt_high),
              COR_optimization_seq = input$net_cor_opt_seq
            ),
            "SpiecEasi" = list(
              network_method = "SpiecEasi",
              add_taxa_name = add_taxa_name_val,
              delete_unlinked_nodes = input$net_delete_unlinked,
              SpiecEasi_method = input$net_spieceasi_method
            ),
            "FlashWeave" = list(
              network_method = "FlashWeave",
              add_taxa_name = add_taxa_name_val,
              delete_unlinked_nodes = input$net_delete_unlinked,
              FlashWeave_other_para = paste0(
                "alpha=", input$net_flash_alpha,
                ",sensitive=", tolower(input$net_flash_sensitive),
                ",heterogeneous=", tolower(input$net_flash_heterogeneous)
              )
            ),
            "beemStatic" = list(
              network_method = "beemStatic",
              add_taxa_name = add_taxa_name_val,
              delete_unlinked_nodes = input$net_delete_unlinked
            )
          )

          do.call(t_net$cal_network, cal_network_params)

          shiny::incProgress(0.4, detail = "\u7f51\u7edc\u8ba1\u7b97\u5b8c\u6210\uff0c\u6b63\u5728\u6574\u7406\u7ed3\u679c...")

          list(success = TRUE, t_net = t_net, code = cal_code, init_code = init_code)
        }
      )

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      rv$network_obj <- result$t_net
      rv$network_code <- paste0(result$init_code, result$code)
      showNotification("\u7f51\u7edc\u6784\u5efa\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_module, {
      req(rv$network_obj)
      t_net <- rv$network_obj
      result <- tryCatch({
        t_net$cal_module(
          method = input$module_method,
          module_name_prefix = input$module_prefix
        )
        t_net$get_node_table()
        module_code <- paste0(
          "t_net$cal_module(\n",
          "  method = \"", input$module_method, "\",\n",
          "  module_name_prefix = \"", input$module_prefix, "\"\n",
          ")\n",
          "t_net$get_node_table()\n"
        )
        list(success = TRUE, data = t_net$res_node_table, code = module_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })
      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      local_rv$data_module <- result$data
      append_code(rv, paste(rv$network_code, result$code), "\u6a21\u5757\u5212\u5206")
      showNotification("\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_attr, {
      req(rv$network_obj)
      t_net <- rv$network_obj
      result <- tryCatch({
        t_net$cal_network_attr()
        attr_code <- "t_net$cal_network_attr()\n"
        list(success = TRUE, data = t_net$res_network_attr, code = attr_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })
      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      local_rv$data_attr <- result$data
      append_code(rv, paste(rv$network_code, result$code), "\u7f51\u7edc\u5c5e\u6027")
      showNotification("\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_nodes, {
      req(rv$network_obj)
      t_net <- rv$network_obj
      result <- tryCatch({
        t_net$get_node_table(node_roles = TRUE)
        nodes_code <- "t_net$get_node_table(node_roles = TRUE)\n"
        list(success = TRUE, data = t_net$res_node_table, code = nodes_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })
      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      local_rv$data_nodes <- result$data
      append_code(rv, paste(rv$network_code, result$code), "\u8282\u70b9\u5c5e\u6027")
      showNotification("\u8868\u683c\u8ba1\u7b97\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_nodes_plot, {
      req(rv$network_obj)
      req(local_rv$data_nodes)
      t_net <- rv$network_obj
      result <- tryCatch({
        use_type_val <- as.integer(input$roles_plot_type)
        color_values_val <- get_color_palette("Dark2", n = 12)
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
            plot_module_name = input$roles_plot_module_name,
            x_lim = c(0, 1)
          )
        } else {
          show_value_val <- input$roles_show_value
          if (is.null(show_value_val)) show_value_val <- c("z", "p")
          plot_obj <- t_net$plot_taxa_roles(
            use_type = use_type_val,
            use_level = input$roles_use_level,
            show_value = show_value_val,
            show_number = seq_len(input$roles_show_number),
            plot_color = input$roles_plot_color,
            plot_shape = input$roles_plot_shape,
            plot_size = input$roles_plot_size,
            color_values = color_values_val,
            shape_values = c(16, 17, 7, 8, 15, 18, 11, 10, 12, 13, 9, 3, 4, 0, 1, 2, 14),
            plot_module_name = input$roles_plot_module_name
          )
        }
        nodes_plot_code <- paste0(
          "p <- t_net$plot_taxa_roles(use_type = ", use_type_val, ")\n"
        )
        list(success = TRUE, plot = plot_obj, code = nodes_plot_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })
      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      local_rv$plot_nodes <- result$plot
      rv$last_plot <- result$plot
      append_code(rv, result$code, "\u8282\u70b9\u5c5e\u6027\u4f5c\u56fe")
      showNotification("\u4f5c\u56fe\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_edges, {
      req(rv$network_obj)
      t_net <- rv$network_obj
      result <- tryCatch({
        t_net$get_edge_table()
        edges_code <- "t_net$get_edge_table()\n"
        list(success = TRUE, data = t_net$res_edge_table, code = edges_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })
      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      local_rv$data_edges <- result$data
      append_code(rv, paste(rv$network_code, result$code), "\u8fb9\u5c5e\u6027")
      showNotification("\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_eigen, {
      req(rv$network_obj)
      t_net <- rv$network_obj
      result <- tryCatch({
        t_net$cal_eigen()
        eigen_code <- "t_net$cal_eigen()\n"
        list(success = TRUE, data = t_net$res_eigen, code = eigen_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })
      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      local_rv$data_eigen <- result$data
      append_code(rv, paste(rv$network_code, result$code), "\u7279\u5f81\u5206\u6790")
      showNotification("\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_sumlinks, {
      req(rv$network_obj)
      t_net <- rv$network_obj
      result <- tryCatch({
        t_net$cal_sum_links(taxa_level = input$sumlinks_taxa_level_plot)
        plot_obj <- t_net$plot_sum_links(
          plot_pos = input$sumlinks_plot_pos,
          plot_num = if (!is.null(input$sumlinks_plot_num) && input$sumlinks_plot_num > 0) input$sumlinks_plot_num else NULL,
          color_values = get_color_palette(input$sumlinks_color_theme, n = 8),
          method = input$sumlinks_method
        )
        sumlinks_code <- paste0(
          "t_net$cal_sum_links(taxa_level = ", input$sumlinks_taxa_level_plot, ")\n",
          "p <- t_net$plot_sum_links(\n",
          "  plot_pos = ", input$sumlinks_plot_pos, ",\n",
          "  color_values = get_color_palette(\"", input$sumlinks_color_theme, "\"),\n",
          "  method = \"", input$sumlinks_method, "\"\n",
          ")\n"
        )
        list(success = TRUE, data = t_net$res_sum_links_pos, plot = plot_obj, code = sumlinks_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })
      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      local_rv$data_sumlinks <- result$data
      local_rv$plot_sumlinks <- result$plot
      rv$last_plot <- result$plot
      append_code(rv, paste(rv$network_code, result$code), "\u8fb9\u8fde\u63a5\u6c42\u548c")
      showNotification("\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_save_network, {
      req(rv$network_obj)
      t_net <- rv$network_obj

      save_format <- input$net_save_format_btn
      filename <- input$net_save_filename
      if (is.null(filename) || nchar(trimws(filename)) == 0) {
        filename <- "network"
      }
      filename <- trimws(filename)

      save_dir <- switch(input$net_save_dir_text,
        "Desktop" = file.path(path.expand("~"), "Desktop"),
        "Documents" = file.path(path.expand("~"), "Documents"),
        "Home" = path.expand("~")
      )

      result <- tryCatch({
        if (save_format == "gexf") {
          full_path <- file.path(save_dir, paste0(filename, ".gexf"))
          t_net$save_network(file = full_path)
          list(success = TRUE, message = paste("Network saved to:", full_path))
        } else if (save_format == "igraph") {
          full_path <- file.path(save_dir, paste0(filename, "_igraph.RData"))
          save(t_net$res_network, file = full_path)
          list(success = TRUE, message = paste("igraph object saved to:", full_path))
        } else if (save_format == "trans_network") {
          full_path <- file.path(save_dir, paste0(filename, "_trans_network.RData"))
          save(t_net, file = full_path)
          list(success = TRUE, message = paste("trans_network object saved to:", full_path))
        }
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      showNotification(result$message, type = "message", duration = 5)
    })

    observeEvent(input$run_network_plot, {
      req(rv$network_obj)
      t_net <- rv$network_obj
      result <- tryCatch({
        plot_obj <- t_net$plot_network(
          layout = input$net_plot_layout,
          vertex_size = input$net_plot_vertex_size,
          edge_width = input$net_plot_edge_width,
          label = input$net_plot_label
        )
        network_plot_code <- paste0(
          "p <- t_net$plot_network(\n",
          "  layout = \"", input$net_plot_layout, "\",\n",
          "  vertex_size = ", input$net_plot_vertex_size, ",\n",
          "  edge_width = ", input$net_plot_edge_width, ",\n",
          "  label = ", input$net_plot_label, "\n",
          ")\n"
        )
        list(success = TRUE, plot = plot_obj, code = network_plot_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })
      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }
      local_rv$plot_network <- result$plot
      rv$last_plot <- result$plot
      append_code(rv, paste(rv$network_code, result$code), "\u7f51\u7edc\u56fe")
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$network_plot <- shiny::renderPlot({
      req(local_rv$plot_network)
      if (is(local_rv$plot_network, "ggplot")) {
        print(local_rv$plot_network)
      } else if (is(local_rv$plot_network, "igraph")) {
        igraph::plot.igraph(local_rv$plot_network)
      } else {
        print(local_rv$plot_network)
      }
    })

    output$module_table <- DT::renderDataTable({
      dt <- local_rv$data_module
      if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
        DT::datatable(as.data.frame(dt), options = list(scrollX = TRUE, pageLength = 5),
          rownames = TRUE, filter = "top")
      }
    })

    output$attr_table <- DT::renderDataTable({
      dt <- local_rv$data_attr
      if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
        DT::datatable(as.data.frame(dt), options = list(scrollX = TRUE, pageLength = 5),
          rownames = TRUE, filter = "top")
      }
    })

    output$nodes_table <- DT::renderDataTable({
      dt <- local_rv$data_nodes
      if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
        DT::datatable(as.data.frame(dt), options = list(scrollX = TRUE, pageLength = 5),
          rownames = TRUE, filter = "top")
      }
    })

    output$nodes_plot <- shiny::renderPlot({
      req(local_rv$plot_nodes)
      if (is(local_rv$plot_nodes, "ggplot")) {
        print(local_rv$plot_nodes)
      } else if (is(local_rv$plot_nodes, "igraph")) {
        igraph::plot.igraph(local_rv$plot_nodes)
      } else {
        print(local_rv$plot_nodes)
      }
    })

    output$edges_table <- DT::renderDataTable({
      dt <- local_rv$data_edges
      if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
        DT::datatable(as.data.frame(dt), options = list(scrollX = TRUE, pageLength = 5),
          rownames = TRUE, filter = "top")
      }
    })

    output$eigen_table <- DT::renderDataTable({
      dt <- local_rv$data_eigen
      if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
        DT::datatable(as.data.frame(dt), options = list(scrollX = TRUE, pageLength = 5),
          rownames = TRUE, filter = "top")
      }
    })

    output$sumlinks_table <- DT::renderDataTable({
      dt <- local_rv$data_sumlinks
      if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
        DT::datatable(as.data.frame(dt), options = list(scrollX = TRUE, pageLength = 5),
          rownames = TRUE, filter = "top")
      }
    })

    output$sumlinks_plot <- shiny::renderPlot({
      req(local_rv$plot_sumlinks)
      if (is(local_rv$plot_sumlinks, "ggplot")) {
        print(local_rv$plot_sumlinks)
      } else if (is(local_rv$plot_sumlinks, "igraph")) {
        igraph::plot.igraph(local_rv$plot_sumlinks)
      } else {
        print(local_rv$plot_sumlinks)
      }
    })

    output$net_download_table <- downloadHandler(
      filename = function() {
        type <- input$net_table_download_type
        ext <- ifelse(input$net_table_format == ",", ".csv", ".tsv")
        paste0("network_", type, ext)
      },
      content = function(file) {
        dt <- switch(input$net_table_download_type,
          "topo" = local_rv$data_attr,
          "nodes" = local_rv$data_nodes,
          "edges" = local_rv$data_edges,
          "module" = local_rv$data_module,
          "eigen" = local_rv$data_eigen,
          "sumlinks" = local_rv$data_sumlinks,
          NULL
        )
        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$net_table_format,
            row.names = TRUE, quote = TRUE)
        }
      }
    )

    output$net_download_table_sl <- downloadHandler(
      filename = function() {
        ext <- ifelse(input$net_table_format_sl == ",", ".csv", ".tsv")
        paste0("network_sumlinks", ext)
      },
      content = function(file) {
        dt <- local_rv$data_sumlinks
        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$net_table_format_sl,
            row.names = TRUE, quote = TRUE)
        }
      }
    )

    output$net_download_table_mod <- downloadHandler(
      filename = function() {
        ext <- ifelse(input$net_table_format_mod == ",", ".csv", ".tsv")
        paste0("network_module", ext)
      },
      content = function(file) {
        dt <- local_rv$data_module
        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$net_table_format_mod,
            row.names = TRUE, quote = TRUE)
        }
      }
    )

    output$net_download_table_attr <- downloadHandler(
      filename = function() {
        ext <- ifelse(input$net_table_format_attr == ",", ".csv", ".tsv")
        paste0("network_attr", ext)
      },
      content = function(file) {
        dt <- local_rv$data_attr
        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$net_table_format_attr,
            row.names = TRUE, quote = TRUE)
        }
      }
    )

    output$net_download_table_edges <- downloadHandler(
      filename = function() {
        ext <- ifelse(input$net_table_format_edges == ",", ".csv", ".tsv")
        paste0("network_edges", ext)
      },
      content = function(file) {
        dt <- local_rv$data_edges
        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$net_table_format_edges,
            row.names = TRUE, quote = TRUE)
        }
      }
    )

    output$net_download_table_eigen <- downloadHandler(
      filename = function() {
        ext <- ifelse(input$net_table_format_eigen == ",", ".csv", ".tsv")
        paste0("network_eigen", ext)
      },
      content = function(file) {
        dt <- local_rv$data_eigen
        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$net_table_format_eigen,
            row.names = TRUE, quote = TRUE)
        }
      }
    )
  })
}