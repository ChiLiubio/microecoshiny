#' @title Environmental Correlation Module UI
#' @description
#' Provides interface for environmental correlation analysis including correlation
#' heatmaps, Mantel tests, constrained ordination, and scatter plots.
#' Supports complete parameter options from trans_env class.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @importFrom shinyFiles getVolumes parseDirPath shinyDirButton shinyDirChoose
#' @keywords environmental correlation microbiome
#' @family advanced-analysis
mod_env_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(
      column(12, h2(tr("\U0001f517 环境因子关联 Environmental Correlation", "\U0001f517 Environmental Correlation")))
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
            column(3, shinyWidgets::pickerInput(
              ns("env_cols"), tr("选择环境因子列 (env_cols)", "env_cols"),
              choices = character(0), selected = character(0), multiple = TRUE,
              options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
            )),
            column(3, shiny::selectInput(ns("env_group"), tr("分组列 (group)", "group"), choices = character(0))),
            column(3, shiny::selectInput(ns("env_by_group"), "by_group", choices = character(0))),
            column(3, shinyWidgets::materialSwitch(ns("env_standardize"), tr("standardize (标准化)", "standardize"), value = FALSE, status = "primary"))
          ),
          fluidRow(
            column(3, shinyWidgets::materialSwitch(ns("env_character2numeric"), "character2numeric", value = FALSE, status = "info")),
            column(3, shinyWidgets::materialSwitch(ns("env_complete_na"), tr("complete_na (填充NA)", "complete_na"), value = FALSE, status = "warning")),
            column(6)
          ),
          hr(),
          h4(tr("分析方法", "Analysis Methods")),
          fluidRow(
            column(12, shiny::radioButtons(ns("analysis_type"), tr("分析类型", "Analysis Type"),
              choices = setNames(c("cor", "mantel", "ordination", "diff", "autocor", "scatterfit"),
                                 c(tr("相关性分析 (cal_cor)", "Correlation Analysis (cal_cor)"),
                                   tr("Mantel 检验 (cal_mantel)", "Mantel Test (cal_mantel)"),
                                   tr("约束排序 (cal_ordination)", "Constrained Ordination (cal_ordination)"),
                                   tr("差异检验 (cal_diff)", "Difference Test (cal_diff)"),
                                   tr("自相关 (cal_autocor)", "Autocorrelation (cal_autocor)"),
                                   tr("散点图 (plot_scatterfit)", "Scatter Plot (plot_scatterfit)"))),
              selected = "cor", inline = TRUE))
          ),
          shiny::conditionalPanel(condition = "input.analysis_type == 'cor'", ns = ns,
            h4(tr("cal_cor 参数", "cal_cor Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("cor_use_data"), "use_data",
                choices = c("Genus", "all", "other"), selected = "Genus")),
              column(2, shiny::selectInput(ns("cor_method"), "method",
                choices = c("pearson", "spearman", "kendall", "maaslin"), selected = "spearman")),
              column(2, shinyWidgets::materialSwitch(ns("cor_partial"), tr("partial (偏相关)", "partial"), value = FALSE, status = "info")),
              column(2, shiny::selectInput(ns("cor_p_adjust_method"), "p_adjust_method",
                choices = c("fdr", "holm", "bonferroni", "none", "BH", "BY"), selected = "fdr")),
              column(2, shiny::selectInput(ns("cor_p_adjust_type"), "p_adjust_type",
                choices = c("All", "Taxa", "Env"), selected = "All"))
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("cor_filter_thres"), "filter_thres", value = 0, min = 0, max = 1, step = 0.0001)),
              column(2, shinyWidgets::materialSwitch(ns("cor_filter_unknown"), "filter_unknown", value = TRUE, status = "success")),
              column(2, shiny::numericInput(ns("cor_use_taxa_num"), "use_taxa_num", value = NA, min = 1)),
              column(2, shinyWidgets::materialSwitch(ns("cor_taxa_name_full"), "taxa_name_full", value = TRUE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("cor_complete_cases"), "complete_cases", value = FALSE, status = "warning")),
              column(2)
            ),
            shiny::conditionalPanel(condition = "input.cor_use_data == 'other'", ns = ns,
              fluidRow(
                column(6, shiny::textInput(ns("cor_other_taxa"), tr("other_taxa (用逗号分隔)", "other_taxa (comma-separated)"),
                  placeholder = "e.g., taxon1,taxon2,taxon3"))
              )
            ),
            fluidRow(
              column(3, shiny::selectInput(ns("cor_group_use"), "group_use", choices = character(0))),
              column(3, shiny::selectInput(ns("cor_group_select"), "group_select", choices = character(0))),
              column(6)
            )
          ),
          shiny::conditionalPanel(condition = "input.analysis_type == 'cor'", ns = ns,
            h4(tr("plot_cor 参数", "plot_cor Parameters")),
            fluidRow(
              column(2, shiny::textInput(ns("cor_color_vector"), "color_vector",
                value = "#053061,white,#A50026")),
              column(2, shiny::numericInput(ns("cor_xtext_angle"), "xtext_angle", value = 30, min = 0, max = 90)),
              column(2, shiny::numericInput(ns("cor_xtext_size"), "xtext_size", value = 10, min = 6, max = 16)),
              column(2, shiny::textInput(ns("cor_xtext_color"), "xtext_color", value = "black")),
              column(2, shinyWidgets::materialSwitch(ns("cor_ytext_italic"), "ytext_italic", value = FALSE, status = "info")),
              column(2, shiny::selectInput(ns("cor_ytext_position"), "ytext_position",
                choices = c("right", "left"), selected = "right"))
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("cor_ytext_size"), "ytext_size", value = NA, min = 4, max = 16)),
              column(2, shiny::textInput(ns("cor_ytext_color"), "ytext_color", value = "black")),
              column(2, shiny::numericInput(ns("cor_sig_label_size"), "sig_label_size", value = 4, min = 2, max = 8)),
              column(2, shiny::selectInput(ns("cor_cluster_ggplot"), "cluster_ggplot",
                choices = c("none", "row", "col", "both"), selected = "none")),
              column(2, shiny::selectInput(ns("cor_trans"), "trans",
                choices = c("identity", "log10", "sqrt"), selected = "identity")),
              column(2)
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("cor_cluster_height_rows"), "cluster_height_rows", value = 0.2, min = 0.1, max = 0.5, step = 0.05)),
              column(2, shiny::numericInput(ns("cor_cluster_height_cols"), "cluster_height_cols", value = 0.2, min = 0.1, max = 0.5, step = 0.05)),
              column(2, shiny::textInput(ns("cor_na_value"), "na.value", value = "grey50")),
              column(2, shiny::textInput(ns("cor_legend_title"), "legend_title", value = "")),
              column(2, shiny::textInput(ns("cor_font_family"), "font_family", value = "", placeholder = "\u5b57\u4f53\u540d\u79f0")),
              column(2, shinyWidgets::materialSwitch(ns("cor_keep_prefix"), "keep_prefix", value = TRUE, status = "info"))
            )
          ),
          shiny::conditionalPanel(condition = "input.analysis_type == 'mantel'", ns = ns,
            h4(tr("cal_mantel 参数", "cal_mantel Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("mantel_use_measure"), "use_measure",
                choices = c("bray", "jaccard", "unifrac", "wunifrac"), selected = "bray")),
              column(2, shiny::selectInput(ns("mantel_method"), "method",
                choices = c("pearson", "spearman", "kendall"), selected = "pearson")),
              column(2, shiny::selectInput(ns("mantel_p_adjust_method"), "p_adjust_method",
                choices = c("fdr", "holm", "bonferroni", "none", "BH", "BY"), selected = "fdr")),
              column(2, shinyWidgets::materialSwitch(ns("mantel_partial"), "partial_mantel", value = FALSE, status = "warning")),
              column(2, shiny::selectInput(ns("mantel_by_group"), "by_group", choices = setNames("", tr("无", "None")))),
              column(2)
            )
          ),
          shiny::conditionalPanel(condition = "input.analysis_type == 'ordination'", ns = ns,
            h4(tr("cal_ordination 参数", "cal_ordination Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("ord_method"), "method",
                choices = c("RDA", "dbRDA", "CCA"), selected = "RDA")),
              column(2, shiny::selectInput(ns("ord_taxa_level"), "taxa_level",
                choices = c("Genus", "Phylum", "Class", "Order", "Family", "OTU"), selected = "Genus")),
              column(2, shiny::numericInput(ns("ord_taxa_filter_thres"), "taxa_filter_thres", value = 0, min = 0, max = 1, step = 0.001)),
              column(2, shinyWidgets::materialSwitch(ns("ord_feature_sel"), "feature_sel", value = FALSE, status = "warning")),
              column(2, shiny::selectInput(ns("ord_use_measure"), "use_measure", choices = character(0))),
              column(2)
            ),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("ord_show_taxa"), "show_taxa", value = TRUE, status = "info")),
              column(3, shiny::numericInput(ns("ord_show_taxa_num"), "show_taxa_num", value = 10, min = 1, max = 50)),
              column(3, shinyWidgets::materialSwitch(ns("ord_adjust_arrow"), "adjust_arrow_length", value = FALSE, status = "warning")),
              column(3)
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("ord_min_perc_env"), "min_perc_env", value = 0.1, min = 0.05, max = 0.5, step = 0.05)),
              column(2, shiny::numericInput(ns("ord_max_perc_env"), "max_perc_env", value = 0.8, min = 0.5, max = 1.5, step = 0.1)),
              column(2, shiny::numericInput(ns("ord_min_perc_tax"), "min_perc_tax", value = 0.1, min = 0.05, max = 0.5, step = 0.05)),
              column(2, shiny::numericInput(ns("ord_max_perc_tax"), "max_perc_tax", value = 0.8, min = 0.5, max = 1.5, step = 0.1)),
              column(4)
            ),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("ord_anova"), "\u663e\u8457\u6027\u68c0\u9a8c (anova)", value = FALSE, status = "primary")),
              column(3, shinyWidgets::materialSwitch(ns("ord_envfit"), "\u73af\u5883\u5411\u91cf\u62df\u5408 (envfit)", value = FALSE, status = "success")),
              column(6)
            )
          ),
          shiny::conditionalPanel(condition = "input.analysis_type == 'ordination'", ns = ns,
            h4("plot_ordination \u53c2\u6570"),
            fluidRow(
              column(2, shiny::selectInput(ns("ord_plot_color"), "plot_color", choices = character(0))),
              column(2, shiny::selectInput(ns("ord_plot_shape"), "plot_shape", choices = character(0))),
              column(2, shiny::selectInput(ns("ord_color_theme"), "color_theme",
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
              column(2, shiny::selectInput(ns("ord_plot_type"), "plot_type",
                choices = c("point", "point+ellipse", "point+chull", "point+centroid", "none"),
                selected = "point"))
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("ord_point_size"), "point_size", value = 3, min = 1, max = 10)),
              column(2, shiny::numericInput(ns("ord_point_alpha"), "point_alpha", value = 0.8, min = 0.1, max = 1, step = 0.1)),
              column(2, shiny::numericInput(ns("ord_point_second_size"), "point_second_size", value = NA, min = 1, max = 10)),
              column(2, shiny::numericInput(ns("ord_point_second_alpha"), "point_second_alpha", value = NA, min = 0.1, max = 1, step = 0.1)),
              column(2, shiny::textInput(ns("ord_point_second_color"), "point_second_color", value = "", placeholder = "\u9ed8\u8ba4\u540c\u4e3b\u8272")),
              column(2)
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("ord_env_text_size"), "env_text_size", value = 3.7, min = 2, max = 8)),
              column(2, shiny::numericInput(ns("ord_taxa_text_size"), "taxa_text_size", value = 3, min = 2, max = 8)),
              column(2, shinyWidgets::materialSwitch(ns("ord_taxa_text_italic"), "taxa_text_italic", value = TRUE, status = "info")),
              column(2, shinyWidgets::materialSwitch(ns("ord_taxa_text_prefix"), "taxa_text_prefix", value = FALSE, status = "warning")),
              column(2, shiny::selectInput(ns("ord_ellipse_type"), "ellipse_type",
                choices = c("t", "norm", "euclid"), selected = "t")),
              column(2, shiny::numericInput(ns("ord_ellipse_chull_alpha"), "ellipse_chull_alpha", value = 0.1, min = 0.05, max = 0.5, step = 0.05))
            ),
            fluidRow(
              column(2, shiny::textInput(ns("ord_env_text_color"), "env_text_color", value = "black")),
              column(2, shiny::textInput(ns("ord_env_arrow_color"), "env_arrow_color", value = "grey30")),
              column(2, shiny::textInput(ns("ord_taxa_text_color"), "taxa_text_color", value = "firebrick1")),
              column(2, shiny::textInput(ns("ord_taxa_arrow_color"), "taxa_arrow_color", value = "firebrick1")),
              column(2, shiny::numericInput(ns("ord_ellipse_level"), "ellipse_level", value = 0.9, min = 0.5, max = 0.99, step = 0.05)),
              column(2, shinyWidgets::materialSwitch(ns("ord_ellipse_chull_fill"), "ellipse_chull_fill", value = TRUE, status = "success"))
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("ord_env_nudge_x"), "env_nudge_x", value = NA, step = 0.01)),
              column(2, shiny::numericInput(ns("ord_env_nudge_y"), "env_nudge_y", value = NA, step = 0.01)),
              column(2, shiny::numericInput(ns("ord_taxa_nudge_x"), "taxa_nudge_x", value = NA, step = 0.01)),
              column(2, shiny::numericInput(ns("ord_taxa_nudge_y"), "taxa_nudge_y", value = NA, step = 0.01)),
              column(4)
            ),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("ord_point_second"), "point_second", value = FALSE, status = "warning")),
              column(3, shiny::selectInput(ns("ord_add_sample_label"), "add_sample_label (\u6807\u7b7e\u5217)", choices = c("\u65e0" = "", character(0)))),
              column(6)
            )
          ),
          shiny::conditionalPanel(condition = "input.analysis_type == 'diff'", ns = ns,
            h4(tr("cal_diff 参数", "cal_diff Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("diff_method"), "method",
                choices = c("KW", "KW_dunn", "wilcox", "t.test", "anova", "scheirerRayHare", "lm", "lme", "glmm"),
                selected = "KW")),
              column(2, shiny::selectInput(ns("diff_p_adjust_method"), "p_adjust_method",
                choices = c("fdr", "holm", "bonferroni", "none", "BH", "BY"), selected = "fdr")),
              column(2, shiny::numericInput(ns("diff_alpha_level"), "alpha", value = 0.05, min = 0.01, max = 0.1)),
              column(6)
            ),
            fluidRow(
              column(6, shiny::textInput(ns("diff_formula"), tr("formula (lm/lme/glmm)", "formula"),
                value = "~ Group"))
            )
          ),
          shiny::conditionalPanel(condition = "input.analysis_type == 'diff'", ns = ns,
            h4(tr("plot_diff 参数", "plot_diff Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("diff_measure"), tr("measure (环境因子)", "measure"), choices = character(0))),
              column(2, shiny::selectInput(ns("diff_plot_type"), "plot_type",
                choices = c("ggboxplot", "ggdotplot", "ggviolin", "ggstripchart", "ggerrorplot", "errorbar", "barerrorbar"),
                selected = "ggboxplot")),
              column(2, shiny::textInput(ns("diff_color_vector"), "color_values",
                value = "RColorBrewer::brewer.pal(8, 'Dark2')")),
              column(2, shiny::selectInput(ns("diff_add"), "add (\u53e0\u52a0\u5143\u7d20)",
                choices = c("none" = "", "jitter", "dotplot", "boxplot", "violin"), selected = "")),
              column(2, shinyWidgets::materialSwitch(ns("diff_add_sig"), "add_significance", value = TRUE, status = "primary")),
              column(2)
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("diff_xtext_angle"), "xtext_angle", value = 30, min = 0, max = 90)),
              column(2, shiny::numericInput(ns("diff_xtext_size"), "xtext_size", value = 13, min = 6, max = 20)),
              column(2, shiny::numericInput(ns("diff_ytitle_size"), "ytitle_size", value = 17, min = 10, max = 24)),
              column(2, shiny::numericInput(ns("diff_bar_width"), "bar_width", value = 0.9, min = 0.3, max = 1, step = 0.1)),
              column(2, shiny::selectInput(ns("diff_add_sig_label"), "add_sig_label",
                choices = c("Significance", "Letter", "P.adj"), selected = "Significance")),
              column(2)
            )
          ),
          shiny::conditionalPanel(condition = "input.analysis_type == 'autocor'", ns = ns,
            h4(tr("cal_autocor 参数", "cal_autocor Parameters")),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("autocor_ggpairs"), "ggpairs", value = TRUE, status = "primary")),
              column(3, shiny::numericInput(ns("autocor_alpha"), "alpha", value = 0.8, min = 0.1, max = 1, step = 0.1)),
              column(3, shiny::selectInput(ns("autocor_color_theme"), "color_theme",
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
              column(3)
            )
          ),
          shiny::conditionalPanel(condition = "input.analysis_type == 'scatterfit'", ns = ns,
            h4(tr("plot_scatterfit 参数", "plot_scatterfit Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("scatter_x"), tr("x (环境因子)", "x (env factor)"), choices = character(0))),
              column(2, shiny::selectInput(ns("scatter_y"), tr("y (环境因子)", "y (env factor)"), choices = character(0))),
              column(2, shiny::selectInput(ns("scatter_type"), "type",
                choices = c("cor", "lm"), selected = "cor")),
              column(2, shiny::selectInput(ns("scatter_cor_method"), "cor_method",
                choices = c("pearson", "spearman", "kendall"), selected = "pearson")),
              column(2, shiny::selectInput(ns("scatter_group"), "group", choices = character(0))),
              column(2)
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("scatter_point_size"), "point_size", value = 5, min = 1, max = 10)),
              column(2, shiny::numericInput(ns("scatter_point_alpha"), "point_alpha", value = 0.6, min = 0.1, max = 1, step = 0.1)),
              column(2, shiny::numericInput(ns("scatter_line_size"), "line_size", value = 0.8, min = 0.5, max = 2)),
              column(2, shiny::textInput(ns("scatter_line_color"), "line_color", value = "black")),
              column(2, shinyWidgets::materialSwitch(ns("scatter_line_se"), "line_se", value = TRUE, status = "info")),
              column(2, shiny::textInput(ns("scatter_line_se_color"), "line_se_color", value = "grey70"))
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("scatter_line_alpha"), "line_alpha", value = 0.5, min = 0.1, max = 1, step = 0.1)),
              column(2, shiny::textInput(ns("scatter_label_sep"), "label_sep", value = ";")),
              column(2, shiny::numericInput(ns("scatter_label_x"), "label.x", value = NA, step = 0.1)),
              column(2, shiny::numericInput(ns("scatter_label_y"), "label.y", value = NA, step = 0.1)),
              column(2, shinyWidgets::materialSwitch(ns("scatter_lm_equation"), "lm_equation", value = TRUE, status = "info")),
              column(2)
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("scatter_pvalue_trim"), "pvalue_trim", value = 4, min = 2, max = 6)),
              column(2, shiny::numericInput(ns("scatter_cor_coef_trim"), "cor_coef_trim", value = 3, min = 1, max = 5)),
              column(2, shiny::numericInput(ns("scatter_lm_fir_trim"), "lm_fir_trim", value = 2, min = 1, max = 4)),
              column(2, shiny::numericInput(ns("scatter_lm_sec_trim"), "lm_sec_trim", value = 2, min = 1, max = 4)),
              column(2, shiny::numericInput(ns("scatter_lm_squ_trim"), "lm_squ_trim", value = 2, min = 1, max = 4)),
              column(2)
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("scatter_label_x_npc"), "label.x.npc", value = 0.1, min = 0, max = 1, step = 0.05)),
              column(2, shiny::numericInput(ns("scatter_label_y_npc"), "label.y.npc", value = 0.95, min = 0, max = 1, step = 0.05)),
              column(2, shiny::textInput(ns("scatter_x_axis_title"), "x_axis_title", value = "")),
              column(2, shiny::textInput(ns("scatter_y_axis_title"), "y_axis_title", value = "")),
              column(4)
            )
          ),
          hr(),
          fluidRow(
            column(2, shiny::actionButton(ns("run_env"), tr("\U0001f4ca 执行", "\U0001f4ca Execute"), icon = icon("play"), class = "btn-primary", width = "100%")),
            column(2, shiny::selectInput(ns("image_format"), tr("格式", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("save_width"), tr("宽 (width)", "Width"), value = 10, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("save_height"), tr("高 (height)", "Height"), value = 7, min = 3, max = 15)),
            column(2, shiny::numericInput(ns("save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(2, shiny::actionButton(ns("save_plot_btn"), tr("\U0001f4e5保存图片", "\U0001f4e5Save Plot"), icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
          ),
          fluidRow(
            column(2, shiny::downloadButton(ns("download_table"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"), class = "btn-outline-info", width = "100%")),
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
          shinycssloaders::withSpinner(shiny::plotOutput(ns("env_plot"), height = "550px"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("env_table"))
        )
      )
    )
  )
}

mod_env_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(plot = NULL, data_result = NULL, save_dir = NULL, result_obj = NULL)

    volumes <- c(
      Home      = path.expand("~"),
      Desktop   = file.path(path.expand("~"), "Desktop"),
      Documents = file.path(path.expand("~"), "Documents"),
      get_volumes_safe()
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
      default_name <- paste0("env_", input$analysis_type, ".", input$image_format)
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
        fname <- paste0("env_", input$analysis_type)
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
        updatePickerInput(session, "env_cols", choices = character(0), selected = character(0))
        updateSelectInput(session, "env_group", choices = character(0))
        updateSelectInput(session, "env_by_group", choices = character(0))
        updateSelectInput(session, "cor_group_use", choices = character(0))
        updateSelectInput(session, "cor_group_select", choices = character(0))
        updateSelectInput(session, "ord_plot_color", choices = character(0))
        updateSelectInput(session, "ord_plot_shape", choices = character(0))
        updateSelectInput(session, "ord_add_sample_label", choices = character(0))
        updateSelectInput(session, "scatter_x", choices = character(0))
        updateSelectInput(session, "scatter_y", choices = character(0))
        updateSelectInput(session, "scatter_group", choices = character(0))
        updateSelectInput(session, "diff_measure", choices = character(0))
        return()
      }
      cols <- get_sample_cols(rv)
      env_cols <- cols
      updatePickerInput(session, "env_cols", choices = env_cols, selected = env_cols)
      updateSelectInput(session, "env_group", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "env_by_group", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "cor_group_use", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "cor_group_select", choices = character(0))
      updateSelectInput(session, "ord_plot_color", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "ord_plot_shape", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "ord_add_sample_label", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "scatter_x", choices = cols)
      updateSelectInput(session, "scatter_y", choices = cols)
      updateSelectInput(session, "scatter_group", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "diff_measure", choices = cols)
      updateSelectInput(session, "mantel_by_group", choices = c("\u65e0" = "", cols))
    })

    observe({
      if (!check_microtable(rv)) return()
      if (length(input$cor_group_use) == 0 || !isTRUE(nchar(input$cor_group_use) > 0)) {
        updateSelectInput(session, "cor_group_select", choices = character(0))
        return()
      }
      vals <- unique(rv$microtable$sample_table[, input$cor_group_use])
      updateSelectInput(session, "cor_group_select", choices = vals)
    })

    get_color_palette <- function(name, n = 8) {
      tryCatch({
        max_colors <- switch(name,
          "Dark2"    = 8,
          "Set1"     = 8,
          "Set2"     = 8,
          "Set3"     = 12,
          "Paired"   = 12,
          "Spectral" = 11,
          "Viridis"  = Inf,
          8
        )
        use_n <- min(n, max_colors)
        switch(name,
          "Dark2"    = RColorBrewer::brewer.pal(max(use_n, 3), "Dark2"),
          "Set1"     = RColorBrewer::brewer.pal(max(use_n, 3), "Set1"),
          "Set2"     = RColorBrewer::brewer.pal(max(use_n, 3), "Set2"),
          "Set3"     = RColorBrewer::brewer.pal(max(use_n, 3), "Set3"),
          "Paired"   = RColorBrewer::brewer.pal(max(use_n, 3), "Paired"),
          "Spectral" = RColorBrewer::brewer.pal(max(use_n, 3), "Spectral"),
          "Viridis"  = viridisLite::viridis(use_n),
          RColorBrewer::brewer.pal(max(use_n, 3), "Dark2")
        )
      }, error = function(e) RColorBrewer::brewer.pal(8, "Dark2"))
    }

    observeEvent(input$run_env, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      if (length(input$env_cols) == 0) {
        showNotification("\u8bf7\u9009\u62e9\u73af\u5883\u56e0\u5b50\u5217", type = "warning")
        return()
      }

      analysis <- input$analysis_type
      group_val <- if (nchar(input$env_group)) input$env_group else NULL
      by_group_val <- if (nchar(input$env_by_group)) input$env_by_group else NULL

      result <- tryCatch({
        mt <- rv$microtable
        dataset_name <- rv$microtable_name %||% "tmp_microtable"

        env_str <- paste0('c("', paste(input$env_cols, collapse = '", "'), '")')

        init_code <- paste0(
          "t_env <- microeco::trans_env$new(\n",
          "  dataset = ", dataset_name, ",\n",
          "  env_cols = ", env_str, ",\n",
          "  standardize = ", input$env_standardize, ",\n",
          "  character2numeric = ", input$env_character2numeric, ",\n",
          "  complete_na = ", input$env_complete_na, "\n",
          ")\n"
        )

        if (analysis == "cor") {
          use_taxa_num_val <- {
            val <- input$cor_use_taxa_num
            if (is.null(val) || is.na(val) || val <= 0) NULL else val
          }
          group_use_val <- if (nchar(input$cor_group_use)) input$cor_group_use else NULL
          group_select_val <- if (nchar(input$cor_group_select)) input$cor_group_select else NULL
          other_taxa_val <- if (input$cor_use_data == "other" && nchar(input$cor_other_taxa)) {
            trimws(strsplit(input$cor_other_taxa, ",\\s*")[[1]])
          } else NULL

          color_vec <- strsplit(input$cor_color_vector, ",\\s*")[[1]]
          if (length(color_vec) != 3) color_vec <- c("#053061", "white", "#A50026")

          t_env <- microeco::trans_env$new(
            dataset = mt,
            env_cols = input$env_cols,
            standardize = input$env_standardize,
            character2numeric = input$env_character2numeric,
            complete_na = input$env_complete_na
          )

          t_env$cal_cor(
            use_data = input$cor_use_data,
            method = input$cor_method,
            partial = input$cor_partial,
            p_adjust_method = input$cor_p_adjust_method,
            p_adjust_type = input$cor_p_adjust_type,
            filter_thres = input$cor_filter_thres,
            filter_unknown = input$cor_filter_unknown,
            use_taxa_num = use_taxa_num_val,
            taxa_name_full = input$cor_taxa_name_full,
            complete_cases = input$cor_complete_cases,
            group_use = group_use_val,
            group_select = group_select_val,
            other_taxa = other_taxa_val
          )

          p <- t_env$plot_cor(
            color_vector = color_vec,
            xtext_angle = input$cor_xtext_angle,
            xtext_size = input$cor_xtext_size,
            xtext_color = input$cor_xtext_color,
            ytext_size = if (is.na(input$cor_ytext_size)) NULL else input$cor_ytext_size,
            ytext_color = input$cor_ytext_color,
            ytext_italic = input$cor_ytext_italic,
            ytext_position = input$cor_ytext_position,
            sig_label_size = input$cor_sig_label_size,
            cluster_ggplot = input$cor_cluster_ggplot,
            cluster_height_rows = input$cor_cluster_height_rows,
            cluster_height_cols = input$cor_cluster_height_cols,
            na.value = input$cor_na_value,
            font_family = if (nchar(input$cor_font_family)) input$cor_font_family else NULL,
            trans = input$cor_trans,
            legend_title = if (nchar(input$cor_legend_title)) input$cor_legend_title else NULL,
            keep_prefix = input$cor_keep_prefix
          )

          cor_code <- paste0(
            "t_env$cal_cor(\n",
            "  use_data = \"", input$cor_use_data, "\",\n",
            "  method = \"", input$cor_method, "\",\n",
            "  partial = ", input$cor_partial, ",\n",
            "  p_adjust_method = \"", input$cor_p_adjust_method, "\",\n",
            "  p_adjust_type = \"", input$cor_p_adjust_type, "\",\n",
            "  filter_thres = ", input$cor_filter_thres, ",\n",
            "  filter_unknown = ", input$cor_filter_unknown, ",\n",
            if (!is.null(use_taxa_num_val)) paste0("  use_taxa_num = ", use_taxa_num_val, ",\n") else "",
            "  taxa_name_full = ", input$cor_taxa_name_full, ",\n",
            "  complete_cases = ", input$cor_complete_cases, "\n",
            ")\n",
            "p <- t_env$plot_cor(\n",
            "  color_vector = c(\"", paste(color_vec, collapse = "\", \""), "\"),\n",
            "  xtext_angle = ", input$cor_xtext_angle, ",\n",
            "  xtext_size = ", input$cor_xtext_size, ",\n",
            "  xtext_color = \"", input$cor_xtext_color, "\",\n",
            "  ytext_italic = ", input$cor_ytext_italic, ",\n",
            "  ytext_position = \"", input$cor_ytext_position, "\",\n",
            "  sig_label_size = ", input$cor_sig_label_size, ",\n",
            "  cluster_ggplot = \"", input$cor_cluster_ggplot, "\",\n",
            "  trans = \"", input$cor_trans, "\"\n",
            ")\n"
          )
          code <- paste0(init_code, "# \u76f8\u5173\u6027\u5206\u6790\n", cor_code)

          list(success = TRUE, plot = p, data_result = t_env$res_cor, result_obj = t_env, code = code)

        } else if (analysis == "mantel") {
          by_group_mantel <- if (nchar(input$mantel_by_group)) input$mantel_by_group else NULL

          t_env <- microeco::trans_env$new(
            dataset = mt,
            env_cols = input$env_cols,
            standardize = input$env_standardize,
            character2numeric = input$env_character2numeric,
            complete_na = input$env_complete_na
          )

          t_env$cal_mantel(
            use_measure = input$mantel_use_measure,
            method = input$mantel_method,
            p_adjust_method = input$mantel_p_adjust_method,
            partial_mantel = input$mantel_partial,
            by_group = by_group_mantel
          )

          mantel_code <- paste0(
            "t_env$cal_mantel(\n",
            "  use_measure = \"", input$mantel_use_measure, "\",\n",
            "  method = \"", input$mantel_method, "\",\n",
            "  p_adjust_method = \"", input$mantel_p_adjust_method, "\",\n",
            "  partial_mantel = ", input$mantel_partial, ",\n",
            "  by_group = ", if (!is.null(by_group_mantel)) paste0("\"", by_group_mantel, "\"") else "NULL", "\n",
            ")\n"
          )
          code <- paste0(init_code, "# Mantel \u68c0\u9a8c\n", mantel_code)

          list(success = TRUE, plot = NULL, data_result = t_env$res_mantel, result_obj = t_env, code = code)

        } else if (analysis == "ordination") {
          t_env <- microeco::trans_env$new(
            dataset = mt,
            env_cols = input$env_cols,
            standardize = input$env_standardize,
            character2numeric = input$env_character2numeric,
            complete_na = input$env_complete_na
          )

          t_env$cal_ordination(
            method = input$ord_method,
            taxa_level = input$ord_taxa_level,
            taxa_filter_thres = if (input$ord_taxa_filter_thres > 0) input$ord_taxa_filter_thres else NULL,
            feature_sel = input$ord_feature_sel,
            use_measure = if (nchar(input$ord_use_measure)) input$ord_use_measure else NULL
          )

          if (input$ord_show_taxa) {
            t_env$trans_ordination(
              show_taxa = input$ord_show_taxa_num,
              adjust_arrow_length = input$ord_adjust_arrow,
              min_perc_env = input$ord_min_perc_env,
              max_perc_env = input$ord_max_perc_env,
              min_perc_tax = input$ord_min_perc_tax,
              max_perc_tax = input$ord_max_perc_tax
            )
          } else {
            t_env$trans_ordination(
              show_taxa = NULL,
              adjust_arrow_length = input$ord_adjust_arrow,
              min_perc_env = input$ord_min_perc_env,
              max_perc_env = input$ord_max_perc_env,
              min_perc_tax = input$ord_min_perc_tax,
              max_perc_tax = input$ord_max_perc_tax
            )
          }

          if (input$ord_anova) {
            t_env$cal_ordination_anova()
          }

          if (input$ord_envfit) {
            t_env$cal_ordination_envfit()
          }

          plot_color_val <- if (nchar(input$ord_plot_color)) input$ord_plot_color else NULL
          plot_shape_val <- if (nchar(input$ord_plot_shape)) input$ord_plot_shape else NULL

          plot_type_val <- switch(input$ord_plot_type,
            "point" = "point",
            "point+ellipse" = c("point", "ellipse"),
            "point+chull" = c("point", "chull"),
            "point+centroid" = c("point", "centroid"),
            "none" = "none",
            "point"
          )
          
          ord_group_count <- if (!is.null(plot_color_val) && plot_color_val %in% colnames(mt$sample_table)) {
            length(unique(mt$sample_table[, plot_color_val]))
          } else 8

          p <- t_env$plot_ordination(
            plot_color = plot_color_val,
            plot_shape = plot_shape_val,
            color_values = get_color_palette(input$ord_color_theme, ord_group_count),
            plot_type = plot_type_val,
            point_size = input$ord_point_size,
            point_alpha = input$ord_point_alpha,
            point_second = input$ord_point_second,
            point_second_size = if (is.na(input$ord_point_second_size)) NULL else input$ord_point_second_size,
            point_second_alpha = if (is.na(input$ord_point_second_alpha)) NULL else input$ord_point_second_alpha,
            point_second_color = if (nchar(input$ord_point_second_color)) input$ord_point_second_color else NULL,
            env_text_size = input$ord_env_text_size,
            taxa_text_size = input$ord_taxa_text_size,
            taxa_text_italic = input$ord_taxa_text_italic,
            taxa_text_prefix = input$ord_taxa_text_prefix,
            env_text_color = input$ord_env_text_color,
            env_arrow_color = input$ord_env_arrow_color,
            taxa_text_color = input$ord_taxa_text_color,
            taxa_arrow_color = input$ord_taxa_arrow_color,
            ellipse_level = input$ord_ellipse_level,
            ellipse_type = input$ord_ellipse_type,
            ellipse_chull_fill = input$ord_ellipse_chull_fill,
            ellipse_chull_alpha = input$ord_ellipse_chull_alpha,
            env_nudge_x = if (is.na(input$ord_env_nudge_x)) NULL else input$ord_env_nudge_x,
            env_nudge_y = if (is.na(input$ord_env_nudge_y)) NULL else input$ord_env_nudge_y,
            taxa_nudge_x = if (is.na(input$ord_taxa_nudge_x)) NULL else input$ord_taxa_nudge_x,
            taxa_nudge_y = if (is.na(input$ord_taxa_nudge_y)) NULL else input$ord_taxa_nudge_y,
            add_sample_label = if (nchar(input$ord_add_sample_label)) input$ord_add_sample_label else NULL
          )

          ord_code <- paste0(
            "t_env$cal_ordination(\n",
            "  method = \"", input$ord_method, "\",\n",
            "  taxa_level = \"", input$ord_taxa_level, "\",\n",
            if (!is.null(input$ord_taxa_filter_thres) && input$ord_taxa_filter_thres > 0) paste0("  taxa_filter_thres = ", input$ord_taxa_filter_thres, ",\n") else "",
            "  feature_sel = ", input$ord_feature_sel, "\n",
            ")\n",
            "t_env$trans_ordination(\n",
            "  show_taxa = ", if (input$ord_show_taxa) input$ord_show_taxa_num else "NULL", ",\n",
            "  adjust_arrow_length = ", input$ord_adjust_arrow, "\n",
            ")\n",
            "p <- t_env$plot_ordination(\n",
            if (!is.null(plot_color_val)) paste0("  plot_color = \"", plot_color_val, "\",\n") else "  plot_color = NULL,\n",
            if (!is.null(plot_shape_val)) paste0("  plot_shape = \"", plot_shape_val, "\",\n") else "  plot_shape = NULL,\n",
            "  plot_type = c(\"", paste(plot_type_val, collapse = "\", \""), "\"),\n",
            "  point_size = ", input$ord_point_size, ",\n",
            "  point_alpha = ", input$ord_point_alpha, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# \u7ed6\u675f\u6392\u5e8f\n", ord_code)

          list(success = TRUE, plot = p, data_result = NULL, result_obj = t_env, code = code)

        } else if (analysis == "diff") {
          t_env <- microeco::trans_env$new(
            dataset = mt,
            env_cols = input$env_cols,
            standardize = input$env_standardize,
            character2numeric = input$env_character2numeric,
            complete_na = input$env_complete_na
          )

          formula_val <- if (nchar(input$diff_formula)) input$diff_formula else NULL

          t_env$cal_diff(
            group = group_val,
            by_group = by_group_val,
            method = input$diff_method,
            p_adjust_method = input$diff_p_adjust_method,
            alpha = input$diff_alpha_level,
            formula = formula_val
          )

          measure_val <- if (nchar(input$diff_measure)) input$diff_measure else NULL
          add_val <- if (nchar(input$diff_add)) input$diff_add else NULL

          p <- t_env$plot_diff(
            measure = measure_val,
            plot_type = input$diff_plot_type,
            color_values = RColorBrewer::brewer.pal(8, "Dark2"),
            add = add_val,
            add_sig = input$diff_add_sig,
            add_sig_label = input$diff_add_sig_label,
            xtext_angle = input$diff_xtext_angle,
            xtext_size = input$diff_xtext_size,
            ytitle_size = input$diff_ytitle_size,
            bar_width = input$diff_bar_width
          )

          diff_code <- paste0(
            "t_env$cal_diff(\n",
            "  group = ", if (!is.null(group_val)) paste0("\"", group_val, "\"") else "NULL", ",\n",
            "  by_group = ", if (!is.null(by_group_val)) paste0("\"", by_group_val, "\"") else "NULL", ",\n",
            "  method = \"", input$diff_method, "\",\n",
            "  p_adjust_method = \"", input$diff_p_adjust_method, "\",\n",
            "  alpha = ", input$diff_alpha_level,
            if (!is.null(formula_val)) paste0(",\n  formula = ", formula_val) else "",
            "\n)\n",
            "p <- t_env$plot_diff(\n",
            "  measure = ", if (!is.null(measure_val)) paste0("\"", measure_val, "\"") else "NULL", ",\n",
            "  plot_type = \"", input$diff_plot_type, "\",\n",
            "  add = ", if (!is.null(add_val)) paste0("\"", add_val, "\"") else "NULL", ",\n",
            "  add_sig = ", input$diff_add_sig, ",\n",
            "  xtext_angle = ", input$diff_xtext_angle, ",\n",
            "  xtext_size = ", input$diff_xtext_size, ",\n",
            "  ytitle_size = ", input$diff_ytitle_size, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# \u5dee\u5f0f\u68c0\u9a8c\n", diff_code)

          list(success = TRUE, plot = p, data_result = t_env$res_diff, result_obj = t_env, code = code)

        } else if (analysis == "autocor") {
          t_env <- microeco::trans_env$new(
            dataset = mt,
            env_cols = input$env_cols,
            standardize = input$env_standardize,
            character2numeric = input$env_character2numeric,
            complete_na = input$env_complete_na
          )

          p <- t_env$cal_autocor(
            group = group_val,
            ggpairs = input$autocor_ggpairs,
            color_values = get_color_palette(input$autocor_color_theme),
            alpha = input$autocor_alpha
          )

          autocor_code <- paste0(
            "t_env$cal_autocor(\n",
            "  group = ", if (!is.null(group_val)) paste0("\"", group_val, "\"") else "NULL", ",\n",
            "  ggpairs = ", input$autocor_ggpairs, ",\n",
            "  alpha = ", input$autocor_alpha, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# \u81ea\u76f8\u5173\n", autocor_code)

          list(success = TRUE, plot = p, data_result = NULL, result_obj = t_env, code = code)

        } else if (analysis == "scatterfit") {
          t_env <- microeco::trans_env$new(
            dataset = mt,
            env_cols = input$env_cols,
            standardize = input$env_standardize,
            character2numeric = input$env_character2numeric,
            complete_na = input$env_complete_na
          )

          scatter_x_val <- if (nchar(input$scatter_x)) input$scatter_x else 1
          scatter_y_val <- if (nchar(input$scatter_y)) input$scatter_y else 2
          scatter_group_val <- if (nchar(input$scatter_group)) input$scatter_group else NULL

          p <- t_env$plot_scatterfit(
            x = scatter_x_val,
            y = scatter_y_val,
            group = scatter_group_val,
            type = input$scatter_type,
            cor_method = input$scatter_cor_method,
            color_values = get_color_palette(input$autocor_color_theme),
            label_sep = input$scatter_label_sep,
            label.x.npc = input$scatter_label_x_npc,
            label.y.npc = input$scatter_label_y_npc,
            label.x = if (is.na(input$scatter_label_x)) NULL else input$scatter_label_x,
            label.y = if (is.na(input$scatter_label_y)) NULL else input$scatter_label_y,
            x_axis_title = if (nchar(input$scatter_x_axis_title)) input$scatter_x_axis_title else "",
            y_axis_title = if (nchar(input$scatter_y_axis_title)) input$scatter_y_axis_title else "",
            point_size = input$scatter_point_size,
            point_alpha = input$scatter_point_alpha,
            line_size = input$scatter_line_size,
            line_color = input$scatter_line_color,
            line_se = input$scatter_line_se,
            line_se_color = input$scatter_line_se_color,
            line_alpha = input$scatter_line_alpha,
            pvalue_trim = input$scatter_pvalue_trim,
            cor_coef_trim = input$scatter_cor_coef_trim,
            lm_equation = input$scatter_lm_equation,
            lm_fir_trim = input$scatter_lm_fir_trim,
            lm_sec_trim = input$scatter_lm_sec_trim,
            lm_squ_trim = input$scatter_lm_squ_trim
          )

          scatter_code <- paste0(
            "p <- t_env$plot_scatterfit(\n",
            "  x = \"", scatter_x_val, "\",\n",
            "  y = \"", scatter_y_val, "\",\n",
            if (!is.null(scatter_group_val)) paste0("  group = \"", scatter_group_val, "\",\n") else "  group = NULL,\n",
            "  type = \"", input$scatter_type, "\",\n",
            "  cor_method = \"", input$scatter_cor_method, "\",\n",
            "  point_size = ", input$scatter_point_size, ",\n",
            "  point_alpha = ", input$scatter_point_alpha, ",\n",
            "  line_size = ", input$scatter_line_size, ",\n",
            "  line_color = \"", input$scatter_line_color, "\",\n",
            "  line_se = ", input$scatter_line_se, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# \u6563\u70b9\u56fe\n", scatter_code)

          list(success = TRUE, plot = p, data_result = NULL, result_obj = t_env, code = code)

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

      append_code(rv, result$code, paste0("\u73af\u5883\u56e0\u5b50\u5173\u8054 - ", analysis))
      local_rv$plot <- result$plot
      local_rv$data_result <- result$data_result
      local_rv$result_obj <- result$result_obj
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$env_plot <- shiny::renderPlot({
      req(local_rv$plot)
      print(local_rv$plot)
    })

    output$env_table <- DT::renderDataTable({
      dt <- local_rv$data_result
      if (is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
      } else if (is(local_rv$result_obj, "trans_env") && !is.null(local_rv$result_obj$res_mantel)) {
        DT::datatable(local_rv$result_obj$res_mantel, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
      }
    })

    output$download_table <- downloadHandler(
      filename = function() paste0("env_", input$analysis_type, ifelse(input$table_format == ",", ".csv", ".tsv")),
      content = function(file) {
        dt <- local_rv$data_result
        if (is.null(dt)) {
          if (is(local_rv$result_obj, "trans_env") && !is.null(local_rv$result_obj$res_mantel)) {
            dt <- local_rv$result_obj$res_mantel
          }
        }
        if (!is.null(dt) && is.data.frame(dt)) {
          write.table(dt, file, sep = input$table_format, row.names = FALSE, quote = TRUE)
        }
      }
    )
  })
}