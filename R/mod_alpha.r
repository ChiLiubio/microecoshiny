#' @title Alpha Diversity Module UI
#' @description
#' Provides interface for alpha diversity analysis with complete parameter options.
#' Supports multiple diversity indices, group comparison, statistical testing, and visualization.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @importFrom shinyFiles getVolumes parseDirPath shinyDirButton shinyDirChoose
#' @keywords alpha diversity microbiome
#' @family community-analysis
mod_alpha_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh

  tt <- function(input_id, zh_tip, en_tip) {
    tip <- if (lang == "en") en_tip else zh_tip
    shiny::tags$script(HTML(paste0(
      'setTimeout(function() {',
      '  var lbl = $("label[for=', ns(input_id), ']");',
      '  if (lbl.length) { lbl.attr("title", "', tip, '").css("cursor", "help"); }',
      '}, 1000);'
    )))
  }

  tagList(
    fluidRow(
      column(12, h2(tr("\U0001f33f \u03b1\u591a\u6837\u6027 Alpha Diversity", "\U0001f33f Alpha Diversity")))
    ),

    # Part 1: Calculate Alpha Diversity
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4ca \u8ba1\u7b97\u03b1\u591a\u6837\u6027", "\U0001f4ca Calculate Alpha Diversity"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,
          fluidRow(
            column(6,
              shiny::selectizeInput(ns("measure"), tr("\u6307\u6807 (measure)", "measure"),
                choices = app_config$alpha_measures,
                selected = app_config$alpha_measures,
                multiple = TRUE, width = "100%",
                options = list(plugins = list("remove_button"))),
              tt("measure", "\u9009\u62e9\u8ba1\u7b97\u7684alpha\u591a\u6837\u6027\u6307\u6807\uff0c\u53ef\u591a\u9009", "Select alpha diversity indices to calculate, multiple allowed")
            ),
            column(4,
              shinyWidgets::materialSwitch(ns("pd_switch"), tr("PD (\u7cfb\u7edf\u53d1\u80b2\u591a\u6837\u6027)", "PD (Phylogenetic Diversity)"), value = FALSE, status = "info"),
              tt("pd_switch", "\u8ba1\u7b97\u7cfb\u7edf\u53d1\u80b2\u591a\u6837\u6027\uff0c\u9700\u8981\u67f1\u6811\u4fe1\u606f", "Calculate phylogenetic diversity, requires phylogenetic tree")
            )
          ),
          fluidRow(
            column(4, shiny::actionButton(ns("run_alpha"), tr("\U0001f4ca \u8ba1\u7b97", "\U0001f4ca Calculate"), icon = icon("play"), class = "btn-primary", width = "100%")),
            column(4, shiny::downloadButton(ns("download_alpha_table"), tr("\U0001f4e5\u4fdd\u5b58\u03b1\u591a\u6837\u6027\u8868\u683c", "\U0001f4e5Save Alpha Table"), class = "btn-outline-info", width = "100%"))
          )
        )
      )
    ),

    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca \u03b1\u591a\u6837\u6027\u7ed3\u679c", "\U0001f4ca Alpha Diversity Results"), status = "info", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("alpha_table"))
        )
      )
    ),

    # Part 2: Difference Testing (cal_diff)
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4ca \u5dee\u5f02\u68c0\u9a8c (cal_diff)", "\U0001f4ca Difference Testing (cal_diff)"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,

          h4(tr("\u68c0\u9a8c\u65b9\u6cd5", "Test Method")),
          tags$div(
            title = tr("KW\uff1aKruskal-Wallis\u975e\u53c2\u6570\u68c0\u9a8c\uff0c\u9002\u4e8e\u591a\u7ec4\u6bd4\u8f83 |\nKW_dunn\uff1aDunn\u4e8b\u540e\u68c0\u9a8c\uff0c\u7528\u4e8eKW\u540e\u7684\u4e24\u4e24\u6bd4\u8f83 |\nwilcox\uff1aWilcoxon\u6839\u548c\u68c0\u9a8c\uff0c\u4e24\u7ec4\u6bd4\u8f83\uff0c\u652f\u6301\u914d\u5bf9 |\nt.test\uff1a\u91ca\u653e\u6216\u914d\u5bf9\u6837\u672ct\u68c0\u9a8c |\nscheirerRayHare\uff1a\u975e\u53c2\u6570\u591a\u56e0\u7d20\u65b9\u5dee\u5206\u6790 |\none-way anova\uff1a\u5355\u56e0\u7d20\u65b9\u5dee\u5206\u6790\uff0c\u9700\u8981\u6b63\u6001\u5206\u5e03 |\nmulti-anova\uff1a\u591a\u56e0\u7d20\u65b9\u5dee\u5206\u6790\uff0c\u4f7f\u7528formula |\nlm\uff1a\u7ebf\u6027\u56de\u5f52 |\nlme\uff1a\u7ebf\u6027\u6df7\u5408\u6548\u5e94\u6a21\u578b", "KW: Kruskal-Wallis non-parametric test for multiple groups |\nKW_dunn: Dunn post-hoc test for pairwise comparisons after KW |\nwilcox: Wilcoxon rank sum test for two groups, supports paired |\nt.test: Independent or paired t-test |\nscheirerRayHare: Non-parametric two-way ANOVA |\none-way anova: Parametric ANOVA, requires normality |\nmulti-anova: Multi-factor ANOVA using formula |\nlm: Linear regression |\nlme: Linear mixed effects model"),
            style = "cursor: help; display: inline-block; margin-left: 10px;",
            icon("info-circle")
          ),
          fluidRow(
            column(12, shiny::radioButtons(ns("diff_method"), tr("\u65b9\u6cd5 (method)", "method"),
              choices = c("KW", "KW_dunn", "wilcox", "t.test", "scheirerRayHare", "one-way anova", "multi-anova", "lm", "lme"),
              selected = "wilcox", inline = TRUE))
          ),

          hr(),
          h4(tr("\u68c0\u9a8c\u6307\u6807", "Test Measure")),
          fluidRow(
            column(6,
              shiny::selectizeInput(ns("diff_measure"), tr("\u68c0\u9a8c\u6307\u6807", "Test measure"),
                choices = app_config$alpha_measures,
                selected = "Shannon",
                multiple = TRUE, width = "100%",
                options = list(plugins = list("remove_button"))),
              tt("diff_measure", "\u9009\u62e9\u8fdb\u884c\u5dee\u5f02\u68c0\u9a8c\u7684alpha\u6307\u6807\uff0c\u53ef\u591a\u9009", "Select alpha indices for difference testing, multiple allowed")
            )
          ),

          hr(),
          shiny::conditionalPanel(condition = "['KW','wilcox','t.test','KW_dunn','anova','scheirerRayHare'].includes(input.diff_method)", ns = ns,
            h4(tr("\u5206\u7ec4\u53c2\u6570", "Group Parameters")),
            fluidRow(
              column(4, shiny::uiOutput(ns("group_col_ui"))),
              column(4, shiny::uiOutput(ns("by_group_ui"))),
              column(4, shiny::uiOutput(ns("by_ID_ui")))
            ),
            hr()
          ),

          shiny::conditionalPanel(condition = "['KW','wilcox','t.test','KW_dunn','anova'].includes(input.diff_method)", ns = ns,
            h4(tr("\u7edf\u8ba1\u53c2\u6570", "Statistical Parameters")),
            fluidRow(
              column(3,
                shiny::selectInput(ns("p_adjust_method"), "p_adjust_method",
                  choices = c("none", "bonferroni", "holm", "BH", "BY", "hochberg"), selected = "none"),
                tt("p_adjust_method", "p\u503c\u591a\u6b21\u6821\u6b63\u65b9\u6cd5\uff0c\u63a8\u8350\u4f7f\u7528none\u6216holm\uff0cKW_dunn\u65b9\u6cd5\u4f1a\u5c06fdr\u81ea\u52a8\u8f6c\u4e3aholm", "p-value multiple testing correction method, recommend none or holm, KW_dunn will convert fdr to holm automatically")
              )
            ),
            hr()
          ),

          shiny::conditionalPanel(condition = "['KW_dunn','anova'].includes(input.diff_method)", ns = ns,
            h4(tr("\u663e\u8457\u6027\u6c34\u5e73", "Significance Level")),
            fluidRow(
              column(3,
                shiny::numericInput(ns("alpha_level"), "alpha", value = 0.05, min = 0.001, max = 0.2, step = 0.01),
                tt("alpha_level", "\u663e\u8457\u6027\u6c34\u5e73\uff0c\u7528\u4e8e\u4e8b\u540e\u6bd4\u8f83\uff0c\u9ed8\u8ba40.05", "Significance level for post-hoc comparison, default 0.05")
              )
            ),
            hr()
          ),

          h4(tr("\u65b9\u6cd5\u7279\u5b9a\u53c2\u6570", "Method-specific Parameters")),

          shiny::conditionalPanel(condition = "input.diff_method == 'KW_dunn'", ns = ns,
            fluidRow(
              column(4,
                shinyWidgets::materialSwitch(ns("KW_dunn_letter"), "KW_dunn_letter", value = TRUE, status = "info"),
                tt("KW_dunn_letter", "\u663e\u793a\u5b57\u6bcd\u6807\u8bb0\u800c\u975e\u661f\u53f7", "Show letter annotations instead of significance symbols")
              )
            )
          ),

          shiny::conditionalPanel(condition = "input.diff_method == 'one-way anova'", ns = ns,
            fluidRow(
              column(4,
                shiny::selectInput(ns("anova_post_test"), "anova_post_test",
                  choices = c("duncan.test", "LSD.test", "HSD.test"), selected = "duncan.test"),
                tt("anova_post_test", "ANOVA\u4e8b\u540e\u68c0\u9a8c\uff0c\u7528\u4e8e\u5224\u65ad\u5177\u4f53\u7ec4\u95f4\u5dee\u5f02", "Post-hoc test for ANOVA to determine specific group differences")
              ),
              column(4,
                shinyWidgets::materialSwitch(ns("anova_varequal_test"), "anova_varequal_test", value = FALSE, status = "info"),
                tt("anova_varequal_test", "\u68c0\u6d4b\u65b9\u5dee\u9f50\u6027\u540e\u518d\u8fdb\u884cANOVA", "Test for homogeneity of variances before ANOVA")
              )
            )
          ),

          shiny::conditionalPanel(condition = "['lm','lme','multi-anova'].includes(input.diff_method)", ns = ns,
            fluidRow(
              column(8,
                shiny::textInput(ns("diff_formula"), tr("\u516c\u5f0f\u53c2\u6570 formula", "Formula"),
                  value = "~ Group", placeholder = tr("\u4f8b\u5982: ~ Group + Time", "e.g.: ~ Group + Time")),
                tt("diff_formula", "\u56de\u5f52\u516c\u5f0f\uff0c~ Group \u8868\u793a\u5206\u7ec4\u4f5c\u4e3a\u81ea\u53d8\u91cf", "Regression formula, ~ Group means group as independent variable")
              )
            )
          ),

          hr(),
          fluidRow(
            column(4, shiny::actionButton(ns("run_diff"), tr("\U0001f4ca \u8ba1\u7b97\u5dee\u5f02\u68c0\u9a8c", "\U0001f4ca Calculate Difference"), icon = icon("play"), class = "btn-primary", width = "100%")),
            column(4, shiny::downloadButton(ns("download_diff_table"), tr("\U0001f4e5\u4fdd\u5b58\u5dee\u5f02\u7ed3\u679c", "\U0001f4e5Save Diff Results"), class = "btn-outline-info", width = "100%"))
          )
        )
      )
    ),

    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca \u5dee\u5f02\u68c0\u9a8c\u7ed3\u679c", "\U0001f4ca Difference Test Results"), status = "info", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("alpha_diff_table"))
        )
      )
    ),

    # Part 3: Plot Settings + Plot Area
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4cb \u56fe\u8868\u8bbe\u7f6e", "\U0001f4cb Plot Settings"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,

          h4(tr("\u56fe\u578b (plot_type)", "plot_type")),
          fluidRow(
            column(12, shiny::radioButtons(ns("plot_type"), tr("图型 (plot_type)", "plot_type"),
              choices = c("ggboxplot", "ggdotplot", "ggviolin", "ggstripchart", "ggerrorplot", "errorbar", "barerrorbar", "heatmap", "coefplot"),
              selected = "ggboxplot", inline = TRUE))
          ),

          hr(),
          h4(tr("\u6392\u5e8f\u53c2\u6570", "Ordering Parameters")),
          fluidRow(
            column(6,
              shiny::textInput(ns("order_x"), "order_x", value = "",
                placeholder = tr("\u4f8b\u5982: IW,CW,TW (\u6309\u987a\u5e8f\u6392\u5217)", "e.g.: IW,CW,TW (order by sequence)")),
              tt("order_x", "\u56fe\u5f62x\u8f74\u987a\u5e8f\uff0c\u7528\u9017\u53f7\u5206\u9694", "X-axis order, separated by comma")
            ),
            column(4,
              shinyWidgets::materialSwitch(ns("order_x_mean"), "order_x_mean", value = FALSE, status = "info"),
              tt("order_x_mean", "\u6309\u7ec4\u5185\u5e73\u5747\u503c\u6392\u5e8f", "Order by mean value within group")
            )
          ),

          hr(),
          shiny::conditionalPanel(condition = "input.plot_type.startsWith('gg')", ns = ns,
            h4(tr("ggpubr \u53c2\u6570", "ggpubr Parameters")),
            fluidRow(
              column(2,
                shiny::selectInput(ns("color_theme"), tr("\u989c\u8272 (color_theme)", "color_theme"),
                  choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2"),
                tt("color_theme", "\u989c\u8272\u7279\u8272\u65b9\u6848", "Color palette theme")
              ),
              column(2,
                shiny::selectInput(ns("add"), "add",
                  choices = c("none", "jitter", "boxplot", "violin", "dot", "mean", "mean_se", "mean_sd", "median", "median_iqr"), selected = "none"),
                tt("add", "\u56fe\u5f62\u989c\u8272\u65b9\u6848", "Additional plot layer")
              ),
              column(2,
                shiny::numericInput(ns("point_size"), "point_size", value = 3, min = 1, max = 10),
                tt("point_size", "\u70b9\u5927\u5c0f", "Point size")
              ),
              column(2,
                shiny::numericInput(ns("point_alpha"), "point_alpha", value = 0.8, min = 0.1, max = 1, step = 0.1),
                tt("point_alpha", "\u70b9\u900f\u660e\u5ea6", "Point transparency")
              )
            )
          ),

          shiny::conditionalPanel(condition = "input.plot_type == 'errorbar' || input.plot_type == 'barerrorbar'", ns = ns,
            h4(tr("errorbar \u53c2\u6570", "errorbar Parameters")),
            fluidRow(
              column(2,
                shiny::selectInput(ns("color_theme_errorbar"), tr("\u989c\u8272", "Color"),
                  choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2"),
                tt("color_theme_errorbar", "\u9519\u8bef\u6811\u989c\u8272\u65b9\u6848", "Error bar color theme")
              ),
              column(2,
                shinyWidgets::materialSwitch(ns("plot_SE"), "plot_SE (Mean\u00b1SE)", value = TRUE, status = "info"),
                tt("plot_SE", "\u663e\u793a\u6807\u51c6\u8bef\u5dee\u800c\u975e\u65e0\u5bfc\u8bba", "Show standard error instead of standard deviation")
              ),
              column(2,
                shinyWidgets::materialSwitch(ns("errorbar_color_black"), "errorbar_color_black", value = FALSE, status = "warning"),
                tt("errorbar_color_black", "\u5c06\u9519\u8bef\u6811\u989c\u8272\u8bbe\u4e3a\u9ed1\u8272", "Set error bar color to black")
              ),
              column(2,
                shinyWidgets::materialSwitch(ns("errorbar_addpoint"), "errorbar_addpoint", value = TRUE, status = "info"),
                tt("errorbar_addpoint", "\u5728\u9519\u8bef\u6811\u4e0a\u6dfb\u52a0\u70b9", "Add points on error bars")
              ),
              column(2,
                shiny::numericInput(ns("bar_width"), "bar_width", value = 0.9, min = 0.5, max = 1, step = 0.1),
                tt("bar_width", "\u67f1\u72b6\u5bbd\u5ea6", "Bar width")
              ),
              column(2,
                shiny::numericInput(ns("bar_alpha"), "bar_alpha", value = 0.8, min = 0.1, max = 1, step = 0.1),
                tt("bar_alpha", "\u67f1\u72b6\u900f\u660e\u5ea6", "Bar transparency")
              )
            ),
            fluidRow(
              column(2,
                shiny::numericInput(ns("dodge_width"), "dodge_width", value = 0.9, min = 0.5, max = 1, step = 0.1),
                tt("dodge_width", "\u67f1\u72b6\u95f4\u8ddd", "Bar spacing distance")
              ),
              column(2,
                shiny::numericInput(ns("errorbar_size"), "errorbar_size", value = 1, min = 0.5, max = 3, step = 0.1),
                tt("errorbar_size", "\u9519\u8bef\u6811\u7ebf\u6761\u7c97\u7ec6", "Error bar line thickness")
              ),
              column(2,
                shiny::numericInput(ns("errorbar_width"), "errorbar_width", value = 0.2, min = 0.1, max = 0.5, step = 0.05),
                tt("errorbar_width", "\u9519\u8bef\u6811\u6a1a\u5934\u5bbd\u5ea6", "Error bar cap width")
              ),
              column(2,
                shinyWidgets::materialSwitch(ns("add_line"), "add_line", value = FALSE, status = "warning"),
                tt("add_line", "\u6dfb\u52a0\u8fde\u63a5\u7ebf", "Add connecting lines")
              ),
              column(2,
                shiny::numericInput(ns("line_size"), "line_size", value = 0.8, min = 0.5, max = 2, step = 0.1),
                tt("line_size", "\u8fde\u63a5\u7ebf\u7c97\u7ec6", "Line thickness")
              ),
              column(2,
                shiny::selectInput(ns("line_type"), "line_type",
                  choices = c("1-solid" = 1, "2-dashed" = 2, "3-dotted" = 3, "4-dotdash" = 4), selected = 2),
                tt("line_type", "\u8fde\u63a5\u7ebf\u7c97\u7ec6\u7c97\u7ec6\u7c97\u7ec6\u7c97\u7ec6\u7c97\u7ec6", "Line type")
              )
            ),
            fluidRow(
              column(2,
                shiny::textInput(ns("line_color"), "line_color", value = "grey50"),
                tt("line_color", "\u8fde\u63a5\u7ebf\u989c\u8272", "Line color")
              ),
              column(2,
                shiny::numericInput(ns("line_alpha"), "line_alpha", value = 0.5, min = 0.1, max = 1, step = 0.1),
                tt("line_alpha", "\u8fde\u63a5\u7ebf\u900f\u660e\u5ea6", "Line transparency")
              )
            )
          ),

          hr(),
          h4(tr("\u663e\u8457\u6027\u6807\u6ce8 (add_sig)", "Significance annotation (add_sig)")),
          fluidRow(
            column(2,
              shinyWidgets::materialSwitch(ns("add_sig"), "add_sig", value = TRUE, status = "primary"),
              tt("add_sig", "\u663e\u793a\u663e\u8457\u6027\u6807\u6ce8", "Show significance annotations")
            ),
            column(2,
              shiny::selectInput(ns("add_sig_label"), "add_sig_label",
                choices = c("Significance", "P.adj", "P.unadj"), selected = "Significance"),
              tt("add_sig_label", "\u6807\u6ce8\u5185\u5bbd\u7c7b\u578b", "Annotation label type")
            ),
            column(2,
              shiny::numericInput(ns("add_sig_text_size"), "add_sig_text_size", value = 3.88, min = 1, max = 10, step = 0.5),
              tt("add_sig_text_size", "\u6807\u6ce8\u6587\u5b57\u5927\u5c0f", "Annotation text size")
            ),
            column(2,
              shiny::numericInput(ns("add_sig_label_num_dec"), "add_sig_label_num_dec", value = 4, min = 1, max = 10),
              tt("add_sig_label_num_dec", "p\u503c\u5c0f\u6570\u4f4d\u6570", "Number of decimal places for p-value")
            ),
            column(2,
              shiny::numericInput(ns("y_start"), "y_start", value = 0.1, min = 0, max = 1, step = 0.05),
              tt("y_start", "\u6807\u6ce8\u8d77\u59cby\u8f74\u4f4d\u7f6e", "Starting y-axis position for annotations")
            ),
            column(2,
              shiny::numericInput(ns("y_increase"), "y_increase", value = 0.05, min = 0, max = 0.5, step = 0.01),
              tt("y_increase", "\u6bcf\u6b21\u6807\u6ce8y\u8f74\u589e\u91cf", "Y-axis increment for each annotation")
            )
          ),

          hr(),
          h4(tr("\u5e03\u5c40\u5c42\u9762\u53c2\u6570", "Layout Parameters")),
          fluidRow(
            column(2,
              shiny::numericInput(ns("xtext_angle"), "xtext_angle", value = 30, min = 0, max = 90, step = 15),
              tt("xtext_angle", "x\u8f74\u6587\u5b57\u65cb\u8f6c\u89d2\u5ea6", "X-axis text rotation angle")
            ),
            column(2,
              shiny::numericInput(ns("xtext_size"), "xtext_size", value = 13, min = 8, max = 20),
              tt("xtext_size", "x\u8f74\u6587\u5b57\u5927\u5c0f", "X-axis text size")
            ),
            column(2,
              shiny::numericInput(ns("ytitle_size"), "ytitle_size", value = 17, min = 10, max = 24),
              tt("ytitle_size", "y\u8f74\u6807\u9898\u5927\u5c0f", "Y-axis title size")
            )
          ),

          hr(),
          shiny::conditionalPanel(condition = "input.plot_type == 'heatmap' || input.plot_type == 'coefplot'", ns = ns,
            h4(tr("热图/系数图参数 (heatmap/coefplot)", "Heatmap/Coefplot Parameters")),
            fluidRow(
              column(2,
                shiny::selectInput(ns("heatmap_cell"), "heatmap_cell",
                  choices = c("P.unadj", "P.adj", "Significance"), selected = "P.unadj"),
                tt("heatmap_cell", "\u70ed\u56fe\u5355\u5143\u5185\u5bbd", "Heatmap cell content")
              ),
              column(2,
                shiny::selectInput(ns("heatmap_sig"), "heatmap_sig",
                  choices = c("Significance", "P.adj"), selected = "Significance"),
                tt("heatmap_sig", "\u70ed\u56fe\u663e\u8457\u6027\u6807\u8bb0", "Heatmap significance marker")
              ),
              column(2,
                shiny::selectInput(ns("heatmap_x"), "heatmap_x",
                  choices = setNames(c("Factors", "Measure", "Method"), c(tr("\u56e0\u5b50", "Factors"), tr("\u6307\u6807", "Measure"), tr("\u65b9\u6cd5", "Method"))), selected = "Factors"),
                tt("heatmap_x", "\u70ed\u56fex\u8f74\u53d8\u91cf", "Heatmap x-axis variable")
              ),
              column(2,
                shiny::selectInput(ns("heatmap_y"), "heatmap_y",
                  choices = setNames(c("Measure", "Factors", "Method"), c(tr("\u6307\u6807", "Measure"), tr("\u56e0\u5b50", "Factors"), tr("\u65b9\u6cd5", "Method"))), selected = "Measure"),
                tt("heatmap_y", "\u70ed\u56 Fey\u8f74\u53d8\u91cf", "Heatmap y-axis variable")
              ),
              column(2,
                shiny::textInput(ns("heatmap_lab_fill"), "heatmap_lab_fill", value = "P value"),
                tt("heatmap_lab_fill", "\u70ed\u56fe\u586b\u5149\u6807\u7b7e", "Heatmap fill label")
              ),
              column(2,
                shiny::numericInput(ns("coefplot_sig_pos"), "coefplot_sig_pos", value = 2, min = -5, max = 5, step = 0.5),
                tt("coefplot_sig_pos", "\u7cfb\u6570\u56fe\u663e\u8457\u6027\u4f4d\u7f6e", "Coefficient plot significance position")
              )
            )
          ),

          hr(),
          fluidRow(
            column(2, shiny::actionButton(ns("run_plot"), tr("\U0001f4ca \u751f\u6210\u56fe\u5f62", "\U0001f4ca Generate Plot"), icon = icon("play"), class = "btn-primary", width = "100%")),
            column(2, shiny::selectInput(ns("image_format"), tr("\u683c\u5f0f", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("save_width"), tr("\u5bbd (width)", "Width"), value = 10, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("save_height"), tr("\u9ad8 (height)", "Height"), value = 7, min = 3, max = 15)),
            column(2, shiny::numericInput(ns("save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(3, shiny::actionButton(ns("save_plot_btn"), tr("\U0001f4e5\u4fdd\u5b58\u56fe\u7247", "\U0001f4e5Save Plot"), icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
          )
        )
      )
    ),

    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca \u56fe\u533a", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
          shinycssloaders::withSpinner(shiny::plotOutput(ns("alpha_plot"), height = "500px"))
        )
      )
    )
  )
}

mod_alpha_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(
      plot = NULL,
      data_alpha = NULL,
      data_stat = NULL,
      res_diff = NULL,
      t_alpha = NULL,
      save_dir = NULL
    )

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
      default_name <- paste0("alpha_", input$plot_type, ".", input$image_format)
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
        showNotification("\U0001f4be \u8bf7\u5148\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939", type = "warning")
        return()
      }
      fname <- input$save_filename
      if (is.null(fname) || !isTRUE(nzchar(trimws(fname)))) {
        fname <- paste0("alpha_", input$plot_type)
      }
      fname <- trimws(fname)
      ext <- input$image_format
      if (!grepl(paste0("\\.", ext, "$"), fname, ignore.case = TRUE)) {
        fname <- sub("\\.(png|pdf|svg|tiff|tif)$", "", fname, ignore.case = TRUE)
        fname <- paste0(fname, ".", ext)
      }
      if (!dir.exists(save_dir)) {
        showNotification("\U0001f6d1 \u6587\u4ef6\u5939\u4e0d\u5b58\u5728\uff0c\u8bf7\u91cd\u65b0\u9009\u62e9", type = "error")
        return()
      }
      full_path <- file.path(save_dir, fname)

      tryCatch({
        ggplot2::ggsave(filename = full_path, plot = local_rv$plot,
          width = input$save_width, height = input$save_height,
          units = "in", dpi = input$save_dpi, scale = 1)
        removeModal()
        showNotification(paste0("\u2705 \U0001f4e5 \u5df2\u4fdd\u5b58\u81f3: ", full_path), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("\U0001f6a8 \U0001f50d \u4fdd\u5b58\u5931\u8d25: ", e$message), type = "error", duration = 10)
      })
    })

    output$group_col_ui <- shiny::renderUI({
      req(rv$microtable, rv$data_loaded)
      cols <- get_sample_cols(rv)
      lang <- rv$current_language %||% "zh"
      label <- if (lang == "en") "group" else "\u5206\u7ec4\u5217 (group)"
      shiny::selectInput(ns("group_col"), label,
        choices = c("\U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d" = "", cols))
    })

    output$by_group_ui <- shiny::renderUI({
      req(rv$microtable, rv$data_loaded)
      cols <- get_sample_cols(rv)
      shiny::selectInput(ns("by_group"), "by_group",
        choices = c("\U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d" = "", cols))
    })

    output$by_ID_ui <- shiny::renderUI({
      req(rv$microtable, rv$data_loaded)
      cols <- get_sample_cols(rv)
      lang <- rv$current_language %||% "zh"
      label <- if (lang == "en") "by_ID (paired)" else "by_ID (\u914d\u5bf9)"
      shiny::selectInput(ns("by_ID"), label,
        choices = c("\U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d" = "", cols))
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

    observeEvent(input$run_alpha, {
      if (!check_microtable(rv)) {
        showNotification("\U0001f6d1 \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d", type = "error")
        return()
      }

      measure_val <- input$measure
      if (is.null(measure_val) || length(measure_val) == 0) {
        measure_val <- app_config$alpha_measures
      }
      pd_val <- isTRUE(input$pd_switch)

      result <- tryCatch({
        mt <- rv$microtable

        mt$cal_alphadiv(measures = measure_val, PD = pd_val)

        dataset_name <- rv$microtable_name %||% "tmp_microtable"

        code <- paste0(
          "# \u03b1\u591a\u6837\u6027\u5206\u6790\n",
          dataset_name, "$cal_alphadiv(measures = c(",
          paste0("\"", measure_val, "\"", collapse = ", "),
          "), PD = ", tolower(as.character(pd_val)), ")\n"
        )

        list(success = TRUE, mt = mt, data_alpha = mt$alpha_diversity, code = code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      local_rv$mt <- result$mt
      local_rv$data_alpha <- result$data_alpha
      append_code(rv, result$code, "\u03b1\u591a\u6837\u6027\u5206\u6790 - \u8ba1\u7b97")
      showNotification("\u2705 \u03b1\u591a\u6837\u6027\u8ba1\u7b97\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_diff, {
      if (!check_microtable(rv)) {
        showNotification("\U0001f6d1 \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d", type = "error")
        return()
      }

      req(local_rv$mt)

      group_val <- if (isTRUE(nzchar(input$group_col))) input$group_col else NULL
      by_group_val <- if (isTRUE(nzchar(input$by_group))) input$by_group else NULL
      by_ID_val <- if (isTRUE(nzchar(input$by_ID))) input$by_ID else NULL
      diff_method_val <- input$diff_method

      measure_val <- input$diff_measure
      if (is.null(measure_val) || length(measure_val) == 0) {
        measure_val <- "Shannon"
      }
      measure_for_diff <- measure_val[1]

      result <- tryCatch({
        mt <- local_rv$mt

        t_alpha <- microeco::trans_alpha$new(
          dataset = mt,
          group = group_val,
          by_group = by_group_val,
          by_ID = by_ID_val,
          order_x = NULL
        )

        local_rv$t_alpha <- t_alpha

        diff_code <- paste0(
          "t_alpha <- microeco::trans_alpha$new(\n",
          "  dataset = ", rv$microtable_name %||% "tmp_microtable", ",\n",
          if (!is.null(group_val)) paste0("  group = \"", group_val, "\",\n") else "  group = NULL,\n",
          if (!is.null(by_group_val)) paste0("  by_group = \"", by_group_val, "\",\n") else "  by_group = NULL,\n",
          if (!is.null(by_ID_val)) paste0("  by_ID = \"", by_ID_val, "\",\n") else "  by_ID = NULL,\n",
          "  order_x = NULL\n",
          ")\n",
          "t_alpha$cal_diff(\n",
          "  method = \"", diff_method_val, "\",\n",
          "  measure = \"", measure_for_diff, "\",\n"
        )

        if (diff_method_val %in% c("KW", "KW_dunn", "wilcox", "t.test", "one-way anova")) {
          diff_code <- paste0(diff_code, "  p_adjust_method = \"", input$p_adjust_method, "\",\n")
        }

        diff_code <- paste0(diff_code, "  alpha = ", input$alpha_level)

        if (diff_method_val == "KW_dunn") {
          diff_code <- paste0(diff_code, ",\n  KW_dunn_letter = ", input$KW_dunn_letter)
        }
        if (diff_method_val == "one-way anova") {
          diff_code <- paste0(diff_code, ",\n  anova_post_test = \"", input$anova_post_test, "\",\n",
            "  anova_varequal_test = ", input$anova_varequal_test)
        }
        if (diff_method_val %in% c("lm", "lme", "multi-anova")) {
          formula_val <- trimws(input$diff_formula)
          formula_val <- gsub("^['\"]|['\"]$", "", formula_val)
          formula_val <- gsub("^~\\s*", "", formula_val)
          diff_code <- paste0(diff_code, ",\n  formula = ", formula_val)
        }
        diff_code <- paste0(diff_code, "\n)\n")

        if (diff_method_val %in% c("lm", "lme", "multi-anova")) {
          t_alpha$cal_diff(
            method = diff_method_val,
            measure = measure_for_diff,
            alpha = input$alpha_level,
            formula = gsub("^~\\s*", "", trimws(input$diff_formula))
          )
        } else if (diff_method_val == "KW_dunn") {
          t_alpha$cal_diff(
            method = diff_method_val,
            measure = measure_for_diff,
            p_adjust_method = input$p_adjust_method,
            alpha = input$alpha_level,
            KW_dunn_letter = input$KW_dunn_letter
          )
        } else if (diff_method_val == "one-way anova") {
          t_alpha$cal_diff(
            method = diff_method_val,
            measure = measure_for_diff,
            p_adjust_method = input$p_adjust_method,
            alpha = input$alpha_level,
            anova_post_test = input$anova_post_test,
            anova_varequal_test = input$anova_varequal_test
          )
        } else if (diff_method_val == "scheirerRayHare") {
          t_alpha$cal_diff(
            method = diff_method_val,
            measure = measure_for_diff,
            alpha = input$alpha_level
          )
        } else {
          t_alpha$cal_diff(
            method = diff_method_val,
            measure = measure_for_diff,
            p_adjust_method = input$p_adjust_method,
            alpha = input$alpha_level
          )
        }

        list(success = TRUE, t_alpha = t_alpha, res_diff = t_alpha$res_diff, data_alpha = mt$alpha_diversity, code = diff_code)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      local_rv$t_alpha <- result$t_alpha
      local_rv$res_diff <- result$res_diff
      local_rv$data_alpha <- result$data_alpha
      append_code(rv, result$code, "\u03b1\u591a\u6837\u6027\u5206\u6790 - \u5dee\u5f02\u68c0\u9a8c")
      showNotification("\u2705 \u5dee\u5f02\u68c0\u9a8c\u5b8c\u6210", type = "message")
    })

    observeEvent(input$run_plot, {
      if (!check_microtable(rv)) {
        showNotification("\U0001f6d1 \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d \U0001f50d", type = "error")
        return()
      }

      req(local_rv$t_alpha)

      order_x_val <- if (isTRUE(nzchar(input$order_x))) strsplit(trimws(input$order_x), "[,\\s]+")[[1]] else NULL
      measure_for_plot <- if (isTRUE(nzchar(input$diff_measure))) input$diff_measure else "Shannon"

      result <- tryCatch({
        t_alpha <- local_rv$t_alpha

        if (!is.null(order_x_val)) {
          attr(t_alpha, "order_x") <- order_x_val
        }

        p <- t_alpha$plot_alpha(
          plot_type = input$plot_type,
          color_values = get_color_palette(input$color_theme),
          measure = measure_for_plot,
          group = NULL,
          add = input$add,
          add_sig = input$add_sig,
          add_sig_label = input$add_sig_label,
          add_sig_text_size = input$add_sig_text_size,
          add_sig_label_num_dec = input$add_sig_label_num_dec,
          order_x_mean = input$order_x_mean,
          y_start = input$y_start,
          y_increase = input$y_increase,
          xtext_angle = input$xtext_angle,
          xtext_size = input$xtext_size,
          ytitle_size = input$ytitle_size,
          bar_width = input$bar_width,
          bar_alpha = input$bar_alpha,
          dodge_width = input$dodge_width,
          plot_SE = input$plot_SE,
          errorbar_size = input$errorbar_size,
          errorbar_width = input$errorbar_width,
          errorbar_addpoint = input$errorbar_addpoint,
          errorbar_color_black = input$errorbar_color_black,
          point_size = input$point_size,
          point_alpha = input$point_alpha,
          add_line = input$add_line,
          line_size = input$line_size,
          line_type = as.numeric(input$line_type),
          line_color = input$line_color,
          line_alpha = input$line_alpha,
          heatmap_cell = input$heatmap_cell,
          heatmap_sig = input$heatmap_sig,
          heatmap_x = input$heatmap_x,
          heatmap_y = input$heatmap_y,
          heatmap_lab_fill = input$heatmap_lab_fill,
          coefplot_sig_pos = input$coefplot_sig_pos
        )

        list(success = TRUE, plot = p)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      local_rv$plot <- result$plot
      rv$last_plot <- result$plot

      color_code <- if (input$color_theme == "Viridis") {
        "viridisLite::viridis(8)"
      } else {
        paste0("RColorBrewer::brewer.pal(8, \"", input$color_theme, "\")")
      }

      plot_code <- paste0(
        "# Alpha 多样性图形\n",
        "p <- t_alpha$plot_alpha(\n",
        "  plot_type = \"", input$plot_type, "\",\n",
        "  color_values = ", color_code, ",\n",
        "  measure = \"", measure_for_plot, "\",\n",
        "  group = NULL,\n",
        "  add = \"", input$add, "\",\n",
        "  add_sig = ", input$add_sig, ",\n",
        "  add_sig_label = \"", input$add_sig_label, "\",\n",
        "  add_sig_text_size = ", input$add_sig_text_size, ",\n",
        "  add_sig_label_num_dec = ", input$add_sig_label_num_dec, ",\n",
        "  order_x_mean = ", input$order_x_mean, ",\n",
        "  y_start = ", input$y_start, ",\n",
        "  y_increase = ", input$y_increase, ",\n",
        "  xtext_angle = ", input$xtext_angle, ",\n",
        "  xtext_size = ", input$xtext_size, ",\n",
        "  ytitle_size = ", input$ytitle_size, ",\n",
        "  bar_width = ", input$bar_width, ",\n",
        "  bar_alpha = ", input$bar_alpha, ",\n",
        "  dodge_width = ", input$dodge_width, ",\n",
        "  plot_SE = ", input$plot_SE, ",\n",
        "  errorbar_size = ", input$errorbar_size, ",\n",
        "  errorbar_width = ", input$errorbar_width, ",\n",
        "  errorbar_addpoint = ", input$errorbar_addpoint, ",\n",
        "  errorbar_color_black = ", input$errorbar_color_black, ",\n",
        "  point_size = ", input$point_size, ",\n",
        "  point_alpha = ", input$point_alpha, ",\n",
        "  add_line = ", input$add_line, ",\n",
        "  line_size = ", input$line_size, ",\n",
        "  line_type = ", as.numeric(input$line_type), ",\n",
        "  line_color = \"", input$line_color, "\",\n",
        "  line_alpha = ", input$line_alpha, ",\n",
        "  heatmap_cell = \"", input$heatmap_cell, "\",\n",
        "  heatmap_sig = \"", input$heatmap_sig, "\",\n",
        "  heatmap_x = \"", input$heatmap_x, "\",\n",
        "  heatmap_y = \"", input$heatmap_y, "\",\n",
        "  heatmap_lab_fill = \"", input$heatmap_lab_fill, "\",\n",
        "  coefplot_sig_pos = ", input$coefplot_sig_pos, "\n",
        ")\n",
        "\nprint(p)"
      )
      append_code(rv, plot_code, "\u03b1\u591a\u6837\u6027 - \u751f\u6210\u56fe\u5f62")

      showNotification("\u2705 \u56fe\u5f62\u751f\u6210\u5b8c\u6210", type = "message")
    })

    output$alpha_plot <- shiny::renderPlot({
      req(local_rv$plot)
      local_rv$plot
    })

    output$alpha_table <- DT::renderDataTable({
      req(local_rv$data_alpha)
      dt <- local_rv$data_alpha
      if (is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 5), rownames = TRUE, filter = "top")
      }
    })

    output$alpha_diff_table <- DT::renderDataTable({
      req(local_rv$res_diff)
      dt <- local_rv$res_diff
      if (is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
      }
    })

    output$download_alpha_table <- downloadHandler(
      filename = function() "alpha_diversity.csv",
      content = function(file) {
        req(local_rv$data_alpha)
        write.table(local_rv$data_alpha, file, sep = ",", row.names = TRUE, quote = TRUE)
      }
    )

    output$download_diff_table <- downloadHandler(
      filename = function() "alpha_diff_results.csv",
      content = function(file) {
        req(local_rv$res_diff)
        write.table(local_rv$res_diff, file, sep = ",", row.names = FALSE, quote = TRUE)
      }
    )
  })
}
