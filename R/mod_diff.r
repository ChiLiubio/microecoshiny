#' @title Differential Abundance Module UI
#' @description
#' Provides interface for differential abundance analysis using multiple methods
#' including LEfSe, Wilcoxon, ANOVA, DESeq2, edgeR, ANCOMBC2, etc.
#' @param id Module ID
#' @param lang Language code ("zh" or "en")
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @keywords differential abundance microbiome
#' @family statistical-analysis
mod_diff_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(
      column(12, h2(tr("\u2696 \u5dee\u5f02\u4e30\u5ea6\u5206\u6790 Differential Abundance", "\u2696 Differential Abundance")))
    ),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4cb \u53c2\u6570\u8bbe\u7f6e", "\U0001f4cb Parameters"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,
        fluidRow(
          column(12, shiny::radioButtons(ns("diff_method"), tr("\u65b9\u6cd5 (method)", "method"),
            choices = c("lefse", "rf", "KW", "KW_dunn", "wilcox", "t.test", "anova", "scheirerRayHare",
              "lm", "betareg", "lme", "glmm", "glmm_beta", "metastat", "metagenomeSeq",
              "ALDEx2_t", "ALDEx2_kw", "DESeq2", "edgeR", "ancombc2", "linda", "maaslin"),
            selected = "wilcox", inline = TRUE))
        ),
        hr(),
        h4(tr("\u57fa\u672c\u53c2\u6570", "Basic Parameters")),
        fluidRow(
          column(2, shiny::selectInput(ns("diff_taxrank"), tr("\u5206\u7c7b\u5c42\u7ea7 (taxa_level)", "taxa_level"),
            choices = character(0), selected = NULL)),
          column(2, shiny::selectInput(ns("diff_group"), tr("\u5206\u7ec4\u5217 (group)", "group"), choices = character(0))),
          column(2, shiny::numericInput(ns("filter_thres"), tr("\u4e30\u5ea6\u8fc7\u6ee4\u9608\u503c (filter_thres)", "filter_thres"), value = 0, min = 0, max = 1, step = 0.001)),
          column(2, shiny::selectInput(ns("p_adjust_method"), tr("p\u503c\u6821\u6b63 (p_adjust_method)", "p_adjust_method"),
            choices = c("fdr", "holm", "bonferroni", "none", "BH", "BY"), selected = "fdr")),
          column(2, shinyWidgets::materialSwitch(ns("remove_unknown"), tr("\u79fb\u9664\u672a\u77e5\u5206\u7c7b (remove_unknown)", "remove_unknown"), value = TRUE, status = "success"))
        ),
          
          shiny::conditionalPanel(condition = "input.diff_method == 'lefse'", ns = ns,
            hr(),
            h4(tr("LEfSe \u53c2\u6570", "LEfSe Parameters")),
            fluidRow(
              column(2, shiny::numericInput(ns("alpha"), tr("\u663e\u8457\u6027\u6c34\u5e73 (alpha)", "alpha"), value = 0.05, min = 0.001, max = 0.1, step = 0.01)),
              column(2, shiny::selectInput(ns("lefse_subgroup"), tr("\u5b50\u7ec4\u5217 (lefse_subgroup)", "lefse_subgroup"), choices = character(0))),
              column(2, shiny::numericInput(ns("lefse_min_subsam"), tr("\u6700\u5c0f\u6837\u672c\u6570 (lefse_min_subsam)", "lefse_min_subsam"), value = 10, min = 1, max = 100)),
              column(2, shinyWidgets::materialSwitch(ns("lefse_sub_strict"), tr("\u4e25\u683c\u6a21\u5f0f (lefse_sub_strict)", "lefse_sub_strict"), value = FALSE, status = "warning")),
              column(2, shiny::numericInput(ns("lefse_sub_alpha"), tr("\u5b50\u7ec4\u663e\u8457\u6027 (lefse_sub_alpha)", "lefse_sub_alpha"), value = 0.05, min = 0.001, max = 0.1, step = 0.01)),
              column(2)
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("lefse_norm"), tr("\u5f52\u4e00\u5316\u503c (lefse_norm)", "lefse_norm"), value = 1000000, min = -1, max = 10000000, step = 100000)),
              column(2, shiny::numericInput(ns("nresam_lefse"), tr("\u91c7\u6837\u6bd4\u4f8b (nresam)", "nresam"), value = 0.6667, min = 0.1, max = 1, step = 0.1)),
              column(2, shiny::numericInput(ns("boots_lefse"), tr("\u81ea\u52a9\u6cd5\u6b21\u6570 (boots)", "boots"), value = 30, min = 1, max = 100)),
              column(6)
            )
          ),
          
          shiny::conditionalPanel(condition = "input.diff_method == 'rf'", ns = ns,
            hr(),
            h4(tr("Random Forest \u53c2\u6570", "Random Forest Parameters")),
            fluidRow(
              column(2, shiny::numericInput(ns("alpha"), tr("\u663e\u8457\u6027\u6c34\u5e73 (alpha)", "alpha"), value = 0.05, min = 0.001, max = 0.1, step = 0.01)),
              column(2, shiny::selectInput(ns("rf_imp_type"), tr("\u91cd\u8981\u6027\u7c7b\u578b (rf_imp_type)", "rf_imp_type"),
                choices = setNames(c("2", "1"), c(tr("MeanDecreaseGini", "MeanDecreaseGini"), tr("MeanDecreaseAccuracy", "MeanDecreaseAccuracy"))), selected = "2")),
              column(2, shiny::numericInput(ns("nresam_rf"), tr("\u91c7\u6837\u6bd4\u4f8b (nresam)", "nresam"), value = 0.6667, min = 0.1, max = 1, step = 0.1)),
              column(2, shiny::numericInput(ns("boots_rf"), tr("\u81ea\u52a9\u6cd5\u6b21\u6570 (boots)", "boots"), value = 30, min = 1, max = 100)),
              column(6)
            )
          ),
          
          shiny::conditionalPanel(condition = "['KW','KW_dunn','wilcox','t.test','anova','scheirerRayHare'].includes(input.diff_method)", ns = ns,
            hr(),
            h4(tr("\u7edf\u8ba1\u68c0\u9a8c\u53c2\u6570", "Statistical Test Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("transformation"), tr("\u6570\u636e\u8f6c\u6362 (transformation)", "transformation"),
                choices = c("none", "AST", "log", "log10", "sqrt"), selected = "none")),
              column(2, shiny::selectInput(ns("by_group"), tr("\u5206\u7ec4\u8ba1\u7b97 (by_group)", "by_group"), choices = character(0))),
              column(2, shiny::selectInput(ns("by_ID"), tr("\u914d\u5bf9\u5217 (by_ID)", "by_ID"), choices = character(0))),
              column(2)
            ),
            shiny::conditionalPanel(condition = "['anova','scheirerRayHare'].includes(input.diff_method)", ns = ns,
              fluidRow(
                column(2, shiny::numericInput(ns("alpha"), tr("\u663e\u8457\u6027\u6c34\u5e73 (alpha)", "alpha"), value = 0.05, min = 0.001, max = 0.1, step = 0.01)),
                column(6, shiny::textInput(ns("diff_formula"), tr("\u516c\u5f0f (formula)", "formula"), value = "Group")),
                column(4)
              )
            )
          ),
          
          shiny::conditionalPanel(condition = "['lm','betareg','lme','glmm','glmm_beta'].includes(input.diff_method)", ns = ns,
            hr(),
            h4(tr("\u516c\u5f0f\u6a21\u578b\u53c2\u6570", "Formula Model Parameters")),
            fluidRow(
              column(2, shiny::selectInput(ns("transformation"), tr("\u6570\u636e\u8f6c\u6362 (transformation)", "transformation"),
                choices = c("none", "AST", "log", "log10", "sqrt"), selected = "none")),
              column(6, shiny::textInput(ns("diff_formula"), tr("\u516c\u5f0f (formula)", "formula"), value = "Group")),
              column(4)
            )
          ),
          
          shiny::conditionalPanel(condition = "['betareg','glmm_beta'].includes(input.diff_method)", ns = ns,
            fluidRow(
              column(2, shiny::numericInput(ns("beta_pseudo"), tr("\u4f2a\u8ba1\u6570 (beta_pseudo)", "beta_pseudo"), value = .Machine$double.eps, min = 0, max = 0.01, step = 0.0001)),
              column(10)
            )
          ),
          
          shiny::conditionalPanel(condition = "['metastat','metagenomeSeq','ALDEx2_t','edgeR'].includes(input.diff_method)", ns = ns,
            hr(),
            h4(tr("\u914d\u5bf9\u68c0\u9a8c\u53c2\u6570", "Paired Test Parameters")),
            fluidRow(
              column(4, shiny::selectInput(ns("group_choose_paired"), tr("\u914d\u5bf9\u7ec4\u5217 (group_choose_paired)", "group_choose_paired"), choices = character(0))),
              column(8)
            )
          ),
          
          shiny::conditionalPanel(condition = "input.diff_method == 'metagenomeSeq'", ns = ns,
            fluidRow(
              column(2, shiny::numericInput(ns("metagenomeSeq_count"), tr("\u6700\u5c0f\u8ba1\u6570 (metagenomeSeq_count)", "metagenomeSeq_count"), value = 1, min = 0, max = 100)),
              column(10)
            )
          ),
          
          shiny::conditionalPanel(condition = "['ALDEx2_t','ALDEx2_kw'].includes(input.diff_method)", ns = ns,
            hr(),
            h4(tr("ALDEx2 \u53c2\u6570", "ALDEx2 Parameters")),
            fluidRow(
              column(4, shiny::selectInput(ns("ALDEx2_sig"), tr("\u663e\u8457\u6027\u6807\u51c6 (ALDEx2_sig)", "ALDEx2_sig"),
                choices = c("wi.eBH & kw.eBH", "we.eBH & glm.eBH"), selected = "wi.eBH & kw.eBH")),
              column(8)
            )
          ),
          
          hr(),
          fluidRow(
            column(3, shiny::actionButton(ns("run_diff"), tr("\U0001f4ca \u5f00\u59cb\u8ba1\u7b97", "\U0001f4ca Calculate"), icon = icon("play"), class = "btn-primary", width = "100%")),
            column(9)
          ),
          hr(),
          fluidRow(
            column(12,
              bs4Dash::box(
                title = tr("\u23f3 \u8ba1\u7b97\u72b6\u6001", "\u23f3 Calculation Status"),
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = NULL,
                shiny::verbatimTextOutput(ns("calc_status"), placeholder = TRUE)
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca \u7edf\u8ba1\u68c0\u9a8c\u7ed3\u679c", "\U0001f4ca Results Table"), status = "info", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("diff_table"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4cb \u56fe\u8868\u8bbe\u7f6e", "\U0001f4cb Plot Settings"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          collapsible = FALSE,
          h4(tr("\u56fe\u8868\u7c7b\u578b (plot_type)", "plot_type")),
          fluidRow(
            column(3, shinyWidgets::materialSwitch(ns("add_diff_bar"), "diff_bar", value = TRUE, status = "primary")),
            column(3, shinyWidgets::materialSwitch(ns("add_diff_abund"), "diff_abund", value = FALSE, status = "success")),
            column(3, shinyWidgets::materialSwitch(ns("add_volcano"), "volcano", value = FALSE, status = "warning")),
            column(3, shinyWidgets::materialSwitch(ns("add_cladogram"), "cladogram", value = FALSE, status = "info"))
          ),
          hr(),
          h4(tr("\u56fe\u8868\u53c2\u6570", "Plot Parameters")),
          fluidRow(
            column(2, shiny::selectInput(ns("color_theme"), tr("\u989c\u8272 (color_theme)", "color_theme"),
              choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"), selected = "Dark2")),
            column(2, shiny::selectInput(ns("use_number_type"), "use_number",
              choices = c("1:10" = "1:10", "1:20" = "1:20", "1:30" = "1:30", "1:50" = "1:50"), selected = "1:10")),
            column(2, shinyWidgets::materialSwitch(ns("coord_flip"), "coord_flip", value = TRUE, status = "info")),
            column(2, shinyWidgets::materialSwitch(ns("add_sig"), "add_sig", value = TRUE, status = "primary")),
            column(2, shiny::numericInput(ns("threshold"), "threshold (LDA)", value = 0, min = 0, max = 10, step = 0.5)),
            column(2, shinyWidgets::materialSwitch(ns("simplify_names"), "simplify_names", value = TRUE, status = "success"))
          ),
          fluidRow(
            column(2, shinyWidgets::materialSwitch(ns("keep_prefix"), "keep_prefix", value = TRUE, status = "info")),
            column(2, shiny::numericInput(ns("xtext_angle"), "xtext_angle", value = 45, min = 0, max = 90, step = 15)),
            column(2, shiny::numericInput(ns("xtext_size"), "xtext_size", value = 13, min = 8, max = 20)),
            column(2, shiny::numericInput(ns("ytitle_size"), "ytitle_size", value = 17, min = 10, max = 24)),
            column(4)
          ),
          hr(),
          fluidRow(
            column(2, shiny::selectInput(ns("image_format"), tr("\u683c\u5f0f", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("save_width"), tr("\u5bbd (width)", "Width"), value = 12, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("save_height"), tr("\u9ad8 (height)", "Height"), value = 8, min = 3, max = 15)),
            column(2, shiny::numericInput(ns("save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(2, shiny::downloadButton(ns("save_plot"), tr("\U0001f4e5\u4fdd\u5b58\u56fe\u7247", "\U0001f4e5Save Plot"), class = "btn-outline-secondary", width = "100%")),
            column(2, shiny::downloadButton(ns("download_table"), tr("\U0001f4e5\u8868\u683c\u4fdd\u5b58", "\U0001f4e5Save Table"), class = "btn-outline-info", width = "100%"))
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca \u56fe\u533a", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
          shinycssloaders::withSpinner(shiny::plotOutput(ns("diff_plot"), height = "500px"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca \u4e30\u5ea6\u6570\u636e", "\U0001f4ca Abundance Data"), status = "secondary", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("abund_table"))
        )
      )
    )
  )
}

mod_diff_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(plot = NULL, data_diff = NULL, data_abund = NULL, t_diff = NULL, calc_messages = "\u7b49\u5f85\u5f00\u59cb\u8ba1\u7b97...")

    output$calc_status <- renderText({
      local_rv$calc_messages
    })

    output$save_plot <- downloadHandler(
      filename = function() {
        paste0("diff_", input$diff_method, ".", input$image_format)
      },
      content = function(file) {
        req(local_rv$plot)
        ggplot2::ggsave(filename = file, plot = local_rv$plot,
          width = input$save_width, height = input$save_height,
          units = "in", dpi = input$save_dpi, scale = 1)
      }
    )

    observe({
      if (!check_microtable(rv)) {
        updateSelectInput(session, "diff_group", choices = character(0))
        updateSelectInput(session, "by_group", choices = character(0))
        updateSelectInput(session, "by_ID", choices = character(0))
        updateSelectInput(session, "lefse_subgroup", choices = character(0))
        updateSelectInput(session, "group_choose_paired", choices = character(0))
        updateSelectInput(session, "diff_taxrank", choices = character(0))
        return()
      }
      cols <- get_sample_cols(rv)
      updateSelectInput(session, "diff_group", choices = cols)
      updateSelectInput(session, "by_group", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "by_ID", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "lefse_subgroup", choices = c("\u65e0" = "", cols))
      updateSelectInput(session, "group_choose_paired", choices = c("\u65e0" = "", cols))
      
      tax_ranks <- get_tax_ranks(rv)
      current_method <- input$diff_method
      
      if (is.null(current_method) || current_method == "") {
        current_method <- "KW"
      }
      
      if (current_method == "lefse") {
        tax_choices <- c("all" = "all", "ASV/OTU" = "ASV", tax_ranks)
        tax_selected <- "all"
      } else if (current_method %in% c("rf", "KW", "KW_dunn", "wilcox", "t.test", "anova", "scheirerRayHare")) {
        phylum_rank <- if ("Phylum" %in% tax_ranks) "Phylum" else if ("phylum" %in% tax_ranks) "phylum" else tax_ranks[1]
        tax_choices <- c("ASV/OTU" = "ASV", tax_ranks)
        tax_selected <- phylum_rank
      } else if (current_method %in% c("lm", "betareg", "lme", "glmm", "glmm_beta", "maaslin")) {
        genus_rank <- if ("Genus" %in% tax_ranks) "Genus" else if ("genus" %in% tax_ranks) "genus" else tax_ranks[1]
        tax_choices <- c("ASV/OTU" = "ASV", tax_ranks)
        tax_selected <- genus_rank
      } else {
        tax_choices <- c("ASV/OTU" = "ASV", tax_ranks)
        tax_selected <- "ASV"
      }
      
      updateSelectInput(session, "diff_taxrank", choices = tax_choices, selected = tax_selected)
    })

    get_color_palette <- function(name) {
      palettes <- list(
        "Dark2"    = RColorBrewer::brewer.pal(8, "Dark2"),
        "Set1"     = RColorBrewer::brewer.pal(8, "Set1"),
        "Set2"     = RColorBrewer::brewer.pal(8, "Set2"),
        "Set3"     = RColorBrewer::brewer.pal(12, "Set3"),
        "Paired"   = RColorBrewer::brewer.pal(12, "Paired"),
        "Spectral" = RColorBrewer::brewer.pal(11, "Spectral"),
        "Viridis"  = viridisLite::viridis(8)
      )
      if (name %in% names(palettes)) {
        return(palettes[[name]])
      }
      RColorBrewer::brewer.pal(8, "Dark2")
    }

    parse_use_number <- function(str) {
      parts <- strsplit(str, ":")[[1]]
      as.integer(parts[1]):as.integer(parts[2])
    }

    observeEvent(input$run_diff, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      alpha_val <- if (!is.null(input$alpha)) input$alpha else 0.05
      
      group_val <- if (isTRUE(nzchar(input$diff_group))) input$diff_group else NULL
      lefse_subgroup_val <- if (!is.null(input$lefse_subgroup) && isTRUE(nzchar(input$lefse_subgroup))) input$lefse_subgroup else NULL
      group_choose_paired_val <- if (!is.null(input$group_choose_paired) && isTRUE(nzchar(input$group_choose_paired))) input$group_choose_paired else NULL
      
      by_group_val <- if (!is.null(input$by_group) && isTRUE(nzchar(input$by_group))) input$by_group else NULL
      
      by_ID_val <- if (!is.null(input$by_ID) && isTRUE(nzchar(input$by_ID))) input$by_ID else NULL
      
      transformation_val <- if (!is.null(input$transformation) && input$transformation != "none") input$transformation else NULL
      
      formula_val <- {
        f <- if (!is.null(input$diff_formula)) trimws(input$diff_formula) else ""
        if (isTRUE(nzchar(f))) {
          gsub("^~\\s*", "", f)
        } else {
          NULL
        }
      }
      
      beta_pseudo_val <- if (!is.null(input$beta_pseudo)) input$beta_pseudo else .Machine$double.eps

      aldEx2_sig_val <- if (!is.null(input$ALDEx2_sig) && input$ALDEx2_sig == "wi.eBH & kw.eBH") c("wi.eBH", "kw.eBH") else c("we.eBH", "glm.eBH")

      nresam_val <- if (input$diff_method == "lefse") input$nresam_lefse else if (input$diff_method == "rf") input$nresam_rf else 0.6667
      boots_val <- if (input$diff_method == "lefse") input$boots_lefse else if (input$diff_method == "rf") input$boots_rf else 30

      dataset_name <- rv$microtable_name %||% "tmp_microtable"
      transformation_code <- if (is.null(transformation_val)) "NULL" else paste0("\"", transformation_val, "\"")
      code_str <- paste0(
        "# \u5dee\u5f02\u4e30\u5ea6\u5206\u6790 - ", input$diff_method, "\n",
        "t_diff <- microeco::trans_diff$new(\n",
        "  dataset = ", dataset_name, ",\n",
        "  method = \"", input$diff_method, "\",\n",
        "  group = ", if (!is.null(group_val)) paste0("\"", group_val, "\"") else "NULL", ",\n",
        "  taxa_level = \"", input$diff_taxrank, "\",\n",
        "  alpha = ", alpha_val, ",\n",
        "  filter_thres = ", input$filter_thres, ",\n",
        "  p_adjust_method = \"", input$p_adjust_method, "\",\n",
        "  transformation = ", transformation_code, ",\n",
        "  remove_unknown = ", input$remove_unknown, ",\n",
        "  lefse_subgroup = ", if (!is.null(lefse_subgroup_val)) paste0("\"", lefse_subgroup_val, "\"") else "NULL", ",\n",
        "  lefse_min_subsam = ", input$lefse_min_subsam, ",\n",
        "  lefse_sub_strict = ", input$lefse_sub_strict, ",\n",
        "  lefse_sub_alpha = ", input$lefse_sub_alpha, ",\n",
        "  lefse_norm = ", input$lefse_norm, ",\n",
        "  boots = ", boots_val, ",\n",
        "  nresam = ", nresam_val, ",\n",
        "  rf_imp_type = ", input$rf_imp_type, ",\n",
        "  group_choose_paired = ", if (!is.null(group_choose_paired_val)) paste0("\"", group_choose_paired_val, "\"") else "NULL", ",\n",
        "  metagenomeSeq_count = ", input$metagenomeSeq_count, ",\n",
        "  by_group = ", if (!is.null(by_group_val)) paste0("\"", by_group_val, "\"") else "NULL", ",\n",
        "  by_ID = ", if (!is.null(by_ID_val)) paste0("\"", by_ID_val, "\"") else "NULL", ",\n",
        "  beta_pseudo = ", beta_pseudo_val, ",\n",
        "  formula = ", if (!is.null(formula_val)) paste0("\"", formula_val, "\"") else "NULL", "\n",
        ")\n"
      )

      append_code(rv, code_str, paste0("\u5dee\u5f02\u4e30\u5ea6 - ", input$diff_method))

      local_rv$calc_messages <- "开始计算..."
      messages <- character(0)

      t_diff <- tryCatch(
        withCallingHandlers({
          microeco::trans_diff$new(
            dataset = rv$microtable,
            method = input$diff_method,
            group = group_val,
            taxa_level = input$diff_taxrank,
            filter_thres = input$filter_thres,
            alpha = alpha_val,
            p_adjust_method = input$p_adjust_method,
            transformation = transformation_val,
            remove_unknown = input$remove_unknown,
            lefse_subgroup = lefse_subgroup_val,
            lefse_min_subsam = input$lefse_min_subsam,
            lefse_sub_strict = input$lefse_sub_strict,
            lefse_sub_alpha = input$lefse_sub_alpha,
            lefse_norm = input$lefse_norm,
            boots = boots_val,
            nresam = nresam_val,
            rf_imp_type = as.integer(input$rf_imp_type),
            group_choose_paired = group_choose_paired_val,
            metagenomeSeq_count = input$metagenomeSeq_count,
            ALDEx2_sig = aldEx2_sig_val,
            by_group = by_group_val,
            by_ID = by_ID_val,
            beta_pseudo = beta_pseudo_val,
            formula = formula_val
          )
        }, message = function(m) {
          messages <<- c(messages, conditionMessage(m))
          local_rv$calc_messages <<- paste(messages, collapse = "\n")
        }),
        error = function(e) {
          local_rv$calc_messages <<- paste0(local_rv$calc_messages, "\n\n\u274c \u8ba1\u7b97\u5931\u8d25: ", conditionMessage(e))
          stop(conditionMessage(e))
        }
      )

      local_rv$calc_messages <- paste0(local_rv$calc_messages, "\n\n\u2705 \u8ba1\u7b97\u5b8c\u6210\uff01")
      showNotification("\u5b8c\u6210", type = "message")

      local_rv$t_diff <- t_diff
      local_rv$data_diff <- t_diff$res_diff
      local_rv$data_abund <- t_diff$res_abund

      plot_type <- if (isTRUE(input$add_diff_bar)) {
        "diff_bar"
      } else if (isTRUE(input$add_diff_abund)) {
        "diff_abund"
      } else if (isTRUE(input$add_volcano)) {
        "volcano"
      } else if (isTRUE(input$add_cladogram) && input$diff_method %in% c("lefse", "rf")) {
        "cladogram"
      } else {
        "diff_bar"
      }

      p <- tryCatch({
        if (plot_type == "diff_bar") {
          t_diff$plot_diff_bar(
            color_values = get_color_palette(input$color_theme),
            use_number = parse_use_number(input$use_number_type),
            threshold = if (input$threshold > 0) input$threshold else NULL,
            coord_flip = input$coord_flip,
            add_sig = input$add_sig,
            simplify_names = input$simplify_names,
            keep_prefix = input$keep_prefix,
            xtext_angle = input$xtext_angle,
            xtext_size = input$xtext_size,
            ytitle_size = input$ytitle_size
          )
        } else if (plot_type == "diff_abund") {
          t_diff$plot_diff_abund(
            use_number = parse_use_number(input$use_number_type),
            color_values = get_color_palette(input$color_theme),
            coord_flip = input$coord_flip,
            add_sig = input$add_sig,
            simplify_names = input$simplify_names,
            keep_prefix = input$keep_prefix,
            xtext_angle = input$xtext_angle,
            xtext_size = input$xtext_size,
            ytitle_size = input$ytitle_size
          )
        } else if (plot_type == "volcano") {
          t_diff$plot_volcano(
            log2fc_cutoff = 1,
            pvalue_cutoff = input$alpha,
            label_top_n = 10
          )
        } else if (plot_type == "cladogram") {
          t_diff$plot_diff_cladogram(
            color = get_color_palette(input$color_theme),
            use_taxa_num = 200,
            use_feature_num = if (input$threshold > 0) NULL else 30
          )
        }
      }, error = function(e) {
        showNotification(paste0("\u7ed8\u56fe\u5931\u8d25: ", e$message), type = "warning", duration = 10)
        NULL
      })

      local_rv$plot <- p
      rv$last_plot <- p
      local_rv$calc_messages <- paste0(local_rv$calc_messages, "\n\n\u2705 \u8ba1\u7b97\u5b8c\u6210\uff01")
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$diff_plot <- shiny::renderPlot({
      req(local_rv$plot)
      local_rv$plot
    })

    output$diff_table <- DT::renderDataTable({
      req(local_rv$data_diff)
      dt <- local_rv$data_diff
      if (is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
      }
    })

    output$abund_table <- DT::renderDataTable({
      req(local_rv$data_abund)
      dt <- local_rv$data_abund
      if (is.data.frame(dt)) {
        DT::datatable(dt, options = list(scrollX = TRUE, pageLength = 20), rownames = FALSE, filter = "top")
      }
    })

    output$download_table <- downloadHandler(
      filename = function() paste0("diff_result_", input$diff_method, ifelse(input$table_format == ",", ".csv", ".tsv")),
      content = function(file) {
        req(local_rv$data_diff)
        write.table(local_rv$data_diff, file, sep = input$table_format, row.names = FALSE, quote = TRUE)
      }
    )
  })
}
