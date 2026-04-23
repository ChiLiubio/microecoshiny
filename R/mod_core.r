#' @title Core Microbiome Module UI
#' @description
#' Provides interface for core microbiome analysis including identification of
#' core taxa present in majority of samples and Venn diagram visualization.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash
#' @keywords core microbiome venn
#' @family community-analysis
mod_core_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(column(12, h2(tr("\U0001f3af 核心微生物组 Core Microbiome", "\U0001f3af Core Microbiome")))),
    fluidRow(
      column(4,
        bs4Dash::box(
          title = tr("参数设置", "Parameters"),
          status = "primary", solidHeader = TRUE, width = NULL,
          shiny::selectInput(ns("core_taxrank"), tr("分类水平", "Taxonomic Level"), choices = app_config$taxranks, selected = "Genus"),
          shiny::sliderInput(ns("core_freq"), tr("出现频率阈值 (%)", "Presence Frequency Threshold (%)"), min = 0, max = 100, value = 50),
          shiny::sliderInput(ns("core_abund"), tr("平均相对丰度阈值", "Mean Relative Abundance Threshold"), min = 0, max = 0.5, value = 0.01, step = 0.001),
          shiny::selectInput(ns("core_group"), tr("分组列", "Group Column"), choices = character(0)),
          br(),
          shiny::actionButton(ns("run_core"), tr("计算核心微生物组", "Calculate Core Microbiome"), icon = icon("bullseye"), class = "btn-primary"),
          shiny::actionButton(ns("run_venn"), tr("Venn 图", "Venn Diagram"), icon = icon("chart-pie"), class = "btn-info")
        )
      ),
      column(8,
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(title = tr("核心微生物图", "Core Microbiome Plot"),
            shinycssloaders::withSpinner(shiny::plotOutput(ns("core_plot"), height = "500px"))
          ),
          shiny::tabPanel(title = tr("Venn 图", "Venn Diagram"),
            shinycssloaders::withSpinner(shiny::plotOutput(ns("venn_plot"), height = "500px"))
          ),
          shiny::tabPanel(title = tr("数据表", "Data Table"),
            shinycssloaders::withSpinner(DT::dataTableOutput(ns("core_table")))
          )
        )
      )
    )
  )
}

#' Core Microbiome Module Server
#' @import shiny
mod_core_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      if (!check_microtable(rv)) return()
      updateSelectInput(session, "core_group", choices = c("None" = "", get_sample_cols(rv)))
    })

    observeEvent(input$run_core, {
      if (!check_microtable(rv)) return()

      result <- safe_run({
        mt <- rv$microtable
        group <- if (isTRUE(nzchar(input$core_group))) input$core_group else NULL

        # Calculate relative abundance per taxon
        otu_rel <- sweep(mt$otu_table, 2, colSums(mt$otu_table), "/")
        tax_table <- mt$tax_table

        if (input$core_taxrank %in% colnames(tax_table)) {
          tax_col <- input$core_taxrank
          taxa_names <- tax_table[[tax_col]]
          # Aggregate by taxrank
          if (is.null(rownames(otu_rel))) rownames(otu_rel) <- paste0("Tax", seq_len(nrow(otu_rel)))
          tax_df <- data.frame(Taxa = taxa_names[match(rownames(otu_rel), rownames(tax_table))],
                               otu_rel, stringsAsFactors = FALSE)
          agg <- stats::aggregate(. ~ Taxa, data = tax_df, FUN = mean, na.rm = TRUE)
          rownames(agg) <- agg$Taxa

          # Core: taxa present in >= freq% of samples with mean abund >= threshold
          presence <- (otu_rel > 0) * 1
          tax_pres <- data.frame(Taxa = taxa_names[match(rownames(presence), rownames(tax_table))], presence, stringsAsFactors = FALSE)
          pres_agg <- stats::aggregate(. ~ Taxa, data = tax_pres, FUN = mean)
          rownames(pres_agg) <- pres_agg$Taxa
          pres_agg$Taxa <- NULL
          pres_agg$mean_prevalence <- rowMeans(pres_agg > 0)

          core_idx <- pres_agg$mean_prevalence >= (input$core_freq / 100) &
                      agg[, 2] >= input$core_abund

          list(core_taxa = rownames(pres_agg)[core_idx],
               core_data = agg[core_idx, , drop = FALSE],
               presence = pres_agg,
               n_total = nrow(pres_agg))
        } else {
          stop("\u6307\u5b9a\u7684\u5206\u7c7b\u6c34\u5e73\u4e0d\u5b58\u5728")
        }
      }, "\u6838\u5fc3\u5fae\u751f\u7269\u7ec4\u8ba1\u7b97\u5931\u8d25")

      if (result$success) {
        rv$core_result <- result$result
        n_core <- length(result$result$core_taxa)
        showNotification(paste0("\u2705 \u53d1\u73b0 ", n_core, " \u4e2a\u6838\u5fc3\u5206\u7c7b\u5355\u5143"), type = "message")

        code <- paste0(
          '# \u6838\u5fc3\u5fae\u751f\u7269\u7ec4: \u51fa\u73b0\u9891\u6b21 >= ', input$core_freq, '%, \u5e73\u5747\u76f8\u5bf9\u4e30\u5ea6 >= ', input$core_abund, '\n',
          'otu_rel <- sweep(tmp_microtable$otu_table, 2, colSums(tmp_microtable$otu_table), "/")\n',
          '# \u7b5b\u9009\u6838\u5fc3\u5206\u7c7b\u5355\u5143...\n'
        )
        append_code(rv, code, "\u6838\u5fc3\u5fae\u751f\u7269\u7ec4\u8ba1\u7b97")
      } else {
        showNotification(result$error, type = "error", duration = 10)
      }
    })

    output$core_plot <- shiny::renderPlot({
      req(rv$core_result)
      tryCatch({
        cd <- rv$core_result$core_data
        cd_long <- tidyr::pivot_longer(cd, cols = -1, names_to = "Sample", values_to = "Abundance")
        cd_long$Abundance[is.na(cd_long$Abundance)] <- 0

        top <- cd_long %>%
          dplyr::group_by(Taxa) %>%
          dplyr::summarise(Mean = mean(Abundance, na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(dplyr::desc(Mean)) %>%
          dplyr::slice_head(n = 20)

        cd_long$Taxa <- factor(cd_long$Taxa, levels = rev(top$Taxa))

        ggplot2::ggplot(cd_long, ggplot2::aes(x = Taxa, y = Abundance, fill = Taxa)) +
          ggplot2::geom_boxplot(outlier.size = 0.3, alpha = 0.7) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = "\u6838\u5fc3\u5fae\u751f\u7269\u7ec4\u5206\u5e03", x = NULL, y = "Relative Abundance") +
          ggplot2::theme_bw(base_size = 11) +
          ggplot2::theme(legend.position = "none")
      }, error = function(e) NULL)
    })

    observeEvent(input$run_venn, {
      if (!check_microtable(rv)) return()
      if (!isTRUE(nzchar(input$core_group))) {
        showNotification("\u8bf7\u9009\u62e9\u5206\u7ec4\u5217\u7528\u4e8e Venn \u5206\u6790", type = "warning")
        return()
      }

      result <- safe_run({
        group <- input$core_group
        mt <- rv$microtable
        groups <- unique(as.character(mt$sample_table[[group]]))
        groups <- groups[!is.na(groups)]

        if (length(groups) < 2 || length(groups) > 5) {
          stop("Venn \u5206\u6790\u9700\u8981 2-5 \u4e2a\u5206\u7ec4")
        }

        mt_merged <- mt$merge_samples(group = group)
        mt_merged$tidy_dataset()

        t_venn <- microeco::trans_venn$new(
          dataset = mt_merged,
          ratio = "numratio"
        )
        t_venn
      }, "Venn \u5206\u6790\u5931\u8d25")

      if (result$success) {
        rv$venn_result <- result$result
        showNotification("\u2705 Venn \u5206\u6790\u5b8c\u6210", type = "message")

        code <- paste0(
          't_venn <- trans_venn$new(\n',
          '  dataset = tmp_microtable,\n',
          '  group = "', input$core_group, '"\n)\n',
          't_venn$plot_bar()\n'
        )
        append_code(rv, code, "Venn \u56fe\u5206\u6790")
      } else {
        showNotification(result$error, type = "error", duration = 10)
      }
    })

    output$venn_plot <- shiny::renderPlot({
      req(rv$venn_result)
      tryCatch(rv$venn_result$plot_bar(), error = function(e) NULL)
    })

    output$core_table <- DT::renderDataTable({
      req(rv$core_result)
      DT::datatable(rv$core_result$core_data, options = list(scrollX = TRUE), class = "compact stripe")
    })
  })
}
