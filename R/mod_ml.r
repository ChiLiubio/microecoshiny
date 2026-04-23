#' @title Machine Learning Module UI
#' @description
#' Provides interface for machine learning classification and regression analysis
#' using trans_classifier class. Supports complete parameter options including
#' data preprocessing, feature selection, model training, prediction, confusion
#' matrix, ROC/PR curves, and multi-model comparison.
#' @param id Module ID
#' @return Shiny UI tagList
#' @import shiny bs4Dash shinyWidgets
#' @importFrom shinyFiles getVolumes parseDirPath shinyDirButton shinyDirChoose
#' @keywords machine learning classification regression microbiome
#' @family advanced-analysis
mod_ml_ui <- function(id, lang = "zh") {
  ns <- NS(id)
  tr <- function(zh, en) if (lang == "en") en else zh
  tagList(
    fluidRow(column(12, h2(tr("\U0001f916 机器学习 Machine Learning", "\U0001f916 Machine Learning")))),
    fluidRow(
      column(12,
        bs4Dash::box(
          title = tr("\U0001f4cb 参数设置", "\U0001f4cb Parameters"),
          status = "primary", solidHeader = TRUE, width = NULL, collapsible = FALSE,
          h4(tr("基本参数", "Basic Parameters")),
          fluidRow(
            column(3, shiny::selectInput(ns("ml_group"), tr("y.response (目标变量)", "y.response (target variable)"),
              choices = character(0))),
            column(3, shiny::selectInput(ns("ml_x_predictors"), tr("x.predictors (特征等级)", "x.predictors (feature level)"),
              choices = c("Genus", "all", "OTU", "Phylum", "Class", "Order", "Family"),
              selected = "Genus")),
            column(3, shiny::numericInput(ns("ml_n_cores"), "n.cores", value = 1, min = 1, max = 16)),
            column(3)
          ),
          hr(),
          h4(tr("分析步骤", "Analysis Steps")),
          fluidRow(
            column(12, shiny::radioButtons(ns("ml_analysis_step"), tr("分析步骤", "Analysis Steps"),
              choices = setNames(c("split", "preprocess", "feature", "train", "predict", "caretlist"),
                                 c(tr("1. 数据分割", "1. Data Split"), tr("2. 预处理", "2. Preprocess"),
                                   tr("3. 特征选择", "3. Feature Selection"), tr("4. 训练模型", "4. Train Model"),
                                   tr("5. 预测与评估", "5. Predict & Evaluate"), tr("6. 多模型比较", "6. Multi-model Comparison"))),
              selected = "split", inline = TRUE))
          ),
          shiny::conditionalPanel(condition = "input.ml_analysis_step == 'split'", ns = ns,
            h4(paste0("cal_split ", tr("参数", "Parameters"))),
            fluidRow(
              column(4, shiny::numericInput(ns("ml_prop_train"), tr("prop.train (训练集比例)", "prop.train (train set ratio)"),
                value = 0.75, min = 0.5, max = 0.95, step = 0.05)),
              column(4, shinyWidgets::materialSwitch(ns("ml_use_all_data"), tr("use_all_data (无分割)", "use_all_data (no split)"),
                value = FALSE, status = "info")),
              column(4)
            )
          ),
          shiny::conditionalPanel(condition = "input.ml_analysis_step == 'preprocess'", ns = ns,
            h4(paste0("cal_preProcess ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("ml_pre_center"), "center", value = TRUE, status = "primary")),
              column(3, shinyWidgets::materialSwitch(ns("ml_pre_scale"), "scale", value = TRUE, status = "info")),
              column(3, shinyWidgets::materialSwitch(ns("ml_pre_nzv"), tr("nzv (移除近零变异)", "nzv (remove near-zero variance)"), value = FALSE, status = "warning")),
              column(3, shinyWidgets::materialSwitch(ns("ml_pre_pca"), "pca", value = FALSE, status = "success"))
            ),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("ml_pre_yj"), tr("yjTrans (Yeo-Johnson变换)", "yjTrans (Yeo-Johnson)"), value = FALSE, status = "info")),
              column(3, shinyWidgets::materialSwitch(ns("ml_pre_range"), "range", value = FALSE, status = "primary")),
              column(6)
            )
          ),
          shiny::conditionalPanel(condition = "input.ml_analysis_step == 'feature'", ns = ns,
            h4(paste0("cal_feature_sel (Boruta) ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shiny::numericInput(ns("ml_boruta_maxRuns"), "boruta.maxRuns",
                value = 300, min = 100, max = 500)),
              column(3, shiny::numericInput(ns("ml_boruta_pValue"), "boruta.pValue",
                value = 0.01, min = 0.001, max = 0.1, step = 0.001)),
              column(3, shiny::numericInput(ns("ml_boruta_rep"), "boruta.repetitions",
                value = 4, min = 1, max = 10)),
              column(3)
            )
          ),
          shiny::conditionalPanel(condition = "input.ml_analysis_step == 'train' || input.ml_analysis_step == 'predict'", ns = ns,
            h4(paste0("set_trainControl ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shiny::selectInput(ns("ml_tc_method"), "method",
                choices = c("repeatedcv", "cv", "boot", "LOOCV", "LGOCV", "none"),
                selected = "repeatedcv")),
              column(3, shiny::numericInput(ns("ml_tc_number"), tr("number (折数)", "number (folds)"),
                value = 5, min = 2, max = 20)),
              column(3, shiny::numericInput(ns("ml_tc_repeats"), "repeats",
                value = 3, min = 1, max = 10)),
              column(3, shinyWidgets::materialSwitch(ns("ml_tc_classProbs"), "classProbs",
                value = TRUE, status = "info"))
            ),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("ml_tc_savePredictions"), "savePredictions",
                value = TRUE, status = "success")),
              column(9)
            ),
            h4(paste0("cal_train ", tr("参数", "Parameters"))),
            fluidRow(
              column(2, shiny::selectInput(ns("ml_train_method"), "method",
                choices = c("rf" = "rf", "SVM Radial" = "svmRadial", "SVM Linear" = "svmLinear",
                  "Logistic" = "logreg", "XGBoost" = "xgbLinear", "LASSO" = "lasso",
                  "ElasticNet" = "enet", "Gradient Boosting" = "gbm", "Naive Bayes" = "nb",
                  "K-Nearest" = "knn", "CART" = "rpart", "Boosted" = "bstTree"),
                selected = "rf")),
              column(2, shiny::numericInput(ns("ml_max_mtry"), tr("max.mtry (最大森林参数)", "max.mtry (max trees)"),
                value = 2, min = 1, max = 10)),
              column(2, shiny::numericInput(ns("ml_ntree"), tr("ntree (树的数量)", "ntree (number of trees)"),
                value = 500, min = 100, max = 2000, step = 100)),
              column(2, shiny::numericInput(ns("ml_tuneLength"), "tuneLength",
                value = 3, min = 1, max = 15)),
              column(2, shinyWidgets::materialSwitch(ns("ml_rf_sig"), tr("rf_feature_sig (随机树显著性)", "rf_feature_sig (RF significance)"),
                value = FALSE, status = "warning")),
              column(2)
            ),
            h4(paste0("plot_feature_imp ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shiny::selectInput(ns("ml_imp_rf_sig_show"), "rf_sig_show",
                choices = setNames(c("", "MeanDecreaseAccuracy", "MeanDecreaseGini", "%IncMSE", "IncNodePurity", "Significance"),
                                   c(tr("自动", "Auto"), "MeanDecreaseAccuracy", "MeanDecreaseGini", "%IncMSE", "IncNodePurity", "Significance")),
                selected = "")),
              column(3, shinyWidgets::materialSwitch(ns("ml_imp_show_sig_group"), "show_sig_group",
                value = FALSE, status = "info")),
              column(2, shiny::numericInput(ns("ml_imp_use_number"), tr("use_number (特征数)", "use_number (number of features)"),
                value = 20, min = 1, max = 100)),
              column(2, shinyWidgets::materialSwitch(ns("ml_imp_coord_flip"), "coord_flip",
                value = TRUE, status = "success")),
              column(2)
            )
          ),
          shiny::conditionalPanel(condition = "input.ml_analysis_step == 'predict'", ns = ns,
            h4(paste0("cal_predict ", tr("参数", "Parameters"))),
            fluidRow(
              column(4, shiny::selectInput(ns("ml_positive_class"), tr("positive_class (正相类别)", "positive_class (positive class)"),
                choices = character(0))),
              column(4, shiny::selectInput(ns("ml_roc_input"), "cal_ROC input",
                choices = c("pred" = "pred", "train" = "train"), selected = "pred")),
              column(4)
            ),
            h4(paste0("plot_confusionMatrix ", tr("参数", "Parameters"))),
            fluidRow(
              column(3, shinyWidgets::materialSwitch(ns("ml_plot_confusion"), "plot_confusion",
                value = TRUE, status = "primary")),
              column(3, shinyWidgets::materialSwitch(ns("ml_plot_statistics"), "plot_statistics",
                value = TRUE, status = "info")),
              column(6)
            ),
            h4(paste0("plot_ROC ", tr("参数", "Parameters"))),
            fluidRow(
              column(2, shiny::selectInput(ns("ml_roc_plot_type"), "plot_type",
                choices = c("ROC", "PR"), selected = "ROC")),
              column(2, shiny::selectInput(ns("ml_roc_plot_group"), "plot_group",
                choices = c("all", "add"), selected = "all")),
              column(2, shiny::selectInput(ns("ml_roc_color"), "color_values",
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"),
                selected = "Dark2")),
              column(2, shinyWidgets::materialSwitch(ns("ml_roc_add_auc"), "add_AUC",
                value = TRUE, status = "success")),
              column(2, shinyWidgets::materialSwitch(ns("ml_roc_plot_method"), "plot_method",
                value = FALSE, status = "info")),
              column(2)
            ),
            fluidRow(
              column(2, shiny::numericInput(ns("ml_roc_line_size"), "line_size",
                value = 1, min = 0.5, max = 3, step = 0.1)),
              column(2, shiny::numericInput(ns("ml_roc_line_alpha"), "line_alpha",
                value = 1, min = 0.1, max = 1, step = 0.1)),
              column(2, shiny::selectInput(ns("ml_roc_line_type"), "line_type",
                choices = c("solid" = 1, "dashed" = 2, "dotted" = 3), selected = "1")),
              column(6)
            )
          ),
          shiny::conditionalPanel(condition = "input.ml_analysis_step == 'caretlist'", ns = ns,
            h4(paste0("cal_caretList ", tr("参数 (多模型比较)", "Parameters (Multi-model Comparison)"))),
            fluidRow(
              column(4, shiny::selectInput(ns("ml_caret_methods"), tr("methodList (模型列表)", "methodList (model list)"),
                choices = c("rf,svmRadial" = "rf,svmRadial", "rf,svmLinear,logreg" = "rf,svmLinear,logreg",
                  "rf,xgbLinear,gbm" = "rf,xgbLinear,gbm", "rf,knn,nb" = "rf,knn,nb",
                  "rf,svmRadial,xgbLinear" = "rf,svmRadial,xgbLinear"),
                selected = "rf,svmRadial")),
              column(4, shiny::selectInput(ns("ml_caret_color"), "color_values",
                choices = c("Dark2", "Set1", "Set2", "Set3", "Paired", "Spectral", "Viridis"),
                selected = "Dark2")),
              column(4)
            )
          ),
          hr(),
          h4(tr("图片保存与下载", "Save & Download Plot")),
          fluidRow(
            column(2, shiny::actionButton(ns("run_ml"), tr("\U0001f4ca 执行", "\U0001f4ca Run"),
              icon = icon("play"), class = "btn-primary", width = "100%")),
            column(2, shiny::selectInput(ns("ml_image_format"), tr("格式", "Format"),
              choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"), selected = "png")),
            column(1, shiny::numericInput(ns("ml_save_width"), tr("宽", "Width"), value = 10, min = 4, max = 20)),
            column(1, shiny::numericInput(ns("ml_save_height"), tr("高", "Height"), value = 8, min = 4, max = 15)),
            column(2, shiny::numericInput(ns("ml_save_dpi"), "DPI", value = 300, min = 72, max = 600, step = 72)),
            column(2, shiny::actionButton(ns("ml_save_plot_btn"), tr("\U0001f4e5保存图片", "\U0001f4e5Save Plot"),
              icon = icon("save"), class = "btn-outline-secondary", width = "100%"))
          ),
          fluidRow(
            column(2, shiny::selectInput(ns("ml_table_download_type"), tr("下载类型", "Download Type"),
              choices = setNames(c("metrics", "feature", "confusion", "caretlist"),
                                 c(tr("评估指标", "Metrics"), tr("特征重要性", "Feature Importance"),
                                   tr("混淆矩阵", "Confusion Matrix"), tr("模型比较", "Model Comparison"))),
              selected = "metrics")),
            column(2, shiny::selectInput(ns("ml_table_format"), tr("表格", "Table"),
              choices = c("CSV" = ",", "TSV" = "\t"), selected = ",")),
            column(2, shiny::downloadButton(ns("ml_download_table"), tr("\U0001f4e5表格下载", "\U0001f4e5Download Table"),
              class = "btn-outline-info", width = "100%")),
            column(6)
          )
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 图区", "\U0001f4ca Plot Area"), status = "info", solidHeader = TRUE, width = NULL,
          shinycssloaders::withSpinner(shiny::plotOutput(ns("ml_plot"), height = "550px"))
        )
      )
    ),
    fluidRow(
      column(12,
        bs4Dash::box(title = tr("\U0001f4ca 结果表", "\U0001f4ca Results Table"), status = "secondary", solidHeader = TRUE, width = NULL,
          DT::dataTableOutput(ns("ml_table"))
        )
      )
    )
  )
}

mod_ml_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(
      plot = NULL,
      data_result = NULL,
      save_dir = NULL,
      result_obj = NULL,
      table_type = "metrics",
      step_completed = list(split = FALSE, preprocess = FALSE, feature = FALSE,
                            train = FALSE, predict = FALSE, caretlist = FALSE)
    )

    volumes <- c(
      Home      = path.expand("~"),
      Desktop   = file.path(path.expand("~"), "Desktop"),
      Documents = file.path(path.expand("~"), "Documents"),
      get_volumes_safe()
    )
    shinyFiles::shinyDirChoose(input, "ml_save_dir_btn", roots = volumes,
      session = session, defaultRoot = "Desktop")

    observeEvent(input$ml_save_dir_btn, {
      parsed <- shinyFiles::parseDirPath(volumes, input$ml_save_dir_btn)
      if (!is.null(parsed) && length(parsed) > 0 && nchar(parsed) > 0) {
        local_rv$save_dir <- parsed
        updateTextInput(session, "ml_save_dir_text", value = parsed)
      }
    })

    observeEvent(input$ml_save_plot_btn, {
      default_name <- paste0("ml_", input$ml_analysis_step, ".", input$ml_image_format)
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
                shiny::textInput(ns("ml_save_dir_text"), label = NULL, value = dir_display,
                  placeholder = "\u8bf7\u70b9\u51fb\u53f3\u4fa7\u6309\u94ae\u9009\u62e9\u6587\u4ef6\u5939...",
                  width = "100%")
              ),
              column(3, style = "padding-top: 0;",
                shinyFiles::shinyDirButton(ns("ml_save_dir_btn"), "\u6d4f\u89c8",
                  "\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939",
                  class = "btn-outline-primary", icon = icon("folder-open"),
                  style = "margin-top: 25px;")
              )
            )
          )
        ),
        tags$hr(style = "margin: 12px 0;"),
        fluidRow(
          column(6, shiny::textInput(ns("ml_save_filename"), "\U0001f4dd \u6587\u4ef6\u540d\u79f0",
            value = default_name)),
          column(6, tags$div(style = "padding-top: 28px;",
            tags$p(class = "text-muted", style = "margin-bottom: 0;",
              tags$span(icon("info-circle")), " \u683c\u5f0f: ",
              tags$code(toupper(input$ml_image_format)),
              " | \u5bbd\u00d7\u9ad8: ",
              tags$code(paste0(input$ml_save_width, "\u00d7", input$ml_save_height)),
              " | DPI: ", tags$code(input$ml_save_dpi))
          ))
        ),
        footer = tagList(
          shiny::actionButton(ns("ml_confirm_save"), "\u2705 \u4fdd\u5b58\u5230\u6587\u4ef6\u5939",
            icon = icon("save"), class = "btn-primary"),
          shiny::modalButton("\u53d6\u6d88")
        )
      ))
    })

    observeEvent(input$ml_confirm_save, {
      req(local_rv$plot)
      save_dir <- local_rv$save_dir
      if (is.null(save_dir) || !isTRUE(nchar(save_dir) > 0)) {
        showNotification("\u8bf7\u5148\u9009\u62e9\u4fdd\u5b58\u6587\u4ef6\u5939", type = "warning")
        return()
      }
      fname <- input$ml_save_filename
      if (is.null(fname) || !isTRUE(nzchar(trimws(fname)))) {
        fname <- paste0("ml_", input$ml_analysis_step)
      }
      fname <- trimws(fname)
      ext <- input$ml_image_format
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
          width = input$ml_save_width, height = input$ml_save_height,
          units = "in", dpi = input$ml_save_dpi, scale = 1)
        removeModal()
        showNotification(paste0("\u2705 \u5df2\u4fdd\u5b58\u81f3: ", full_path), type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste0("\u4fdd\u5b58\u5931\u8d25: ", e$message), type = "error", duration = 10)
      })
    })

    observe({
      if (!check_microtable(rv)) {
        updateSelectInput(session, "ml_group", choices = character(0))
        updateSelectInput(session, "ml_positive_class", choices = character(0))
        return()
      }
      cols <- get_sample_cols(rv)
      updateSelectInput(session, "ml_group", choices = cols)
    })

    observe({
      if (!check_microtable(rv)) return()
      if (!isTRUE(nzchar(input$ml_group))) return()
      col_data <- rv$microtable$sample_table[, input$ml_group]
      if (is.numeric(col_data)) {
        updateSelectInput(session, "ml_positive_class", choices = character(0))
      } else {
        vals <- unique(col_data)
        updateSelectInput(session, "ml_positive_class", choices = c("\u81ea\u52a8" = "", vals))
      }
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

    observeEvent(input$run_ml, {
      if (!check_microtable(rv)) {
        showNotification("\u8bf7\u5148\u5bfc\u5165\u6570\u636e", type = "error")
        return()
      }

      if (!isTRUE(nzchar(input$ml_group))) {
        showNotification("\u8bf7\u9009\u62e9\u76ee\u6807\u53d8\u91cf", type = "warning")
        return()
      }

      step <- input$ml_analysis_step
      result <- tryCatch({
        dataset_name <- rv$microtable_name %||% "tmp_microtable"

        init_code <- paste0(
          "t_cl <- microeco::trans_classifier$new(\n",
          "  dataset = ", dataset_name, ",\n",
          "  y.response = \"", input$ml_group, "\",\n",
          "  x.predictors = \"", input$ml_x_predictors, "\",\n",
          "  n.cores = ", input$ml_n_cores, "\n",
          ")\n"
        )

        if (step == "split") {
          if (is.null(rv$classifier_obj)) {
            t_cl <- microeco::trans_classifier$new(
              dataset = rv$microtable,
              y.response = input$ml_group,
              x.predictors = input$ml_x_predictors,
              n.cores = input$ml_n_cores
            )
          } else {
            t_cl <- rv$classifier_obj
          }

          if (!input$ml_use_all_data) {
            t_cl$cal_split(prop.train = input$ml_prop_train)
            split_code <- paste0(
              "t_cl$cal_split(prop.train = ", input$ml_prop_train, ")\n"
            )
          } else {
            message("使用全部数据进行训练，不执行数据分割")
            split_code <- "# 使用全部数据进行训练\n# t_cl$cal_split() 未执行\n"
          }
          
          rv$classifier_obj <- t_cl
          local_rv$step_completed$split <- TRUE

          code <- paste0(init_code, "# 1. 数据分割\n", split_code)

          list(success = TRUE, plot = NULL, data_result = NULL,
               result_obj = t_cl, code = code, step = "split")

        } else if (step == "preprocess") {
          if (is.null(rv$classifier_obj)) {
            showNotification("\u8bf7\u5148\u6267\u884c\u6570\u636e\u5206\u5272", type = "warning")
            return()
          }
          t_cl <- rv$classifier_obj

          preprocess_methods <- c()
          if (input$ml_pre_center) preprocess_methods <- c(preprocess_methods, "center")
          if (input$ml_pre_scale) preprocess_methods <- c(preprocess_methods, "scale")
          if (input$ml_pre_nzv) preprocess_methods <- c(preprocess_methods, "nzv")
          if (input$ml_pre_pca) preprocess_methods <- c(preprocess_methods, "pca")
          if (input$ml_pre_yj) preprocess_methods <- c(preprocess_methods, "YeoJohnson")
          if (input$ml_pre_range) preprocess_methods <- c(preprocess_methods, "range")

          if (length(preprocess_methods) == 0) {
            showNotification("\u8bf7\u81f3\u5c11\u9009\u62e9\u4e00\u79cd\u9884\u5904\u7406\u65b9\u6cd5", type = "warning")
            return()
          }

          t_cl$cal_preProcess(method = preprocess_methods)
          rv$classifier_obj <- t_cl
          local_rv$step_completed$preprocess <- TRUE

          preprocess_code <- paste0(
            "t_cl$cal_preProcess(method = c(\"", paste(preprocess_methods, collapse = "\", \""), "\"))\n"
          )
          code <- paste0(init_code, "# 2. \u9884\u5904\u7406\n", preprocess_code)

          list(success = TRUE, plot = NULL, data_result = NULL,
               result_obj = t_cl, code = code, step = "preprocess")

        } else if (step == "feature") {
          if (is.null(rv$classifier_obj)) {
            showNotification("\u8bf7\u5148\u6267\u884c\u6570\u636e\u5206\u5272", type = "warning")
            return()
          }
          t_cl <- rv$classifier_obj

          t_cl$cal_feature_sel(
            boruta.maxRuns = input$ml_boruta_maxRuns,
            boruta.pValue = input$ml_boruta_pValue,
            boruta.repetitions = input$ml_boruta_rep
          )
          rv$classifier_obj <- t_cl
          local_rv$step_completed$feature <- TRUE

          feature_code <- paste0(
            "t_cl$cal_feature_sel(\n",
            "  boruta.maxRuns = ", input$ml_boruta_maxRuns, ",\n",
            "  boruta.pValue = ", input$ml_boruta_pValue, ",\n",
            "  boruta.repetitions = ", input$ml_boruta_rep, "\n",
            ")\n"
          )
          code <- paste0(init_code, "# 3. \u7279\u5f81\u9009\u62e9\n", feature_code)

          list(success = TRUE, plot = NULL, data_result = NULL,
               result_obj = t_cl, code = code, step = "feature")

        } else if (step == "train") {
          if (is.null(rv$classifier_obj)) {
            showNotification("\u8bf7\u5148\u6267\u884c\u6570\u636e\u5206\u5272", type = "warning")
            return()
          }
          t_cl <- rv$classifier_obj

          t_cl$set_trainControl(
            method = input$ml_tc_method,
            number = input$ml_tc_number,
            repeats = input$ml_tc_repeats,
            classProbs = input$ml_tc_classProbs,
            savePredictions = input$ml_tc_savePredictions
          )

          t_cl$cal_train(method = input$ml_train_method,
            max.mtry = input$ml_max_mtry,
            ntree = input$ml_ntree,
            tuneLength = input$ml_tuneLength)

          t_cl$cal_feature_imp(rf_feature_sig = input$ml_rf_sig)
          rv$classifier_obj <- t_cl
          local_rv$step_completed$train <- TRUE

          train_code <- paste0(
            "t_cl$set_trainControl(\n",
            "  method = \"", input$ml_tc_method, "\",\n",
            "  number = ", input$ml_tc_number, ",\n",
            "  repeats = ", input$ml_tc_repeats, ",\n",
            "  classProbs = ", input$ml_tc_classProbs, ",\n",
            "  savePredictions = ", input$ml_tc_savePredictions, "\n",
            ")\n",
            "t_cl$cal_train(\n",
            "  method = \"", input$ml_train_method, "\",\n",
            "  max.mtry = ", input$ml_max_mtry, ",\n",
            "  ntree = ", input$ml_ntree, ",\n",
            "  tuneLength = ", input$ml_tuneLength, "\n",
            ")\n",
            "t_cl$cal_feature_imp(rf_feature_sig = ", input$ml_rf_sig, ")\n"
          )
          code <- paste0(init_code, "# 4. \u8bad\u7ec3\u6a21\u578b\n", train_code)

          p <- t_cl$plot_feature_imp(
            rf_sig_show = if (nchar(input$ml_imp_rf_sig_show)) input$ml_imp_rf_sig_show else NULL,
            show_sig_group = input$ml_imp_show_sig_group,
            use_number = seq_len(min(input$ml_imp_use_number, nrow(t_cl$res_feature_imp))),
            coord_flip = input$ml_imp_coord_flip
          )

          list(success = TRUE, plot = p, data_result = t_cl$res_train$results,
               result_obj = t_cl, code = code, step = "train")

        } else if (step == "predict") {
          if (is.null(rv$classifier_obj) || is.null(rv$classifier_obj$res_train)) {
            showNotification("\u8bf7\u5148\u8bad\u7ec3\u6a21\u578b", type = "warning")
            return()
          }
          t_cl <- rv$classifier_obj

          positive_val <- if (nchar(input$ml_positive_class)) input$ml_positive_class else NULL
          t_cl$cal_predict(positive_class = positive_val)

          t_cl$cal_ROC(input = input$ml_roc_input)

          p1 <- t_cl$plot_confusionMatrix(
            plot_confusion = input$ml_plot_confusion,
            plot_statistics = input$ml_plot_statistics
          )

          p2 <- t_cl$plot_ROC(
            plot_type = input$ml_roc_plot_type,
            plot_group = input$ml_roc_plot_group,
            color_values = get_color_palette(input$ml_roc_color),
            add_AUC = input$ml_roc_add_auc,
            plot_method = input$ml_roc_plot_method,
            size = input$ml_roc_line_size,
            alpha = input$ml_roc_line_alpha,
            linetype = as.integer(input$ml_roc_line_type)
          )

          combined_plot <- gridExtra::grid.arrange(p1, p2, nrow = 1, ncol = 2)

          rv$classifier_obj <- t_cl
          local_rv$step_completed$predict <- TRUE

          predict_code <- paste0(
            "t_cl$cal_predict(\n",
            "  positive_class = ", if (!is.null(positive_val)) paste0("\"", positive_val, "\"") else "NULL", "\n",
            ")\n",
            "t_cl$cal_ROC(input = \"", input$ml_roc_input, "\")\n",
            "p1 <- t_cl$plot_confusionMatrix()\n",
            "p2 <- t_cl$plot_ROC()\n"
          )
          code <- paste0(init_code, "# 5. \u9884\u6d4b\u4e0e\u8bc4\u4f30\n", predict_code)

          list(success = TRUE, plot = combined_plot,
               data_result = t_cl$res_confusion_stats,
               result_obj = t_cl, code = code, step = "predict")

        } else if (step == "caretlist") {
          if (is.null(rv$classifier_obj)) {
            showNotification("\u8bf7\u5148\u6267\u884c\u6570\u636e\u5206\u5272", type = "warning")
            return()
          }
          t_cl <- rv$classifier_obj

          methods_vec <- unlist(strsplit(input$ml_caret_methods, ","))
          t_cl$cal_caretList(methodList = methods_vec)
          t_cl$cal_caretList_resamples()

          p <- t_cl$plot_caretList_resamples(
            color_values = get_color_palette(input$ml_caret_color)
          )

          rv$classifier_obj <- t_cl
          local_rv$step_completed$caretlist <- TRUE

          caretlist_code <- paste0(
            "t_cl$cal_caretList(methodList = c(\"", paste(methods_vec, collapse = "\", \""), "\"))\n",
            "t_cl$cal_caretList_resamples()\n",
            "p <- t_cl$plot_caretList_resamples()\n"
          )
          code <- paste0(init_code, "# 6. \u591a\u6a21\u578b\u6bd4\u8f83\n", caretlist_code)

          list(success = TRUE, plot = p,
               data_result = t_cl$res_caretList_resamples_reshaped,
               result_obj = t_cl, code = code, step = "caretlist")

        } else {
          stop("\u672a\u77e5\u5206\u6790\u6b65\u9aa4")
        }
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
      })

      if (!isTRUE(result$success)) {
        showNotification(result$error, type = "error", duration = 10)
        return()
      }

      append_code(rv, result$code, paste0("\u673a\u5668\u5b66\u4e60 - ", result$step))
      local_rv$plot <- result$plot
      local_rv$data_result <- result$data_result
      local_rv$result_obj <- result$result_obj
      local_rv$table_type <- switch(result$step,
        "train" = "metrics",
        "predict" = "confusion",
        "caretlist" = "caretlist",
        "feature" = "feature",
        "metrics"
      )
      rv$last_plot <- result$plot
      showNotification("\u5b8c\u6210", type = "message")
    })

    output$ml_plot <- shiny::renderPlot({
      req(local_rv$plot)
      if (is(local_rv$plot, "ggplot")) {
        print(local_rv$plot)
      } else if (is(local_rv$plot, "gtable")) {
        grid::grid.draw(local_rv$plot)
      } else {
        print(local_rv$plot)
      }
    })

    output$ml_table <- DT::renderDataTable({
      obj <- local_rv$result_obj
      table_type <- local_rv$table_type

      if (is.null(obj) || !is(obj, "trans_classifier")) return(NULL)

      dt <- switch(table_type,
        "metrics" = {
          if (!is.null(obj$res_train$results)) obj$res_train$results else NULL
        },
        "confusion" = {
          if (!is.null(obj$res_confusion_stats)) as.data.frame(obj$res_confusion_stats) else NULL
        },
        "feature" = {
          if (!is.null(obj$res_feature_imp)) as.data.frame(obj$res_feature_imp) else NULL
        },
        "caretlist" = {
          if (!is.null(obj$res_caretList_resamples_reshaped)) obj$res_caretList_resamples_reshaped else NULL
        },
        NULL
      )

      if (is.data.frame(dt) || is.matrix(dt)) {
        dt_df <- as.data.frame(dt)
        DT::datatable(dt_df, options = list(scrollX = TRUE, pageLength = 20),
          rownames = FALSE, filter = "top")
      }
    })

    output$ml_download_table <- downloadHandler(
      filename = function() {
        type <- input$ml_table_download_type
        ext <- ifelse(input$ml_table_format == ",", ".csv", ".tsv")
        paste0("ml_", type, ext)
      },
      content = function(file) {
        obj <- local_rv$result_obj
        if (is.null(obj) || !is(obj, "trans_classifier")) return()

        table_type <- input$ml_table_download_type
        dt <- switch(table_type,
          "metrics" = {
            if (!is.null(obj$res_train$results)) obj$res_train$results else NULL
          },
          "confusion" = {
            if (!is.null(obj$res_confusion_stats)) as.data.frame(obj$res_confusion_stats) else NULL
          },
          "feature" = {
            if (!is.null(obj$res_feature_imp)) as.data.frame(obj$res_feature_imp) else NULL
          },
          "caretlist" = {
            if (!is.null(obj$res_caretList_resamples_reshaped)) obj$res_caretList_resamples_reshaped else NULL
          },
          NULL
        )

        if (!is.null(dt) && (is.data.frame(dt) || is.matrix(dt))) {
          write.table(as.data.frame(dt), file, sep = input$ml_table_format,
            row.names = FALSE, quote = TRUE)
        }
      }
    )
  })
}