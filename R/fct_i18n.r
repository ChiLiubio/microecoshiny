## ============================================================================
## fct_i18n.r - Internationalization (i18n) for microecoshiny
## ============================================================================

#' Detect default language based on system locale
#'
#' This function checks the system locale and environment variables to determine
#' the user's default language preference.
#'
#' @return A character string, either "zh" (Chinese) or "en" (English)
#' @family i18n
#' @export
detect_default_language <- function() {
  saved_lang <- tryCatch({
    if (interactive()) {
      NULL
    } else {
      NULL
    }
  }, error = function(e) NULL)

  tryCatch({
    locale <- Sys.getlocale("LC_CTYPE")
    if (grepl("zh|CHN|China|Chinese|UTF-8", locale, ignore.case = TRUE)) {
      if (grepl("zh", locale, ignore.case = TRUE)) return("zh")
    }
  }, error = function(e) {})

  tryCatch({
    lang <- Sys.getenv("LANG")
    if (grepl("zh", lang, ignore.case = TRUE)) return("zh")
  }, error = function(e) {})

  return("zh")
}

#' Get translation for a key
#'
#' This function retrieves the translated text for a given key in the specified language.
#' If the key is not found, it returns the key itself as a fallback.
#'
#' @param key Translation key (character string)
#' @param lang Language code, either "zh" (Chinese) or "en" (English)
#' @return The translated text, or the key itself if not found
#' @family i18n
#' @export
tr <- function(key, lang = "zh") {
  if (is.null(lang)) lang <- "zh"
  if (is.null(translations[[lang]])) {
    return(key)
  }
  val <- translations[[lang]][[key]]
  if (is.null(val)) {
    return(key)
  }
  val
}

#' Complete translation dictionary
translations <- list(

  # ==============================
  # English translations
  # ==============================
  en = list(
    # Dashboard
    "app.title" = "microecoshiny",
    "app.header" = "\U0001f9ec microecoshiny",

    # Navigation - Main Menu
    "menu.data_mgmt" = "Data Management",
    "menu.import" = "Import",
    "menu.preprocess" = "Preprocess",
    "menu.normalization" = "Normalization",
    "menu.export" = "Export",

    "menu.basic" = "Basic Analysis",
    "menu.abundance" = "Abundance",
    "menu.composition" = "Composition",
    "menu.alpha" = "Alpha Diversity",
    "menu.beta" = "Beta Diversity",
    "menu.core" = "Core Microbiome",

    "menu.advanced" = "Advanced Analysis",
    "menu.differential" = "Differential Abundance",
    "menu.env_correlation" = "Env Correlation",
    "menu.network" = "Network Analysis",
    "menu.null_model" = "Null Model",
    "menu.functional" = "Functional Prediction",
    "menu.ml" = "Machine Learning",

    "menu.multiomics" = "Multi-omics",
    "menu.multiomics_sub" = "Multi-omics Analysis",

    "menu.codes" = "Generate Code",
    "menu.workspace" = "Workspace",

    # Sidebar footer
    "sidebar.no_data" = "No data loaded",
    "sidebar.data_loaded" = "Data loaded",

    # Controlbar / Settings
    "settings.title" = "Settings",
    "settings.auto_scroll" = "Auto-scroll Code",
    "settings.show_code" = "Show Code in Modules",
    "settings.interactive" = "Interactive Plots (plotly)",
    "settings.theme" = "Theme",
    "settings.language" = "Language",
    "settings.theme_picker" = "Select Theme",
    "theme.light" = "Light",
    "theme.dark" = "Dark",

    # Right sidebar toggle
    "sidebar.toggle_hint" = "Drag to adjust width / Click to close",

    # ==============================
    # Import Module
    # ==============================
    "mod.import.title" = "Data Import",
    "mod.import.source_tab" = "Data Source",
    "mod.import.preview_tab" = "Data Preview",
    "mod.import.files_tab" = "Imported Files",
    "mod.import.built_in" = "Built-in Data",
    "mod.import.local_file" = "Local File",
    "mod.import.local_dir" = "Local Directory",
    "mod.import.from_env" = "From Environment",
    "mod.import.file_upload" = "Upload File",
    "mod.import.file_upload_hint" = "Supports: OTU table, taxonomy, sample metadata (CSV, TSV, XLSX, TXT)",
    "mod.import.dir_path" = "Directory Path",
    "mod.import.browse" = "Browse",
    "mod.import.file_pattern" = "File Pattern",
    "mod.import.role_table" = "Table Role",
    "mod.import.role_otu" = "OTU Table",
    "mod.import.role_taxonomy" = "Taxonomy Table",
    "mod.import.role_sample" = "Sample Metadata",
    "mod.import.role_other" = "Other Table",
    "mod.import.rowname_col" = "Row Name Column",
    "mod.import.auto_detect" = "Auto Detect",
    "mod.import.import_btn" = "Import Data",
    "mod.import.file_info" = "File Info",
    "mod.import.dim" = "Dimensions",
    "mod.import.preview_data" = "Data Preview",
    "mod.import.file_list" = "Imported File List",
    "mod.import.no_files" = "No files imported yet",
    "mod.import.rdata_tab" = "RData Import",
    "mod.import.rdata_load" = "Load R Data File",
    "mod.import.rdata_select" = "Select RData or RDS file",
    "mod.import.builtin_title" = "Use Built-in Data",
    "mod.import.builtin_desc" = "Load microeco package built-in example dataset for testing and demonstration.",
    "mod.import.builtin_select" = "Select dataset",
    "mod.import.builtin_load" = "Load Data",
    "mod.import.qiime2_hint" = "QIIME2 import requires qiime2R package, currently RData import is recommended",

    # ==============================
    # Preprocess Module
    # ==============================
    "mod.preprocess.title" = "Data Preprocessing",
    "mod.preprocess.filter" = "Sample Filter",
    "mod.preprocess.filter_thres" = "Filter Threshold",
    "mod.preprocess.filter_group" = "Filter by Group",
    "mod.preprocess.filter_sample" = "Select Samples to Remove",
    "mod.preprocess.filter_taxa" = "Select Taxa to Remove",
    "mod.preprocess.sort" = "Sample Sorting",
    "mod.preprocess.sort_by" = "Sort By",
    "mod.preprocess.remove_taxa" = "Remove Taxa",
    "mod.preprocess.remove_taxa_names" = "Taxa Names to Remove",
    "mod.preprocess.run_btn" = "Run Preprocess",

    # ==============================
    # Normalization Module
    # ==============================
    "mod.norm.title" = "Data Normalization",
    "mod.norm.method" = "Normalization Method",
    "mod.norm.run_btn" = "Run Normalization",

    # ==============================
    # Object Browser
    # ==============================
    "obj.browser.title" = "Object Browser",
    "obj.browser.refresh" = "Refresh",
    "obj.browser.collapse" = "Collapse All",
    "obj.browser.expand" = "Expand All",
    "obj.browser.search" = "Search objects...",
    "obj.browser.no_objects" = "No objects",
    "obj.browser.total_objects" = "%d object(s)",
    "obj.browser.not_found" = "No objects found",
    "obj.browser.import_hint" = "Please import data first",
    "obj.browser.obj_lost" = "Object not found",
    "obj.browser.samples" = "Samples",
    "obj.browser.features" = "Features",
    "obj.browser.tax_levels" = "Taxonomic Levels",
    "obj.browser.seqs" = "Seqs",
    "obj.browser.comp_status" = "Component Status",

    # ==============================
    # Data Summary
    # ==============================
    "summary.title" = "Data Summary",
    "summary.samples" = "Samples",
    "summary.total_seqs" = "Total Seqs",
    "summary.integrity" = "Data Integrity Check",
    "summary.not_found" = "Not found",
    "summary.features_samples" = "Features x Samples",
    "summary.tax_levels" = "Taxonomic Levels",
    "summary.obj_info" = "microtable Object Info (print)",

    # ==============================
    # Export Module
    # ==============================
    "mod.export.title" = "Data Export",
    "mod.export.otutable" = "OTU Table",
    "mod.export.taxtable" = "Taxonomy Table",
    "mod.export.sampletable" = "Sample Metadata",
    "mod.export.format" = "Export Format",
    "mod.export.download" = "Download",

    # ==============================
    # Abundance Module
    # ==============================
    "mod.abund.title" = "Taxonomic Abundance",
    "mod.abund.level" = "Taxonomic Level",
    "mod.abund.group" = "Group Column",
    "mod.abund.table" = "Abundance Table",
    "mod.abund.plot" = "Abundance Plot",

    # ==============================
    # Composition Module
    # ==============================
    "mod.composition.title" = "Composition Visualization",
    "mod.composition.params" = "Parameter Settings",
    "mod.composition.basic" = "Basic Parameters",
    "mod.composition.taxrank" = "Taxonomic Level",
    "mod.composition.limit_ntaxa" = "Limit Taxa Number",
    "mod.composition.show" = "Relative Abundance Threshold",
    "mod.composition.groupmean" = "Group Mean",
    "mod.composition.use_percentage" = "Relative (use_percentage)",
    "mod.composition.delete_taxonomy_lineage" = "Delete Taxonomy Lineage",
    "mod.composition.delete_taxonomy_prefix" = "Delete Taxonomy Prefix",
    "mod.composition.prefix" = "Custom Prefix",
    "mod.composition.high_level" = "Add Higher Level",
    "mod.composition.high_level_fix_nsub" = "Number of Sub Taxa at Higher Level",
    "mod.composition.group_morestats" = "Group Statistics Details",
    "mod.composition.plot_type" = "Plot Type",
    "mod.composition.bar" = "Bar Chart",
    "mod.composition.heatmap" = "Heatmap",
    "mod.composition.pie" = "Pie Chart",
    "mod.composition.box" = "Box Plot",
    "mod.composition.line" = "Line Chart",
    "mod.composition.donut" = "Donut Chart",
    "mod.composition.bar_params" = "Bar Chart Parameters",
    "mod.composition.heatmap_params" = "Heatmap Parameters",
    "mod.composition.pie_params" = "Pie Chart Parameters",
    "mod.composition.box_params" = "Box Plot Parameters",
    "mod.composition.color_theme" = "Color Theme",
    "mod.composition.others_color" = "Others Color",
    "mod.composition.barwidth" = "Bar Width",
    "mod.composition.use_alluvium" = "Alluvium Plot",
    "mod.composition.coord_flip" = "Coordinate Flip",
    "mod.composition.order_x" = "X-axis Order",
    "mod.composition.facet" = "Facet",
    "mod.composition.clustering" = "Add Clustering",
    "mod.composition.clustering_plot" = "Add Clustering Plot",
    "mod.composition.cluster_plot_width" = "Clustering Plot Width",
    "mod.composition.facet_color" = "Facet Color",
    "mod.composition.strip_text" = "Strip Text Size",
    "mod.composition.xtext_keep" = "Keep X-axis Text",
    "mod.composition.xtitle_keep" = "Keep X-axis Title",
    "mod.composition.xtext_angle" = "X-axis Text Angle",
    "mod.composition.xtext_size" = "X-axis Text Size",
    "mod.composition.ytitle_size" = "Y-axis Title Size",
    "mod.composition.legend_text_italic" = "Legend Text Italic",
    "mod.composition.ggnested" = "ggplot Nested",
    "mod.composition.high_level_add_other" = "Higher Level Add Others",
    "mod.composition.plot" = "Composition Plot",

    # ==============================
    # Alpha Diversity Module
    # ==============================
    "mod.alpha.title" = "\u03b1 Alpha Diversity",
    "mod.alpha.params" = "Parameter Settings",
    "mod.alpha.basic" = "Basic Parameters",
    "mod.alpha.method" = "Diversity Method",
    "mod.alpha.group" = "Group Column",
    "mod.alpha.use_coverage" = "Use Coverage",
    "mod.alpha.run_btn" = "Run Calculation",
    "mod.alpha.calc_status" = "Calculation Status",
    "mod.alpha.plot_params" = "Plot Parameters",
    "mod.alpha.plot_type" = "Plot Type",
    "mod.alpha.boxplot" = "Box Plot",
    "mod.alpha.violin" = "Violin Plot",
    "mod.alpha.color_theme" = "Color Theme",
    "mod.alpha.add_sig" = "Add Significance",
    "mod.alpha.test_method" = "Test Method",
    "mod.alpha.p_adjust" = "P-value Adjustment",
    "mod.alpha.coord_flip" = "Coordinate Flip",
    "mod.alpha.xtext_angle" = "X-axis Text Angle",
    "mod.alpha.xtext_size" = "X-axis Text Size",
    "mod.alpha.ytitle_size" = "Y-axis Title Size",
    "mod.alpha.export" = "Export Settings",
    "mod.alpha.image_format" = "Format",
    "mod.alpha.width" = "Width",
    "mod.alpha.height" = "Height",
    "mod.alpha.dpi" = "DPI",
    "mod.alpha.save_plot" = "Save Image",
    "mod.alpha.download_table" = "Save Table",
    "mod.alpha.result_table" = "Statistical Results",
    "mod.alpha.plot" = "Alpha Diversity Plot",

    # ==============================
    # Beta Diversity Module
    # ==============================
    "mod.beta.title" = "\u03b2 Beta Diversity",
    "mod.beta.params" = "Parameter Settings",
    "mod.beta.basic" = "Basic Parameters",
    "mod.beta.method" = "Distance Method",
    "mod.beta.group" = "Group Column",
    "mod.beta.run_btn" = "Run Calculation",
    "mod.beta.calc_status" = "Calculation Status",
    "mod.beta.plot_params" = "Plot Parameters",
    "mod.beta.ordination" = "Ordination Method",
    "mod.beta.color_theme" = "Color Theme",
    "mod.beta.add_ellipse" = "Add Ellipse",
    "mod.beta.add_centroid" = "Add Centroid",
    "mod.beta.plot" = "Beta Diversity Plot",

    # ==============================
    # Core Microbiome Module
    # ==============================
    "mod.core.title" = "Core Microbiome",
    "mod.core.params" = "Parameter Settings",
    "mod.core.taxrank" = "Taxonomic Level",
    "mod.core.prevalence" = "Prevalence Threshold",
    "mod.core.abundance" = "Abundance Threshold",
    "mod.core.group" = "Group Column",
    "mod.core.run_btn" = "Run Analysis",
    "mod.core.result_table" = "Core Taxa Results",
    "mod.core.plot" = "Core Microbiome Plot",

    # ==============================
    # Differential Abundance Module
    # ==============================
    "mod.diff.title" = "\u2696 Differential Abundance",
    "mod.diff.params" = "Parameter Settings",
    "mod.diff.method" = "Method",
    "mod.diff.basic" = "Basic Parameters",
    "mod.diff.taxrank" = "Taxonomic Level",
    "mod.diff.group" = "Group Column",
    "mod.diff.filter_thres" = "Abundance Filter Threshold",
    "mod.diff.p_adjust" = "P-value Adjustment",
    "mod.diff.remove_unknown" = "Remove Unknown Classification",
    "mod.diff.lefse_params" = "LEfSe Parameters",
    "mod.diff.alpha" = "Significance Level",
    "mod.diff.lefse_subgroup" = "Subgroup Column",
    "mod.diff.lefse_min_subsam" = "Minimum Sample Size",
    "mod.diff.lefse_sub_strict" = "Strict Mode",
    "mod.diff.lefse_sub_alpha" = "Subgroup Significance",
    "mod.diff.lefse_norm" = "Normalization Value",
    "mod.diff.nresam" = "Sampling Ratio",
    "mod.diff.boots" = "Bootstrap Number",
    "mod.diff.rf_params" = "Random Forest Parameters",
    "mod.diff.rf_imp_type" = "Importance Type",
    "mod.diff.stat_params" = "Statistical Test Parameters",
    "mod.diff.transformation" = "Data Transformation",
    "mod.diff.by_group" = "Group Computation",
    "mod.diff.by_ID" = "Pairing Column",
    "mod.diff.formula" = "Formula",
    "mod.diff.formula_model" = "Formula Model Parameters",
    "mod.diff.beta_pseudo" = "Pseudo Count",
    "mod.diff.paired_params" = "Paired Test Parameters",
    "mod.diff.group_choose_paired" = "Paired Group Column",
    "mod.diff.metagenomeSeq_count" = "Minimum Count",
    "mod.diff.aldex2_params" = "ALDEx2 Parameters",
    "mod.diff.aldex2_sig" = "Significance Standard",
    "mod.diff.run_btn" = "Run Calculation",
    "mod.diff.calc_status" = "Calculation Status",
    "mod.diff.result_table" = "Statistical Results",
    "mod.diff.plot_params" = "Plot Settings",
    "mod.diff.plot_type" = "Plot Type",
    "mod.diff.diff_bar" = "diff_bar",
    "mod.diff.diff_abund" = "diff_abund",
    "mod.diff.volcano" = "Volcano Plot",
    "mod.diff.cladogram" = "Cladogram",
    "mod.diff.plot" = "Differential Abundance Plot",
    "mod.diff.abund_table" = "Abundance Data",

    # ==============================
    # Env Correlation Module
    # ==============================
    "mod.env.title" = "Environment Factor Correlation",
    "mod.env.params" = "Parameter Settings",
    "mod.env.taxrank" = "Taxonomic Level",
    "mod.env.env_cols" = "Environment Variables",
    "mod.env.method" = "Correlation Method",
    "mod.env.run_btn" = "Run Analysis",
    "mod.env.result_table" = "Correlation Results",
    "mod.env.plot" = "Environment Correlation Plot",

    # ==============================
    # Network Module
    # ==============================
    "mod.network.title" = "Network Analysis",
    "mod.network.params" = "Parameter Settings",
    "mod.network.taxrank" = "Taxonomic Level",
    "mod.network.method" = "Correlation Method",
    "mod.network.threshold" = "Correlation Threshold",
    "mod.network.p_value" = "P-value Threshold",
    "mod.network.run_btn" = "Run Analysis",
    "mod.network.result_table" = "Network Statistics",
    "mod.network.plot" = "Network Plot",

    # ==============================
    # Null Model Module
    # ==============================
    "mod.nullmodel.title" = "Null Model Analysis",
    "mod.nullmodel.params" = "Parameter Settings",
    "mod.nullmodel.method" = "Assembly Process Method",
    "mod.nullmodel.run_btn" = "Run Analysis",
    "mod.nullmodel.result_table" = "Null Model Results",
    "mod.nullmodel.plot" = "Null Model Plot",

    # ==============================
    # Functional Prediction Module
    # ==============================
    "mod.func.title" = "Functional Prediction",
    "mod.func.params" = "Parameter Settings",
    "mod.func.method" = "Prediction Method",
    "mod.func.run_btn" = "Run Prediction",
    "mod.func.result_table" = "Function Results",
    "mod.func.plot" = "Function Prediction Plot",

    # ==============================
    # Machine Learning Module
    # ==============================
    "mod.ml.title" = "Machine Learning",
    "mod.ml.params" = "Parameter Settings",
    "mod.ml.taxrank" = "Taxonomic Level",
    "mod.ml.group" = "Target Variable",
    "mod.ml.method" = "ML Algorithm",
    "mod.ml.run_btn" = "Run Training",
    "mod.ml.result_table" = "Model Results",
    "mod.ml.plot" = "Machine Learning Plot",

    # ==============================
    # Multi-omics Module
    # ==============================
    "mod.multiomics.title" = "Multi-omics Analysis",
    "mod.multiomics.params" = "Parameter Settings",
    "mod.multiomics.method" = "Integration Method",
    "mod.multiomics.run_btn" = "Run Analysis",
    "mod.multiomics.result_table" = "Multi-omics Results",
    "mod.multiomics.plot" = "Multi-omics Plot",

    # ==============================
    # Generate Code Module
    # ==============================
    "mod.codes.title" = "Generated Code",
    "mod.codes.params" = "Code Management",
    "mod.codes.editor" = "Code Editor",
    "mod.codes.clear" = "Clear All Code",
    "mod.codes.copy" = "Copy Code",
    "mod.codes.download" = "Download Code",
    "mod.codes.no_code" = "No code generated yet. Please run analysis in other modules first.",

    # ==============================
    # Workspace Module
    # ==============================
    "mod.workspace.title" = "Workspace Management",
    "mod.workspace.params" = "Workspace Settings",
    "mod.workspace.save" = "Save Workspace",
    "mod.workspace.load" = "Load Workspace",
    "mod.workspace.delete" = "Delete Workspace",

    # ==============================
    # Object Browser Module
    # ==============================
    "mod.objbrowser.title" = "Object Browser",

    # ==============================
    # Common / Shared UI Strings
    # ==============================
    "common.run" = "Run",
    "common.calculate" = "Calculate",
    "common.save" = "Save",
    "common.load" = "Load",
    "common.delete" = "Delete",
    "common.clear" = "Clear",
    "common.download" = "Download",
    "common.copy" = "Copy",
    "common.reset" = "Reset",
    "common.none" = "None",
    "common.auto" = "Auto",
    "common.select" = "Select",
    "common.choose" = "Choose",
    "common.apply" = "Apply",
    "common.cancel" = "Cancel",
    "common.confirm" = "Confirm",
    "common.close" = "Close",
    "common.refresh" = "Refresh",
    "common.import" = "Import",
    "common.export" = "Export",
    "common.preview" = "Preview",
    "common.browse" = "Browse",

    # Status Messages
    "status.waiting" = "Waiting to start...",
    "status.calculating" = "Calculating...",
    "status.success" = "Completed!",
    "status.error" = "Error",
    "status.calc_failed" = "Calculation failed: ",
    "status.plot_failed" = "Plot failed: ",
    "status.loading" = "Loading...",
    "status.no_data" = "Please import data first",

    # File formats
    "format.csv" = "CSV",
    "format.tsv" = "TSV",
    "format.xlsx" = "Excel",
    "format.txt" = "Text",
    "format.pdf" = "PDF",
    "format.png" = "PNG",
    "format.svg" = "SVG",
    "format.tiff" = "TIFF"
  ),

  # ==============================
  # Chinese translations
  # ==============================
  zh = list(
    # Dashboard
    "app.title" = "microecoshiny",
    "app.header" = "\U0001f9ec microecoshiny",

    # Navigation - Main Menu
    "menu.data_mgmt" = "数据管理",
    "menu.import" = "数据导入",
    "menu.preprocess" = "数据预处理",
    "menu.normalization" = "数据标准化",
    "menu.export" = "数据导出",

    "menu.basic" = "基础分析",
    "menu.abundance" = "类群丰富度",
    "menu.composition" = "群落组成",
    "menu.alpha" = "\u03b1多样性",
    "menu.beta" = "\u03b2多样性",
    "menu.core" = "核心微生物组",

    "menu.advanced" = "高级分析",
    "menu.differential" = "差异丰度",
    "menu.env_correlation" = "环境因子关联",
    "menu.network" = "网络分析",
    "menu.null_model" = "零模型",
    "menu.functional" = "功能预测",
    "menu.ml" = "机器学习",

    "menu.multiomics" = "多组学",
    "menu.multiomics_sub" = "宏基因组与代谢组",

    "menu.codes" = "生成代码",
    "menu.workspace" = "工作区",

    # Sidebar footer
    "sidebar.no_data" = "未加载数据",
    "sidebar.data_loaded" = "数据已加载",

    # Controlbar / Settings
    "settings.title" = "设置",
    "settings.auto_scroll" = "自动滚动代码",
    "settings.show_code" = "在各模块显示代码",
    "settings.interactive" = "交互式图表 (plotly)",
    "settings.theme" = "主题",
    "settings.language" = "语言",
    "settings.theme_picker" = "选择主题",
    "theme.light" = "浅色",
    "theme.dark" = "深色",

    # Right sidebar toggle
    "sidebar.toggle_hint" = "拖拽调整宽度/点击关闭",

    # ==============================
    # Import Module
    # ==============================
    "mod.import.title" = "数据导入",
    "mod.import.source_tab" = "数据源",
    "mod.import.preview_tab" = "数据预览",
    "mod.import.files_tab" = "已导入文件",
    "mod.import.built_in" = "内置数据",
    "mod.import.local_file" = "本地文件",
    "mod.import.local_dir" = "本地目录",
    "mod.import.from_env" = "从环境导入",
    "mod.import.file_upload" = "上传文件",
    "mod.import.file_upload_hint" = "支持格式：OTU表、分类表、样本元数据（CSV、TSV、XLSX、TXT）",
    "mod.import.dir_path" = "目录路径",
    "mod.import.browse" = "浏览",
    "mod.import.file_pattern" = "文件匹配模式",
    "mod.import.role_table" = "表格角色",
    "mod.import.role_otu" = "OTU 表",
    "mod.import.role_taxonomy" = "分类表",
    "mod.import.role_sample" = "样本元数据",
    "mod.import.role_other" = "其他表",
    "mod.import.rowname_col" = "行名列",
    "mod.import.auto_detect" = "自动识别",
    "mod.import.import_btn" = "导入数据",
    "mod.import.file_info" = "文件信息",
    "mod.import.dim" = "维度",
    "mod.import.preview_data" = "数据预览",
    "mod.import.file_list" = "已导入文件列表",
    "mod.import.no_files" = "暂无导入文件",
    "mod.import.rdata_tab" = "RData 导入",
    "mod.import.rdata_load" = "加载 R 数据文件",
    "mod.import.rdata_select" = "选择 RData 或 rds 文件",
    "mod.import.builtin_title" = "使用内置数据",
    "mod.import.builtin_desc" = "加载 microeco 包内置的示例数据集，用于测试和演示。",
    "mod.import.builtin_select" = "选择数据集",
    "mod.import.builtin_load" = "加载数据",
    "mod.import.qiime2_hint" = "QIIME2 导入需要 qiime2R 包，当前建议使用 RData 导入",

    # ==============================
    # Preprocess Module
    # ==============================
    "mod.preprocess.title" = "数据预处理",
    "mod.preprocess.filter" = "样本过滤",
    "mod.preprocess.filter_thres" = "过滤阈值",
    "mod.preprocess.filter_group" = "按组过滤",
    "mod.preprocess.filter_sample" = "选择要删除的样本",
    "mod.preprocess.filter_taxa" = "选择要删除的分类单元",
    "mod.preprocess.sort" = "样本排序",
    "mod.preprocess.sort_by" = "排序依据",
    "mod.preprocess.remove_taxa" = "删除分类单元",
    "mod.preprocess.remove_taxa_names" = "要删除的分类单元名称",
    "mod.preprocess.run_btn" = "执行预处理",

    # ==============================
    # Normalization Module
    # ==============================
    "mod.norm.title" = "数据标准化",
    "mod.norm.method" = "标准化方法",
    "mod.norm.run_btn" = "执行标准化",

    # ==============================
    # Object Browser
    # ==============================
    "obj.browser.title" = "对象浏览器",
    "obj.browser.refresh" = "刷新",
    "obj.browser.collapse" = "全部折叠",
    "obj.browser.expand" = "全部展开",
    "obj.browser.search" = "搜索对象...",
    "obj.browser.no_objects" = "暂无对象",
    "obj.browser.total_objects" = "共 %d 个对象",
    "obj.browser.not_found" = "未找到对象",
    "obj.browser.import_hint" = "请先导入数据",
    "obj.browser.obj_lost" = "对象已丢失",
    "obj.browser.samples" = "样本",
    "obj.browser.features" = "特征",
    "obj.browser.tax_levels" = "分类层级",
    "obj.browser.seqs" = "序列",
    "obj.browser.comp_status" = "组件状态",

    # ==============================
    # Data Summary
    # ==============================
    "summary.title" = "数据概览",
    "summary.samples" = "样本数",
    "summary.total_seqs" = "总序列数",
    "summary.integrity" = "数据完整性检查",
    "summary.not_found" = "未找到",
    "summary.features_samples" = "特征 x 样本",
    "summary.tax_levels" = "层级",
    "summary.obj_info" = "microtable 对象信息 ( print)",

    # ==============================
    # Export Module
    # ==============================
    "mod.export.title" = "数据导出",
    "mod.export.otutable" = "OTU 表",
    "mod.export.taxtable" = "分类表",
    "mod.export.sampletable" = "样本元数据",
    "mod.export.format" = "导出格式",
    "mod.export.download" = "下载",

    # ==============================
    # Abundance Module
    # ==============================
    "mod.abund.title" = "类群丰富度",
    "mod.abund.level" = "分类层级",
    "mod.abund.group" = "分组列",
    "mod.abund.table" = "丰富度表",
    "mod.abund.plot" = "丰富度图",

    # ==============================
    # Composition Module
    # ==============================
    "mod.composition.title" = "组成可视化",
    "mod.composition.params" = "参数设置",
    "mod.composition.basic" = "基本参数",
    "mod.composition.taxrank" = "分类水平",
    "mod.composition.limit_ntaxa" = "限制 Taxa 数量",
    "mod.composition.show" = "相对丰度阈值",
    "mod.composition.groupmean" = "按组求均值",
    "mod.composition.use_percentage" = "相对 (use_percentage)",
    "mod.composition.delete_taxonomy_lineage" = "删除分类层 (delete_taxonomy_lineage)",
    "mod.composition.delete_taxonomy_prefix" = "删除分类前缀 (delete_taxonomy_prefix)",
    "mod.composition.prefix" = "定制前缀 (prefix)",
    "mod.composition.high_level" = "补充更高层 (high_level)",
    "mod.composition.high_level_fix_nsub" = "更高层下的类数 (high_level_fix_nsub)",
    "mod.composition.group_morestats" = "组统计详情 (group_morestats)",
    "mod.composition.plot_type" = "图型",
    "mod.composition.bar" = "柱状图",
    "mod.composition.heatmap" = "热图",
    "mod.composition.pie" = "饼图",
    "mod.composition.box" = "箱形图",
    "mod.composition.line" = "线图",
    "mod.composition.donut" = "甜甜圈图",
    "mod.composition.bar_params" = "柱状图参数",
    "mod.composition.heatmap_params" = "热图参数",
    "mod.composition.pie_params" = "饼图参数",
    "mod.composition.box_params" = "箱形图参数",
    "mod.composition.color_theme" = "颜色",
    "mod.composition.others_color" = "其他色",
    "mod.composition.barwidth" = "柱宽",
    "mod.composition.use_alluvium" = "alluvium图",
    "mod.composition.coord_flip" = "翻转",
    "mod.composition.order_x" = "X排序",
    "mod.composition.facet" = "分面 Facet",
    "mod.composition.clustering" = "增加聚类",
    "mod.composition.clustering_plot" = "增加聚图",
    "mod.composition.cluster_plot_width" = "聚图宽",
    "mod.composition.facet_color" = "分面色",
    "mod.composition.strip_text" = "分面字大",
    "mod.composition.xtext_keep" = "X字",
    "mod.composition.xtitle_keep" = "X标题",
    "mod.composition.xtext_angle" = "X字角度",
    "mod.composition.xtext_size" = "X字大小",
    "mod.composition.ytitle_size" = "Y标题大小",
    "mod.composition.legend_text_italic" = "图例斜体",
    "mod.composition.ggnested" = "ggnested嵌套",
    "mod.composition.high_level_add_other" = "高层增加Others",
    "mod.composition.plot" = "组成图",

    # ==============================
    # Alpha Diversity Module
    # ==============================
    "mod.alpha.title" = "\u03b1 \u591a\u6837\u6027 Alpha Diversity",
    "mod.alpha.params" = "参数设置",
    "mod.alpha.basic" = "基本参数",
    "mod.alpha.method" = "多样性方法",
    "mod.alpha.group" = "分组列",
    "mod.alpha.use_coverage" = "使用覆盖率",
    "mod.alpha.run_btn" = "开始计算",
    "mod.alpha.calc_status" = "计算状态",
    "mod.alpha.plot_params" = "图表参数",
    "mod.alpha.plot_type" = "图表类型",
    "mod.alpha.boxplot" = "箱线图",
    "mod.alpha.violin" = "小提琴图",
    "mod.alpha.color_theme" = "颜色",
    "mod.alpha.add_sig" = "添加显著性",
    "mod.alpha.test_method" = "检验方法",
    "mod.alpha.p_adjust" = "p值校正",
    "mod.alpha.coord_flip" = "翻转坐标",
    "mod.alpha.xtext_angle" = "X轴文字角度",
    "mod.alpha.xtext_size" = "X轴文字大小",
    "mod.alpha.ytitle_size" = "Y轴标题大小",
    "mod.alpha.export" = "导出设置",
    "mod.alpha.image_format" = "格式",
    "mod.alpha.width" = "宽",
    "mod.alpha.height" = "高",
    "mod.alpha.dpi" = "DPI",
    "mod.alpha.save_plot" = "保存图片",
    "mod.alpha.download_table" = "表格保存",
    "mod.alpha.result_table" = "统计检验结果",
    "mod.alpha.plot" = "\u03b1多样性图",

    # ==============================
    # Beta Diversity Module
    # ==============================
    "mod.beta.title" = "\u03b2 \u591a\u6837\u6027 Beta Diversity",
    "mod.beta.params" = "参数设置",
    "mod.beta.basic" = "基本参数",
    "mod.beta.method" = "距离方法",
    "mod.beta.group" = "分组列",
    "mod.beta.run_btn" = "开始计算",
    "mod.beta.calc_status" = "计算状态",
    "mod.beta.plot_params" = "图表参数",
    "mod.beta.ordination" = "排序方法",
    "mod.beta.color_theme" = "颜色",
    "mod.beta.add_ellipse" = "添加椭圆",
    "mod.beta.add_centroid" = "添加质心",
    "mod.beta.plot" = "\u03b2多样性图",

    # ==============================
    # Core Microbiome Module
    # ==============================
    "mod.core.title" = "核心微生物组",
    "mod.core.params" = "参数设置",
    "mod.core.taxrank" = "分类层级",
    "mod.core.prevalence" = "流行度阈值",
    "mod.core.abundance" = "丰度阈值",
    "mod.core.group" = "分组列",
    "mod.core.run_btn" = "运行分析",
    "mod.core.result_table" = "核心分类结果",
    "mod.core.plot" = "核心微生物组图",

    # ==============================
    # Differential Abundance Module
    # ==============================
    "mod.diff.title" = "\u2696 \u5dee\u5f02\u4e30\u5ea6\u5206\u6790 Differential Abundance",
    "mod.diff.params" = "参数设置",
    "mod.diff.method" = "方法",
    "mod.diff.basic" = "基本参数",
    "mod.diff.taxrank" = "分类层级 (taxa_level)",
    "mod.diff.group" = "分组列 (group)",
    "mod.diff.filter_thres" = "丰度过滤阈值 (filter_thres)",
    "mod.diff.p_adjust" = "p值校正 (p_adjust_method)",
    "mod.diff.remove_unknown" = "移除未知分类 (remove_unknown)",
    "mod.diff.lefse_params" = "LEfSe 参数",
    "mod.diff.alpha" = "显著性水平 (alpha)",
    "mod.diff.lefse_subgroup" = "子组列 (lefse_subgroup)",
    "mod.diff.lefse_min_subsam" = "最小样本数 (lefse_min_subsam)",
    "mod.diff.lefse_sub_strict" = "严格模式 (lefse_sub_strict)",
    "mod.diff.lefse_sub_alpha" = "子组显著性 (lefse_sub_alpha)",
    "mod.diff.lefse_norm" = "归一化值 (lefse_norm)",
    "mod.diff.nresam" = "采样比例 (nresam)",
    "mod.diff.boots" = "自助法次数 (boots)",
    "mod.diff.rf_params" = "Random Forest 参数",
    "mod.diff.rf_imp_type" = "重要性类型 (rf_imp_type)",
    "mod.diff.stat_params" = "统计检验参数",
    "mod.diff.transformation" = "数据转换 (transformation)",
    "mod.diff.by_group" = "分组计算 (by_group)",
    "mod.diff.by_ID" = "配对列 (by_ID)",
    "mod.diff.formula" = "公式 (formula)",
    "mod.diff.formula_model" = "公式模型参数",
    "mod.diff.beta_pseudo" = "伪计数 (beta_pseudo)",
    "mod.diff.paired_params" = "配对检验参数",
    "mod.diff.group_choose_paired" = "配对组列 (group_choose_paired)",
    "mod.diff.metagenomeSeq_count" = "最小计数 (metagenomeSeq_count)",
    "mod.diff.aldex2_params" = "ALDEx2 参数",
    "mod.diff.aldex2_sig" = "显著性标准 (ALDEx2_sig)",
    "mod.diff.run_btn" = "开始计算",
    "mod.diff.calc_status" = "计算状态",
    "mod.diff.result_table" = "统计检验结果",
    "mod.diff.plot_params" = "图表设置",
    "mod.diff.plot_type" = "图表类型 (plot_type)",
    "mod.diff.diff_bar" = "diff_bar",
    "mod.diff.diff_abund" = "diff_abund",
    "mod.diff.volcano" = "volcano",
    "mod.diff.cladogram" = "cladogram",
    "mod.diff.plot" = "差异丰度图",
    "mod.diff.abund_table" = "丰度数据",

    # ==============================
    # Env Correlation Module
    # ==============================
    "mod.env.title" = "\U0001f517 \u73af\u5883\u56e0\u5b50\u5173\u8054 Env Correlation",
    "mod.env.params" = "参数设置",
    "mod.env.taxrank" = "分类层级",
    "mod.env.env_cols" = "环境变量",
    "mod.env.method" = "相关性方法",
    "mod.env.run_btn" = "运行分析",
    "mod.env.result_table" = "相关性结果",
    "mod.env.plot" = "环境因子关联图",

    # ==============================
    # Network Module
    # ==============================
    "mod.network.title" = "\U0001f578\ufe0f \u7f51\u7edc\u5206\u6790 Network Analysis",
    "mod.network.params" = "参数设置",
    "mod.network.taxrank" = "分类层级",
    "mod.network.method" = "相关性方法",
    "mod.network.threshold" = "相关性阈值",
    "mod.network.p_value" = "P值阈值",
    "mod.network.run_btn" = "运行分析",
    "mod.network.result_table" = "网络统计",
    "mod.network.plot" = "网络图",

    # ==============================
    # Null Model Module
    # ==============================
    "mod.nullmodel.title" = "\U0001f3b2 \u96f6\u6a21\u578b\u5206\u6790 Null Model",
    "mod.nullmodel.params" = "参数设置",
    "mod.nullmodel.method" = "群落组装方法",
    "mod.nullmodel.run_btn" = "运行分析",
    "mod.nullmodel.result_table" = "零模型结果",
    "mod.nullmodel.plot" = "零模型图",

    # ==============================
    # Functional Prediction Module
    # ==============================
    "mod.func.title" = "\U0001f9ec \u529f\u80fd\u9884\u6d4b Functional Prediction",
    "mod.func.params" = "参数设置",
    "mod.func.method" = "预测方法",
    "mod.func.run_btn" = "运行预测",
    "mod.func.result_table" = "功能结果",
    "mod.func.plot" = "功能预测图",

    # ==============================
    # Machine Learning Module
    # ==============================
    "mod.ml.title" = "\U0001f916 \u673a\u5668\u5b66\u4e60 Machine Learning",
    "mod.ml.params" = "参数设置",
    "mod.ml.taxrank" = "分类层级",
    "mod.ml.group" = "目标变量",
    "mod.ml.method" = "ML算法",
    "mod.ml.run_btn" = "运行训练",
    "mod.ml.result_table" = "模型结果",
    "mod.ml.plot" = "机器学习图",

    # ==============================
    # Multi-omics Module
    # ==============================
    "mod.multiomics.title" = "\U0001f9ec \u591a\u7ec4\u5b66 Multi-omics",
    "mod.multiomics.params" = "参数设置",
    "mod.multiomics.method" = "整合方法",
    "mod.multiomics.run_btn" = "运行分析",
    "mod.multiomics.result_table" = "多组学结果",
    "mod.multiomics.plot" = "多组学图",

    # ==============================
    # Generate Code Module
    # ==============================
    "mod.codes.title" = "\U0001f4dd \u751f\u6210\u4ee3\u7801 Generate Code",
    "mod.codes.params" = "代码管理",
    "mod.codes.editor" = "代码编辑器",
    "mod.codes.clear" = "清空代码",
    "mod.codes.copy" = "复制代码",
    "mod.codes.download" = "下载代码",
    "mod.codes.no_code" = "暂无生成代码，请在其他模块运行分析后查看",

    # ==============================
    # Workspace Module
    # ==============================
    "mod.workspace.title" = "\U0001f4be \u5de5\u4f5c\u533a Workspace",
    "mod.workspace.params" = "工作区设置",
    "mod.workspace.save" = "保存工作区",
    "mod.workspace.load" = "加载工作区",
    "mod.workspace.delete" = "删除工作区",

    # ==============================
    # Object Browser Module
    # ==============================
    "mod.objbrowser.title" = "对象浏览器",

    # ==============================
    # Common / Shared UI Strings
    # ==============================
    "common.run" = "运行",
    "common.calculate" = "计算",
    "common.save" = "保存",
    "common.load" = "加载",
    "common.delete" = "删除",
    "common.clear" = "清空",
    "common.download" = "下载",
    "common.copy" = "复制",
    "common.reset" = "重置",
    "common.none" = "无",
    "common.auto" = "自动",
    "common.select" = "选择",
    "common.choose" = "选择",
    "common.apply" = "应用",
    "common.cancel" = "取消",
    "common.confirm" = "确认",
    "common.close" = "关闭",
    "common.refresh" = "刷新",
    "common.import" = "导入",
    "common.export" = "导出",
    "common.preview" = "预览",
    "common.browse" = "浏览",

    # Status Messages
    "status.waiting" = "等待开始计算...",
    "status.calculating" = "开始计算...",
    "status.success" = "完成！",
    "status.error" = "错误",
    "status.calc_failed" = "计算失败: ",
    "status.plot_failed" = "绘图失败: ",
    "status.loading" = "加载中...",
    "status.no_data" = "请先导入数据",

    # File formats
    "format.csv" = "CSV",
    "format.tsv" = "TSV",
    "format.xlsx" = "Excel",
    "format.txt" = "文本",
    "format.pdf" = "PDF",
    "format.png" = "PNG",
    "format.svg" = "SVG",
    "format.tiff" = "TIFF"
  )
)
