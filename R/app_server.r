#' microecoshiny Application Server
#'
#' @import shiny
#' @importFrom shiny observeEvent
app_server <- function(input, output, session) {

  # ===== Global Reactive Values =====
  rv <- reactiveValues(
    # Core data objects
    microtable = NULL,
    microtable_name = "tmp_microtable",

    # Workspace: named list of microtable objects
    workspace = list(),  # list(name = microtable_obj, ...)
    workspace_active = NULL,  # name of currently active object

    # Normalized data variants
    norm_data = list(),
    microtable_version = 0,

    # Analysis results
    alpha_result = NULL,
    beta_result = NULL,
    beta_constrained = NULL,
    diff_result = NULL,
    env_result = NULL,
    network_result = NULL,
    nullmodel_result = NULL,
    classifier_result = NULL,
    func_result = NULL,
    core_result = NULL,
    venn_result = NULL,
    mm_result = NULL,
    pathway_result = NULL,
    metab_data = NULL,
    metagenome_data = NULL,

    # Code generation
    generated_codes = character(0),
    code_counter = 0L,

    # UI state
    data_loaded = FALSE,
    current_theme = "light",
    codes_pinned = FALSE,
    current_language = detect_default_language(),

    # Module outputs (plots, tables)
    last_plot = NULL,
    last_table = NULL
  )

  # ===== Object Browser UI Rendering =====
  output$obj_browser_ui <- renderUI({
    lang <- rv$current_language
    mod_obj_browser_ui("mod_obj_browser", lang = lang)
  })

  # ===== Sidebar Data Status =====
  output$sidebar_data_status <- renderUI({
    if (isTRUE(rv$data_loaded) && check_microtable(rv)) {
      mt <- rv$microtable
      n_samples <- nrow(mt$sample_table)
      n_features <- nrow(mt$otu_table)
      n_taxa <- nrow(mt$tax_table)
      tags$div(
        tags$span(icon("check-circle", style = "color: #27AE60;"), "\u6570\u636e\u5df2\u52a0\u8f7d"),
        tags$br(),
        tags$small(
          paste0("\u6837\u672c: ", n_samples, " | \u7279\u5f81: ", n_features, " | \u5206\u7c7b: ", n_taxa),
          style = "color: #888;"
        )
      )
    } else {
      tags$span(icon("exclamation-circle", style = "color: #E74C3C;"), "\u672a\u52a0\u8f7d\u6570\u636e")
    }
  })

  # ===== Header Message =====
  output$header_message <- renderUI({
    bs4Dash::dropdownMenu(
      type = "notifications",
      badgeStatus = "info",
      icon = icon("bell"),
      .list = NULL,
      headerText = "\u901a\u77e5 Notifications"
    )
  })

  # ===== Set default tab to Import on startup =====
  session$onFlushed(function() {
    updateTabItems(session, "sidebar_menu", "mod_import")
  }, once = TRUE)

  # ===== Theme Management =====
  observeEvent(input$theme_picker, {
    rv$current_theme <- input$theme_picker
    if (input$theme_picker == "dark" && rv$current_theme == "light") {
      session$sendCustomMessage(type = "toggleTheme", message = list())
    }
  })

  # ===== Language Management =====
  observeEvent(input$language_switch, {
    if (!is.null(input$language_switch)) {
      rv$current_language <- input$language_switch
      cat("[LangServer] Language changed to:", input$language_switch, "\n")
      # Send message to frontend to update sidebar text
      session$sendCustomMessage(type = "updateSidebarLang", message = list(lang = input$language_switch))
    }
  })

  # ===== Module Content Renderers (dynamic with language) =====
  output$mod_import_content <- renderUI({
    lang <- rv$current_language
    mod_import_ui("mod_import", lang = lang)
  })

  output$mod_preprocess_content <- renderUI({
    lang <- rv$current_language
    mod_preprocess_ui("mod_preprocess", lang = lang)
  })

  output$mod_norm_content <- renderUI({
    lang <- rv$current_language
    mod_norm_ui("mod_norm", lang = lang)
  })

  output$mod_abund_content <- renderUI({
    lang <- rv$current_language
    mod_abund_ui("mod_abund", lang = lang)
  })

  output$mod_composition_content <- renderUI({
    lang <- rv$current_language
    mod_composition_ui("mod_composition", lang = lang)
  })

  output$mod_alpha_content <- renderUI({
    lang <- rv$current_language
    mod_alpha_ui("mod_alpha", lang = lang)
  })

  output$mod_beta_content <- renderUI({
    lang <- rv$current_language
    mod_beta_ui("mod_beta", lang = lang)
  })

  output$mod_core_content <- renderUI({
    lang <- rv$current_language
    mod_core_ui("mod_core", lang = lang)
  })

  output$mod_diff_content <- renderUI({
    lang <- rv$current_language
    mod_diff_ui("mod_diff", lang = lang)
  })

  output$mod_env_content <- renderUI({
    lang <- rv$current_language
    mod_env_ui("mod_env", lang = lang)
  })

  output$mod_network_content <- renderUI({
    lang <- rv$current_language
    mod_network_ui("mod_network", lang = lang)
  })

  output$mod_nullmodel_content <- renderUI({
    lang <- rv$current_language
    mod_nullmodel_ui("mod_nullmodel", lang = lang)
  })

  output$mod_func_content <- renderUI({
    lang <- rv$current_language
    mod_func_ui("mod_func", lang = lang)
  })

  output$mod_ml_content <- renderUI({
    lang <- rv$current_language
    mod_ml_ui("mod_ml", lang = lang)
  })

  output$mod_multiomics_content <- renderUI({
    lang <- rv$current_language
    mod_multiomics_ui("mod_multiomics", lang = lang)
  })

  output$mod_export_content <- renderUI({
    lang <- rv$current_language
    mod_export_ui("mod_export", lang = lang)
  })

  output$mod_workspace_content <- renderUI({
    lang <- rv$current_language
    mod_workspace_ui("mod_workspace", lang = lang)
  })

  output$mod_codes_content <- renderUI({
    lang <- rv$current_language
    mod_codes_ui("mod_codes", lang = lang)
  })
  
  session$onSessionEnded(function() {
    NULL
  })

  # ===== Module Servers =====
  mod_workspace_server("mod_workspace", rv)
  mod_import_server("mod_import", rv)
  mod_preprocess_server("mod_preprocess", rv)
  mod_norm_server("mod_norm", rv)
  mod_abund_server("mod_abund", rv)
  mod_composition_server("mod_composition", rv)
  mod_alpha_server("mod_alpha", rv)
  mod_beta_server("mod_beta", rv)
  mod_core_server("mod_core", rv)
  mod_diff_server("mod_diff", rv)
  mod_env_server("mod_env", rv)
  mod_network_server("mod_network", rv)
  mod_nullmodel_server("mod_nullmodel", rv)
  mod_func_server("mod_func", rv)
  mod_ml_server("mod_ml", rv)
  mod_multiomics_server("mod_multiomics", rv)
  mod_export_server("mod_export", rv)
  mod_obj_browser_server("mod_obj_browser", rv)
  mod_codes_server("mod_codes", rv)

  # ===== Pinned Codes Panel =====
  output$pinned_code_editor <- renderUI({
    code_text <- paste(rv$generated_codes, collapse = "")
    theme_val <- if (rv$current_theme == "dark") "monokai" else "github"
    shinyAce::aceEditor(
      outputId = "pinned_code_output",
      value = code_text,
      mode = "r",
      theme = theme_val,
      readOnly = TRUE,
      wordWrap = TRUE,
      fontSize = 12,
      height = "100%",
      debounce = 300
    )
  })
}
