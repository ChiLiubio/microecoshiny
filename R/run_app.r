#' Run the microecoshiny application
#'
#' @param ... Arguments passed to [shiny::shinyApp()]
#'
#' @return A shiny application object
#' @export
#' @import shiny
#' @importFrom golem with_golem_options
run_app <- function(...) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list(...)
  )
}
