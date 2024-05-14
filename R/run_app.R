
#' Run the Shiny App
#'
#' @inheritParams shiny::shinyApp
#' @inherit shiny::runApp description
#' @return An object that implements the Shiny App.
#' @export
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/"
) {

  pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

  shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = onStart,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )

}
