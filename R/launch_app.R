#' Launch the Agrivoltaics Statistics Shiny App
#'
#' Opens the agrivoltaics statistical analysis interface in your browser.
#'
#' @param port Port number (default: random available port)
#' @param browser Open in browser automatically (default: TRUE)
#'
#' @examples
#' \dontrun{
#' launch_app()
#' }
#'
#' @export
launch_app <- function(port = NULL, browser = TRUE) {
  app_dir <- system.file("shiny", "app", package = "agrivoltaics")
  if (app_dir == "") {
    stop("Could not find the Shiny app. Try re-installing the package with:\n",
         "  remotes::install_github(\"agronomy4future/agrivoltaics\")")
  }
  shiny::runApp(app_dir, port = port, launch.browser = browser)
}
