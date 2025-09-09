#' Launch the epitraxr Shiny Application
#'
#' `run_app` launches the interactive Shiny web application for
#' EpiTrax data analysis and report generation. The app provides a user-friendly
#' interface for uploading data, configuring reports, and generating various
#' types of disease surveillance reports.
#'
#' @param ... Additional arguments passed to `shiny::shinyAppDir()`.
#'
#' @returns Starts the execution of the app, printing the port
#' to the console.
#' @export
#'
#' @examples
#' if (interactive() & requireNamespace("shiny")) {
#'   run_app()
#' }
run_app <- function(...) {
  shiny::shinyAppDir(
    system.file("app/", package="epitraxr"),
    ...
  )
}
