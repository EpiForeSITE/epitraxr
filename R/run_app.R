library(shiny)

run_app <- function(...) {
  shinyAppDir(
    system.file("app/", package="epitraxr"),
    ...
  )
}