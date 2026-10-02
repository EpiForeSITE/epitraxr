# Launch the epitraxr Shiny Application

`run_app` launches the interactive Shiny web application for EpiTrax
data analysis and report generation. The app provides a user-friendly
interface for uploading data, configuring reports, and generating
various types of disease surveillance reports.

## Usage

``` r
run_app(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`shiny::shinyAppDir()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Value

Starts the execution of the app, printing the port to the console.

## Examples

``` r
if (interactive() & requireNamespace("shiny")) {
  run_app()
}
#> Loading required namespace: shiny
```
