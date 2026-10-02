# Write general PDF report of disease stats from R Markdown template

`write_report_pdf` renders a report as a PDF using a R Markdown
template. It is relatively flexible and can be used for various types of
report.

## Usage

``` r
write_report_pdf(data, params, filename, folder, trend.only = FALSE)
```

## Arguments

- data:

  Dataframe. Report data.

- params:

  List. Report parameters containing:

  - `title`: Report title (defaults to "Disease Report")

  - `report_year`: Report year (defaults to 2025)

  - `report_month`: Report month (defaults to 1)

  - `trend_threshold`: Threshold for trend calculations (defaults to
    0.15)

- filename:

  String. Report filename.

- folder:

  Filepath. Report destination folder.

- trend.only:

  Logical. Whether to show only trend in the PDF report. If TRUE,
  "trend_only\_" will be prepended to the filename.

## Value

NULL (called for side effects - creates the report file).

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example not run because it requires LaTeX installation

  # Create sample report data
  r_data <- data.frame(
    Disease = c("COVID", "Flu", "Measles"),
    `March 2024` = c(0, 25, 5),
    `Historical March Avg` = c(0, 15, 8),
    `Trend` = compute_trend(c(0, 25, 5), c(0, 15, 8)),
    check.names = FALSE
  )

  # Set report parameters
  params <- list(
    title = "Monthly Disease Surveillance Report",
    report_year = 2024,
    report_month = 3,
    trend_threshold = 0.20
  )

  # Write to temporary directory
  write_report_pdf(
    data = r_data,
    params = params,
    filename = "monthly_disease_report.pdf",
    folder = tempdir()
  )
} # }
```
