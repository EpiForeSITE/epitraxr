# Write grouped statistics reports from EpiTrax object to PDF files

`epitrax_write_pdf_grouped_stats` writes the grouped statistics reports
from an EpiTrax object to PDF files using a formatted template. It
processes both internal and public grouped statistics reports.

## Usage

``` r
epitrax_write_pdf_grouped_stats(epitrax, params, fsys, trend.only = FALSE)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- params:

  List. Report parameters containing:

  - `title`: Report title (defaults to "Grouped Report")

- fsys:

  Filesystem list containing paths for internal and public reports.

- trend.only:

  Logical. Whether to show only trend in the PDF report.

## Value

The original EpiTrax object, unchanged.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example not run because it requires LaTeX installation

  fsys <- list(
    internal = file.path(tempdir(), "internal_reports"),
    public = file.path(tempdir(), "public_reports"),
    settings = file.path(tempdir(), "report_settings")
  )
  fsys <- setup_filesystem(fsys)

  data_file <- system.file("sample_data/sample_epitrax_data.csv",
                          package = "epitraxr")
  config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
                            package = "epitraxr")
  disease_lists <- list(
    internal = "use_defaults",
    public = "use_defaults"
  )

  params <- list(
    title = "Monthly Grouped Disease Statistics"
  )

  epitrax <- setup_epitrax(
    filepath = data_file,
    config_file = config_file,
    disease_list_files = disease_lists
  ) |>
    epitrax_report_grouped_stats() |>
    epitrax_write_pdf_grouped_stats(params = params, fsys = fsys)

  # Cleanup
  unlink(unlist(fsys, use.names = FALSE), recursive = TRUE)
} # }
```
