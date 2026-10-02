# Create year-to-date (YTD) medians report from an EpiTrax object

`epitrax_report_ytd_medians` generates a report of median year-to-date
counts for each disease up to the current report month across all years
in the EpiTrax object data, with the option to exclude the current
report year. It can be run for either internal or public reports.

## Usage

``` r
epitrax_report_ytd_medians(
  epitrax,
  is.public = FALSE,
  exclude.report.year = FALSE
)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- is.public:

  Logical indicating whether to generate a public report using the
  public disease list. If FALSE (default), generates an internal report
  using the internal disease list.

- exclude.report.year:

  Logical indicating whether to exclude the current report year from the
  report. Defaults to FALSE.

## Value

Updated EpiTrax object with YTD medians report added to either the
`internal_reports` or `public_reports` field, depending on the
`is.public` parameter.

## Examples

``` r
data_file <- system.file("sample_data/sample_epitrax_data.csv",
                         package = "epitraxr")
config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
                           package = "epitraxr")
disease_lists <- list(
  internal = "use_defaults",
  public = "use_defaults"
)

epitrax <- setup_epitrax(
  filepath = data_file,
  config_file = config_file,
  disease_list_files = disease_lists
) |>
 epitrax_report_ytd_medians()
#> Warning: You have not provided a disease list for internal reports.
#>  - The program will default to using only the diseases found in the input dataset.
#>  - If you would like to use a different list, please include a file with a column named
#> 
#>  'EpiTrax_name'
#> Warning: You have not provided a disease list for public reports.
#>  - The program will default to using only the diseases found in the input dataset.
#>  - If you would like to use a different list, please include a file with columns named
#> 
#>  'EpiTrax_name' and 'Public_name'

names(epitrax$internal_reports)
#> [1] "ytd_medians_2019-2024"
```
