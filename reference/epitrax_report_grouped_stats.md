# Create grouped disease statistics report from an EpiTrax object

`epitrax_report_grouped_stats` generates a comprehensive report with
current and historical statistics for diseases organized by group. The
report includes monthly counts/rates, historical averages and medians,
year-to-date counts, and trend analysis. It can be run for either
internal or public reports.

## Usage

``` r
epitrax_report_grouped_stats(epitrax, is.public = FALSE)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- is.public:

  Logical indicating whether to generate a public report using the
  public disease list. If FALSE (default), generates an internal report
  using the internal disease list.

## Value

Updated EpiTrax object with grouped statistics report added to either
the `internal_reports` or `public_reports` field, depending on the
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
 epitrax_report_grouped_stats()
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
#> Warning: The parameter 'diseases' should contain a 'Group_name' column. Since, no groups were provided, all diseases will be 'Uncategorized'.

names(epitrax$internal_reports)
#> [1] "grouped_stats_2019-2024"
```
