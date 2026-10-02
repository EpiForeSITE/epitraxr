# Create year-to-date (YTD) rates public report from an EpiTrax object

`epitrax_preport_ytd_rates` generates a public report of year-to-date
rates for the current month in the EpiTrax object data.

## Usage

``` r
epitrax_preport_ytd_rates(epitrax)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

## Value

Updated EpiTrax object with YTD rates report added to the
`public_reports` field.

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
 epitrax_preport_ytd_rates()
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

names(epitrax$public_reports)
#> [1] "public_report_YTD"
```
