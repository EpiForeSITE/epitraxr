# Create monthly averages internal report from an EpiTrax object

`epitrax_ireport_monthly_avgs` generates an internal report of monthly
averages for all years in the EpiTrax object data, with the option to
exclude the current report year.

## Usage

``` r
epitrax_ireport_monthly_avgs(epitrax, exclude.report.year = FALSE)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- exclude.report.year:

  Logical indicating whether to exclude the current report year from the
  report. Defaults to FALSE.

## Value

Updated EpiTrax object with monthly averages report added to the
`internal_reports` field.

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
 epitrax_ireport_monthly_avgs()
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
#> [1] "monthly_avgs_2019-2024"
```
