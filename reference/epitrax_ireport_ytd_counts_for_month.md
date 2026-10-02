# Create year-to-date (YTD) counts internal report for a given month from an EpiTrax object

`epitrax_ireport_ytd_counts_for_month` generates an internal report of
year-to-date counts up to a specific month in the EpiTrax object data.

## Usage

``` r
epitrax_ireport_ytd_counts_for_month(epitrax, as.rates = FALSE)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- as.rates:

  Logical. If TRUE, returns rates per 100k instead of raw counts.

## Value

Updated EpiTrax object with report added to the `internal_reports`
field.

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
 epitrax_ireport_ytd_counts_for_month(as.rates = TRUE)
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
#> [1] "ytd_rates"
```
