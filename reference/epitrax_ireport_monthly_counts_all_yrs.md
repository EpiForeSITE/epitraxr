# Create monthly counts internal report for all years from an EpiTrax object

`epitrax_ireport_monthly_counts_all_yrs` generates internal reports of
monthly counts for each year in the EpiTrax object data.

## Usage

``` r
epitrax_ireport_monthly_counts_all_yrs(epitrax)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

## Value

Updated EpiTrax object with monthly counts reports for each year added
to the `internal_reports` field.

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
 epitrax_ireport_monthly_counts_all_yrs()
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
#> [1] "monthly_counts_2019" "monthly_counts_2020" "monthly_counts_2021"
#> [4] "monthly_counts_2022" "monthly_counts_2023" "monthly_counts_2024"
```
