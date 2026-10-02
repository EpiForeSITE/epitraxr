# Create annual counts internal report from an EpiTrax object

`epitrax_ireport_annual_counts` generates an internal report of annual
counts for each disease in the EpiTrax object data.

## Usage

``` r
epitrax_ireport_annual_counts(epitrax)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

## Value

Updated EpiTrax object with `annual_counts` added to the
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
 epitrax_ireport_annual_counts()
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

epitrax$internal_reports$annual_counts
#>      disease 2019 2020 2021 2022 2023 2024
#> 1   COVID-19 1014 1627 2398 1855  908 1191
#> 2 Chickenpox  218  318  263  234  249  292
#> 3  Influenza  625 1733 1889 2289 1664 1466
#> 4    Measles  211  326  292  414  586  304
#> 5   Syphilis  278  356  439  398  577  269
```
