# Create monthly cross-section reports from an EpiTrax object

`epitrax_preport_month_crosssections` generates monthly cross-section
reports. These compare the counts for a given month against the monthly
averages for the same month across previous years.

## Usage

``` r
epitrax_preport_month_crosssections(epitrax, month_offsets = 0:3)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- month_offsets:

  Numeric vector of month offsets to create reports for. Defaults to
  0:3, which generates reports for the current month and the three
  previous months.

## Value

Updated EpiTrax object with monthly cross-section reports added to the
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
 epitrax_preport_month_crosssections(month_offsets = 0:1)
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
#> [1] "public_report_Dec2024" "public_report_Nov2024"
```
