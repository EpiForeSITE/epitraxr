# Write reports from EpiTrax object to CSV files

`epitrax_write_csvs` writes the internal and public reports from an
EpiTrax object to CSV files in the specified filesystem. Doesn't write
files if the EpiTrax config setting `generate_csvs` is set to false.

## Usage

``` r
epitrax_write_csvs(epitrax, fsys)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- fsys:

  Filesystem list containing paths for internal and public reports.

## Value

The original EpiTrax object, unchanged.

## Examples

``` r
fsys <- list(
  internal = file.path(tempdir(), "internal_reports"),
  public = file.path(tempdir(), "public_reports"),
  settings = file.path(tempdir(), "report_settings")
)
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
 epitrax_preport_ytd_rates() |>
 epitrax_write_csvs(fsys = fsys)
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

# Cleanup
unlink(unlist(fsys, use.names = FALSE), recursive = TRUE)
```
