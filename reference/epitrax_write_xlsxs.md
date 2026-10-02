# Write reports from EpiTrax object to Excel files

`epitrax_write_xlsxs` writes the internal and public reports from an
EpiTrax object to Excel files in the specified filesystem. Combines all
internal reports into one Excel file with separate sheets for each
report. Likewise with public reports.

## Usage

``` r
epitrax_write_xlsxs(epitrax, fsys)
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
fsys <- setup_filesystem(fsys)

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
 epitrax_write_xlsxs(fsys = fsys)
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
