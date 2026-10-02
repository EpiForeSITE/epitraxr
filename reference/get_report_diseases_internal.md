# Get the internal disease list

'get_report_diseases_internal' reads the internal list from a given CSV
file or uses the default diseases, if the file doesn't exist.

## Usage

``` r
get_report_diseases_internal(filepath, defaults)
```

## Arguments

- filepath:

  Filepath. Internal disease list CSV file.

- defaults:

  String vector. List of default diseases to use if the above file
  doesn't exist.

## Value

A dataframe containing the diseases to include in the internal report
and possibly the disease groupings.

## Details

The provided internal disease list file must contain at least a column
named `EpiTrax_name` which contains EpiTrax disease names to include in
the report. The file can optionally contain a column named `Group_name`,
which maps the diseases in `EpiTrax_name` to a disease group. This is
only used for reports that include disease groupings.

See the example file here:
`system.file("sample_data/sample_disease_list.csv", package = "epitraxr")`

## Examples

``` r
# Using default list (when file doesn't exist)
default_list <- c("Measles", "Chickenpox")
disease_list <- get_report_diseases_internal("", default_list)
#> Warning: You have not provided a disease list for internal reports.
#>  - The program will default to using only the diseases found in the input dataset.
#>  - If you would like to use a different list, please include a file with a column named
#> 
#>  'EpiTrax_name'

# Using a disease list file
list_file <- system.file("sample_data/sample_disease_list.csv",
                        package = "epitraxr")
disease_list <- get_report_diseases_internal(list_file, default_list)
```
