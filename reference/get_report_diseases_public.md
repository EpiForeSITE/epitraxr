# Get the public disease list

'get_report_diseases_public' reads the public list from a given CSV file
or uses the default diseases if the file doesn't exist.

## Usage

``` r
get_report_diseases_public(filepath, defaults)
```

## Arguments

- filepath:

  Filepath. Public disease list CSV file.

- defaults:

  String vector. List of default diseases to use if the above file
  doesn't exist.

## Value

A dataframe containing the diseases to include in the public report and
the name to use for each disease in the public report. It may also
contain the disease groupings.

## Details

The provided public disease list file must contain two columns named
`EpiTrax_name` and `Public_name` which map EpiTrax disease names to a
public-facing name for the public report. The file can optionally
contain a column named `Group_name`, which maps the diseases in
`EpiTrax_name` to a disease group. This is only used for reports that
include disease groupings.

See the example file here:
`system.file("sample_data/sample_disease_list.csv", package = "epitraxr")`

## Examples

``` r
# Using default list (when file doesn't exist)
default_list <- c("Measles", "Chickenpox")
disease_list <- get_report_diseases_public("", default_list)
#> Warning: You have not provided a disease list for public reports.
#>  - The program will default to using only the diseases found in the input dataset.
#>  - If you would like to use a different list, please include a file with columns named
#> 
#>  'EpiTrax_name' and 'Public_name'

# Using a disease list file
list_file <- system.file("sample_data/sample_disease_list.csv",
                        package = "epitraxr")
disease_list <- get_report_diseases_public(list_file, default_list)
```
