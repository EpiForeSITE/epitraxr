# Get both internal and public disease lists

`get_report_diseases` is a convenience function that combines
`get_report_diseases_internal` and `get_report_diseases_public`.

## Usage

``` r
get_report_diseases(internal, public, defaults)
```

## Arguments

- internal:

  Filepath. Path to internal disease list CSV file.

- public:

  Filepath. Path to public disease list CSV file.

- defaults:

  String vector. List of default diseases to use if either file doesn't
  exist.

## Value

A list with two elements:

- `internal`: Dataframe with EpiTrax_name column

- `public`: Dataframe with EpiTrax_name and Public_name columns

## See also

[`get_report_diseases_internal()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases_internal.md),
[`get_report_diseases_public()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases_public.md)
which this function wraps.

## Examples

``` r
# Using default lists (when files don't exist)
default_list <- c("Measles", "Chickenpox")
disease_lists <- get_report_diseases("", "", default_list)
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

# Using disease list files
i_file <- system.file("tinytest/test_files/disease_lists/internal_list.csv",
                       package = "epitraxr")
p_file <- system.file("tinytest/test_files/disease_lists/public_list.csv",
                       package = "epitraxr")
disease_lists <- get_report_diseases(
  internal = i_file,
  public = p_file,
  defaults = default_list
)
```
