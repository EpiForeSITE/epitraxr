# Set report diseases in EpiTrax object

`epitrax_set_report_diseases` reads internal and public disease lists
and adds them to the EpiTrax object.

## Usage

``` r
epitrax_set_report_diseases(epitrax, disease_list_files = NULL)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- disease_list_files:

  Optional list containing filepaths to internal and public report
  disease lists. If omitted, the default lists will be used and a
  warning will be given.

## Value

Updated EpiTrax object with `report_diseases` field set.

## See also

[`setup_epitrax()`](https://epiforesite.github.io/epitraxr/reference/setup_epitrax.md)
the convenience function which wraps this function

## Examples

``` r
i_file <- system.file("tinytest/test_files/disease_lists/internal_list.csv",
                       package = "epitraxr")
p_file <- system.file("tinytest/test_files/disease_lists/public_list.csv",
                       package = "epitraxr")

epitrax <- structure(
  list(data = c(1,2,3)),
  class = "epitrax"
)

epitrax <- epitrax_set_report_diseases(
  epitrax,
  disease_list_files = list(
    internal = i_file,
    public = p_file
  )
)
```
