# Validate EpiTrax object

`validate_epitrax` checks that the EpiTrax object is valid.

## Usage

``` r
validate_epitrax(epitrax, report.check = TRUE)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- report.check:

  Logical indicating whether to check report-related fields.

## Value

NULL if valid, otherwise throws an error.

## Examples

``` r
epitrax <- structure(
  list(
    data = c(1,2,3),
    config = list(rounding_decimals = 2, generate_csvs = TRUE),
    report_diseases = list(internal = "internal_list", public = "public_list")
  ),
  class = "epitrax"
)
validate_epitrax(epitrax, report.check = TRUE)
```
