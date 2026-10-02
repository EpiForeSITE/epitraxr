# Validate input EpiTrax data

'validate_data' checks the data for expected columns and data types,
removes unneeded columns, and returns the resulting data. Missing or NA
values will be removed with a warning. Valid data must include the
following columns (and types):

- `patient_mmwr_year` (integer)

- `patient_mmwr_week` (integer)

- `patient_disease` (character)

## Usage

``` r
validate_data(data)
```

## Arguments

- data:

  Dataframe. EpiTrax data to validate.

## Value

The validated data with all unneeded columns removed.

## Examples

``` r
df <- data.frame(
  patient_mmwr_year = 2020L,
  patient_mmwr_week = 1L,
  patient_disease = "A"
)
validate_data(df)
#>   patient_mmwr_year patient_mmwr_week patient_disease
#> 1              2020                 1               A
```
