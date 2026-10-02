# Convert MMWR week to calendar month

`mmwr_week_to_month` calculates the calendar month from the
`patient_mmwr_week` and `patient_mmwr_year` fields of the EpiTrax data.
The result is stored in the `month` column and the `patient_mmwr_week`
column is removed.

## Usage

``` r
mmwr_week_to_month(data)
```

## Arguments

- data:

  Dataframe. Must contain columns:

  - `patient_mmwr_year` (integer)

  - `patient_mmwr_week` (integer)

## Value

The input data frame with an added "month" column (integer 1-12) and
removed `patient_mmwr_week` column.

## Examples

``` r
df <- data.frame(
  patient_mmwr_year = 2020L,
  patient_mmwr_week = 1L,
  patient_disease = "A"
)
mmwr_week_to_month(df)
#>   patient_mmwr_year patient_disease month
#> 1              2020               A     1
```
