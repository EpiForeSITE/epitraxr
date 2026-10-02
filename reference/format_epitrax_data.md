# Format EpiTrax data for report generation

`format_epitrax_data` prepares the input EpiTrax data for use by report
generation functions in the package. It adds the `counts` column,
renames columns to standard names used by the package ("disease",
"month", "year", "counts"), and rearranges columns for consistency.

## Usage

``` r
format_epitrax_data(data)
```

## Arguments

- data:

  Dataframe. Must contain columns:

  - `patient_disease` (character, unchanged from EpiTrax export)

  - `patient_mmwr_year` (integer, unchanged from EpiTrax export)

  - `month` (integer, converted from `patient_mmwr_week` by
    [`mmwr_week_to_month()`](https://epiforesite.github.io/epitraxr/reference/mmwr_week_to_month.md))

## Value

A standardized data frame with columns "disease", "month", "year", and
"counts".

## See also

[`mmwr_week_to_month()`](https://epiforesite.github.io/epitraxr/reference/mmwr_week_to_month.md)

## Examples

``` r
df <- data.frame(
  patient_mmwr_year = c(2020L, 2020L),
  month = c(1, 2),
  patient_disease = c("A", "B")
)
df <- format_epitrax_data(df)
```
