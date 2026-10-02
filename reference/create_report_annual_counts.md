# Create annual counts report

'create_report_annual_counts' generates a data frame of annual case
counts for each disease, with years as columns.

## Usage

``` r
create_report_annual_counts(data, diseases)
```

## Arguments

- data:

  Dataframe. Input data with columns:

  - `disease` (character)

  - `year` (integer)

  - `month` (integer)

  - `counts` (integer)

- diseases:

  Character vector. Diseases to include in the report

## Value

Dataframe of annual counts with one row per disease and one column per
year.

## Examples

``` r
data <- data.frame(
  disease = c("A", "A", "B"),
  year = c(2020, 2021, 2020),
  counts = c(5, 7, 8)
)
create_report_annual_counts(data, diseases = c("A", "B", "C"))
#>   disease 2020 2021
#> 1       A    5    7
#> 2       B    8    0
#> 3       C    0    0
```
