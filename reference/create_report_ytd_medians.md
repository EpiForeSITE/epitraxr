# Create year-to-date (YTD) medians report

'create_report_ytd_medians' generates a data frame of median
year-to-date counts for each disease up to the given month (months
`1:m`) across all years in the data. This provides a robust central
tendency measure for YTD values.

## Usage

``` r
create_report_ytd_medians(data, diseases, m)
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

- m:

  Integer. Report month (1-12)

## Value

Dataframe with one row per disease and columns for disease name and
median YTD counts.

## Examples

``` r
data <- data.frame(
  disease = c("A", "A", "A", "B", "B", "B"),
  year = c(2022, 2023, 2024, 2022, 2023, 2024),
  month = c(1, 1, 2, 2, 2, 3),
  counts = c(10, 15, 20, 5, 8, 12)
)
create_report_ytd_medians(data, c("A", "B", "C"), 2)
#>   disease median_counts
#> 1       A            15
#> 2       B             5
#> 3       C             0
```
