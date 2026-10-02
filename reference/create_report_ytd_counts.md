# Create year-to-date (YTD) counts report

'create_report_ytd_counts' generates a data frame of year-to-date counts
for each disease up to the given month, comparing the given year to the
average of other years.

## Usage

``` r
create_report_ytd_counts(data, diseases, y, m, config, as.rates = FALSE)
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

- y:

  Integer. Report year

- m:

  Integer. Report month (1-12)

- config:

  List. Report settings

- as.rates:

  Logical. If TRUE, returns rates per 100k instead of raw counts

## Value

Dataframe with one row per disease and columns for current YTD and
average YTD values (either counts or rates per 100k)

## Details

Uses the following config options:

- `current_population`

- `avg_5yr_population`

- `rounding_decimals`

## See also

[`epitraxr_config()`](https://epiforesite.github.io/epitraxr/reference/epitraxr_config.md)
for config options

## Examples

``` r
data <- data.frame(
  disease = c("A", "A", "B", "B"),
  year = c(2024, 2023, 2024, 2023),
  month = c(1, 1, 2, 2),
  counts = c(10, 20, 15, 25)
)
config <- list(
  current_population = 100000,
  avg_5yr_population = 100000,
  rounding_decimals = 1
)
create_report_ytd_counts(data, c("A", "B", "C"), 2024, 2, config)
#>   disease Current_YTD_Counts Avg_5yr_YTD_Counts
#> 1       A                 10                 20
#> 2       B                 15                 25
#> 3       C                  0                  0
```
