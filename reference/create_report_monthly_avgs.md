# Create monthly averages report

'create_report_monthly_avgs' generates a data frame of average monthly
case counts for each disease across all years in the input data.

## Usage

``` r
create_report_monthly_avgs(data, diseases, config)
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

- config:

  List. Report settings

## Value

Dataframe of monthly averages with one row per disease and one column
per month (Jan through Dec).

## Details

Uses the following config options:

- `rounding_decimals`

## See also

[`epitraxr_config()`](https://epiforesite.github.io/epitraxr/reference/epitraxr_config.md)
for config options

## Examples

``` r
data <- data.frame(
  disease = c("A", "A", "B", "B"),
  year = c(2023, 2024, 2023, 2024),
  month = c(1, 1, 2, 2),
  counts = c(10, 20, 15, 25)
)
config <- list(rounding_decimals = 1)
create_report_monthly_avgs(data, c("A", "B", "C"), config)
#>   disease Jan Feb
#> 1       A  15   0
#> 2       B   0  20
#> 3       C   0   0
```
