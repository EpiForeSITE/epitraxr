# Create monthly medians report

'create_report_monthly_medians' generates a data frame of median monthly
case counts for each disease across all years in the input data. This
provides a more robust central tendency measure compared to averages for
skewed data.

## Usage

``` r
create_report_monthly_medians(data, diseases)
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

Dataframe of monthly medians with one row per disease and one column per
month (Jan through Dec).

## Examples

``` r
data <- data.frame(
  disease = c("A", "A", "A", "B", "B", "B"),
  year = c(2022, 2023, 2024, 2022, 2023, 2024),
  month = c(1, 1, 1, 2, 2, 2),
  counts = c(10, 20, 30, 5, 15, 25)
)
create_report_monthly_medians(data, c("A", "B", "C"))
#>   disease Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 1       A  20   0   0   0   0   0   0   0   0   0   0   0
#> 2       B   0  15   0   0   0   0   0   0   0   0   0   0
#> 3       C   0   0   0   0   0   0   0   0   0   0   0   0
```
