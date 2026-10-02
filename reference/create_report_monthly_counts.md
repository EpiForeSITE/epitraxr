# Create monthly counts report

'create_report_monthly_counts' generates a data frame of monthly case
counts for each disease for a specific year, with months as columns.

## Usage

``` r
create_report_monthly_counts(data, diseases, y)
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

## Value

Dataframe of monthly counts with one row per disease and one column per
month (Jan through Dec).

## Examples

``` r
data <- data.frame(
  disease = c("A", "A", "B", "B"),
  year = c(2024, 2024, 2024, 2023),
  month = c(1, 2, 1, 4),
  counts = c(5, 7, 8, 9)
)
create_report_monthly_counts(data, diseases = c("A", "B", "C"), y = 2024)
#>   disease Jan Feb
#> 1       A   5   7
#> 2       B   8   0
#> 3       C   0   0
```
