# Get monthly counts for each disease

`get_month_counts` aggregates disease counts by month and year. This is
a helper function used internally by report generation functions to
summarize monthly disease counts.

## Usage

``` r
get_month_counts(data)
```

## Arguments

- data:

  Dataframe. Must contain columns:

  - `disease` (character)

  - `year` (integer)

  - `month` (integer)

  - `counts` (integer)

## Value

A dataframe with the aggregated monthly counts

## Examples

``` r
df <- data.frame(
  disease = c("Flu", "Flu", "Measles"),
  year = c(2020, 2020, 2020),
  month = c(1, 1, 2),
  counts = c(5, 3, 2)
)
get_month_counts(df)
#>   disease year month counts
#> 1     Flu 2020     1      8
#> 2 Measles 2020     2      2
```
