# Compute the report trend

'compute_trend' compares values of two columns and produces a new column
containing the trend result. The trend is represented by one of three
values:

- "Elevated": increase from baseline

- "Less Than Expected": decrease from baseline

- "Expected": no change from baseline

## Usage

``` r
compute_trend(current, historical, threshold = 0)
```

## Arguments

- current:

  List. Current data.

- historical:

  List. Historical comparison data.

- threshold:

  Numeric. Percentage threshold (as decimal) for determining trend
  significance. Values within this percentage of the historical value
  are considered "Expected". Defaults to 0.0 (any difference triggers
  trend).

## Value

Character vector containing the trend labels.

## Examples

``` r
# Without threshold - any difference triggers trend
compute_trend(c(5, 10, 10), c(3, 10, 11))
#> [1] "Elevated"           "Expected"           "Less Than Expected"

# With 15% threshold - small changes are "Expected"
compute_trend(c(5, 10, 10), c(3, 10, 11), threshold = 0.15)
#> [1] "Elevated" "Expected" "Expected"
```
