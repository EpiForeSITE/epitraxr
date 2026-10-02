# Reshape data with each month as a separate column

'reshape_monthly_wide' reshapes a given data frame with diseases for
rows and months for columns.

## Usage

``` r
reshape_monthly_wide(data)
```

## Arguments

- data:

  Dataframe. Must contain columns:

  - `disease` (character)

  - `month` (integer)

  - `counts` (integer)

## Value

The reshaped data frame.

## Examples

``` r
df <- data.frame(
  disease = c("A", "B"),
  month = c(1, 2),
  counts = c(5, 6)
)
reshape_monthly_wide(df)
#>   disease Jan Feb
#> 1       A   5   0
#> 3       B   0   6
```
