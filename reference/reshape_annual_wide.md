# Reshape data with each year as a separate column

'reshape_annual_wide' reshapes a given data frame with diseases for rows
and years for columns.

## Usage

``` r
reshape_annual_wide(data)
```

## Arguments

- data:

  Dataframe. Must have columns:

  - `disease` (character)

  - `year` (integer)

  - `counts` (integer)

## Value

The reshaped data frame.

## Examples

``` r
df <- data.frame(
  disease = c("A", "A", "B"),
  year = c(2020, 2021, 2020),
  counts = c(5, 7, 8)
)
reshape_annual_wide(df)
#>   disease 2020 2021
#> 1       A    5    7
#> 3       B    8    0
```
