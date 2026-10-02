# Set NA values to 0

'set_na_0' sets NA values to 0 in a data frame.

## Usage

``` r
set_na_0(df)
```

## Arguments

- df:

  Dataframe.

## Value

Dataframe with NA values replaced by 0.

## Examples

``` r
df <- data.frame(year = c(2020, NA, 2022))
set_na_0(df)
#>   year
#> 1 2020
#> 2    0
#> 3 2022
```
