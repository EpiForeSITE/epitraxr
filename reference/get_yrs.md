# Get unique years from the data

'get_yrs' extracts and returns the sorted unique years from the 'year'
column of a data frame.

## Usage

``` r
get_yrs(data)
```

## Arguments

- data:

  Dataframe. Must contain the column:

  - `year` (integer)

## Value

Integer vector of sorted unique years present in the data.

## Examples

``` r
df <- data.frame(year = c(2020, 2021, 2020, 2022))
get_yrs(df)
#> [1] 2020 2021 2022
```
