# Validate config

`validate_config` checks the values of the given config list. If any
values are missing or invalid, they are set to default values and a
warning is issued.

## Usage

``` r
validate_config(config)
```

## Arguments

- config:

  Named list.

## Value

A named list with 'keys' corresponding to config options.

## Examples

``` r
validate_config(config = list())
#> Warning: These config fields are missing/invalid and will be set to defaults:
#> 
#>  - 'current_population' set to 100,000
#>  - 'avg_5yr_population' set to 'current_population'
#>  - 'rounding_decimals' set to 2
#>  - 'generate_csvs' set to TRUE
#>  - 'trend_threshold' set to 0.15
#> $current_population
#> [1] 1e+05
#> 
#> $avg_5yr_population
#> [1] 1e+05
#> 
#> $rounding_decimals
#> [1] 2
#> 
#> $generate_csvs
#> [1] TRUE
#> 
#> $trend_threshold
#> [1] 0.15
#> 
```
