# Create epitraxr config object

`epitraxr_config` creates a list of configuration options used for
generating reports.

## Usage

``` r
epitraxr_config(
  current_population = 1e+05,
  avg_5yr_population = 1e+05,
  rounding_decimals = 2,
  generate_csvs = TRUE,
  trend_threshold = 0.15
)
```

## Arguments

- current_population:

  Integer. Defaults to 100,000.

- avg_5yr_population:

  Integer. Defaults to 100,000.

- rounding_decimals:

  Integer. Defaults to 2.

- generate_csvs:

  Logical. Defaults to TRUE.

- trend_threshold:

  Numeric. Defaults to 0.15.

## Value

A named list with 'keys' corresponding to config options.

## Examples

``` r
epitraxr_config(
  current_population = 56000,
  avg_5yr_population = 57000,
  rounding_decimals = 3,
  generate_csvs = FALSE,
  trend_threshold = 0.2
)
#> $current_population
#> [1] 56000
#> 
#> $avg_5yr_population
#> [1] 57000
#> 
#> $rounding_decimals
#> [1] 3
#> 
#> $generate_csvs
#> [1] FALSE
#> 
#> $trend_threshold
#> [1] 0.2
#> 
```
