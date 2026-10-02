# Set report configuration of EpiTrax object from list

`epitrax_set_config_from_list` sets the report configuration from the
given list.

## Usage

``` r
epitrax_set_config_from_list(epitrax, config = NULL)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- config:

  Optional list of config parameters. If omitted, default values will be
  used.

## Value

Updated EpiTrax object with `config` field set.

## See also

[`epitrax_set_config_from_file()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_config_from_file.md)
and the convenience function
[`setup_epitrax()`](https://epiforesite.github.io/epitraxr/reference/setup_epitrax.md)
which wraps this function

## Examples

``` r
config <- list(
 current_population = 56000,
 avg_5yr_population = 57000,
 rounding_decimals = 3,
 generate_csvs = FALSE
)
epitrax <- structure(
  list(data = c(1,2,3)),
  class = "epitrax"
)
epitrax <- epitrax_set_config_from_list(epitrax, config)
```
