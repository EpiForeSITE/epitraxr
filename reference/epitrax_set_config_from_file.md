# Set report configuration of EpiTrax object from config file

`epitrax_set_config_from_file` reads a report configuration file and
adds it to the EpiTrax object.

## Usage

``` r
epitrax_set_config_from_file(epitrax, filepath)
```

## Arguments

- epitrax:

  Object of class `epitrax`.

- filepath:

  Path to the report configuration file.

## Value

Updated EpiTrax object with `config` field set.

## See also

[`epitrax_set_config_from_list()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_config_from_list.md)
and the convenience function
[`setup_epitrax()`](https://epiforesite.github.io/epitraxr/reference/setup_epitrax.md)
which wraps this function

## Examples

``` r
config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
                           package = "epitraxr")
epitrax <- structure(
  list(data = c(1,2,3)),
  class = "epitrax"
)
epitrax <- epitrax_set_config_from_file(epitrax, config_file)
```
