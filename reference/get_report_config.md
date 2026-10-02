# Read in the report config YAML file

'get_report_config' reads in the config YAML file. Missing fields will
be set to default values and a warning will be issued. The config file
can have the following fields:

- `current_population`: Integer. Current population size.

- `avg_5yr_population`: Integer. Average population over the last 5
  years.

- `rounding_decimals`: Integer. Number of decimals to round report
  values to.

- `generate_csvs`: Logical. Whether to generate CSV files.

- `trend_threshold`: Numeric. Threshold for trend calculations.

## Usage

``` r
get_report_config(filepath)
```

## Arguments

- filepath:

  Filepath. Path to report config file.

## Value

A named list with an attribute of 'keys' from the file.

## Details

See the example config file here:
`system.file("sample_data/sample_config.yml", package = "epitraxr")`.

## Examples

``` r
config_file <- system.file("sample_data/sample_config.yml",
                          package = "epitraxr")
report_config <- get_report_config(config_file)
```
