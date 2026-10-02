# Setup EpiTrax object with configuration and disease lists

`setup_epitrax` initializes an EpiTrax object with configuration and
report disease lists. It is a convenience function that combines
[`create_epitrax_from_file()`](https://epiforesite.github.io/epitraxr/reference/create_epitrax_from_file.md),
[`epitrax_set_config_from_file()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_config_from_file.md),
and
[`epitrax_set_report_diseases()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_report_diseases.md).

## Usage

``` r
setup_epitrax(
  filepath = NULL,
  num_yrs = 5,
  disease_list_files = NULL,
  config_list = NULL,
  config_file = NULL
)
```

## Arguments

- filepath:

  Optional filepath. EpiTrax data file should be a CSV. If this
  parameter is NULL, the user will be prompted to choose a file
  interactively.

- num_yrs:

  Integer. Number of years of data to keep. Defaults to 5.

- disease_list_files:

  Optional list containing filepaths to internal and public report
  disease lists. If omitted, the default lists will be used and a
  warning will be given.

- config_list, config_file:

  Configuration options may be specified as a list or as a path to a
  YAML config file, respectively. Only one can be specified at a time.
  If both are specified, the function will return an error. If both are
  omitted, the default config values will be used.

## Value

An EpiTrax object with configuration and report diseases set.

## See also

[`create_epitrax_from_file()`](https://epiforesite.github.io/epitraxr/reference/create_epitrax_from_file.md),
[`epitrax_set_config_from_file()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_config_from_file.md),
[`epitrax_set_config_from_list()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_config_from_list.md),
and
[`epitrax_set_report_diseases()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_report_diseases.md)
which this function wraps.

## Examples

``` r
data_file <- system.file("sample_data/sample_epitrax_data.csv",
                         package = "epitraxr")
disease_lists <- list(
  internal = system.file("tinytest/test_files/disease_lists/internal_list.csv",
                         package = "epitraxr"),
  public = system.file("tinytest/test_files/disease_lists/public_list.csv",
                       package = "epitraxr")
)

epitrax <- setup_epitrax(
  filepath = data_file,
  disease_list_files = disease_lists
)
```
