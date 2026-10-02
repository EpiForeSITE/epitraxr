# Create a YTD public report

'create_public_report_ytd' creates a public report for YTD rates.

## Usage

``` r
create_public_report_ytd(data, diseases, y, m, config)
```

## Arguments

- data:

  Dataframe. Input data with columns:

  - `disease` (character)

  - `year` (integer)

  - `month` (integer)

  - `counts` (integer)

- diseases:

  Dataframe. Diseases to include in the report. Maps EpiTrax disease
  names to public-facing versions. Must have columns:

  - `EpiTrax_name` (character)

  - `Public_name` (character)

- y:

  Integer. Report year

- m:

  Integer. Report month (1-12)

- config:

  List. Report settings

## Value

List containing the report name and data.

## Details

Uses the following config options:

- `current_population`

- `avg_5yr_population`

- `rounding_decimals`

- `trend_threshold`

## See also

[`create_report_ytd_counts()`](https://epiforesite.github.io/epitraxr/reference/create_report_ytd_counts.md)
which this function uses and
[`epitraxr_config()`](https://epiforesite.github.io/epitraxr/reference/epitraxr_config.md)
for config options

## Examples

``` r
data_file <- system.file("sample_data/sample_epitrax_data.csv",
                         package = "epitraxr")
# Read in EpiTrax data
data <- read_epitrax_data(data_file)

diseases <- data.frame(
  EpiTrax_name = c("Influenza", "COVID-19", "Measles", "Syphilis"),
  Public_name = c("Influenza", "COVID-19", "Measles", "Syphilis")
)
config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
                          package = "epitraxr")
config <- get_report_config(config_file)
create_public_report_ytd(
 data = data,
 diseases = diseases,
 y = 2024,
 m = 1,
 config = config
)
#> $name
#> [1] "public_report_YTD"
#> 
#> $report
#>     Disease YTD_Rate_per_100k Avg_5yr_Rate    Trend
#> 1  COVID-19          2126.786      211.930 Elevated
#> 2 Influenza          2617.857      215.789 Elevated
#> 3   Measles           542.857       49.123 Elevated
#> 4  Syphilis           480.357       61.053 Elevated
#> 
```
