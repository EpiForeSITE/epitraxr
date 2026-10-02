# Create combined monthly and year-to-date public report

'create_public_report_combined_month_ytd' creates a comprehensive public
report that combines monthly case data with year-to-date statistics for
the given month and year. This provides both current month context and
cumulative year progress.

## Usage

``` r
create_public_report_combined_month_ytd(data, diseases, y, m, config)
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

List containing the report name and combined monthly/YTD report data
with columns for monthly cases/averages/trends and YTD statistics.

## Details

Uses the following config options:

- `current_population`

- `avg_5yr_population`

- `rounding_decimals`

- `trend_threshold`

## See also

[`create_public_report_month()`](https://epiforesite.github.io/epitraxr/reference/create_public_report_month.md),
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
create_public_report_combined_month_ytd(
 data = data,
 diseases = diseases,
 y = 2024,
 m = 2,
 config = config
)
#> $name
#> [1] "public_report_combined_Feb2024"
#> 
#> $report
#>     Disease Feb Cases Feb Average Cases              Trend YTD Cases
#> 1  COVID-19        89             145.4 Less Than Expected      1191
#> 2 Influenza       115             149.0 Less Than Expected      1466
#> 3   Measles        16              33.6 Less Than Expected       304
#> 4  Syphilis        18              38.0 Less Than Expected       269
#>   YTD Average Cases YTD Rate per 100k YTD Average Rate per 100k
#> 1             239.2          2126.786                   419.649
#> 2             260.4          2617.857                   456.842
#> 3              55.6           542.857                    97.544
#> 4              64.2           480.357                   112.632
#> 
```
