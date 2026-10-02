# Create grouped disease statistics report

'create_report_grouped_stats' generates a comprehensive report with
current and historical statistics for diseases organized by group. The
report includes monthly counts/rates, year-to-date counts, and trend
analysis.

## Usage

``` r
create_report_grouped_stats(data, diseases, y, m, config, is.public = FALSE)
```

## Arguments

- data:

  Dataframe. Input data with columns:

  - `disease` (character)

  - `year` (integer)

  - `month` (integer)

  - `counts` (integer)

- diseases:

  Dataframe. Diseases to include in the report. Must have column
  `EpiTrax_name` (character) with diseases to include. Optionally may
  have column `Group_name` (character) to define disease groupings. If
  `Group_name` is missing, all diseases will be grouped under
  "Uncategorized".

- y:

  Integer. Report year

- m:

  Integer. Report month (1-12)

- config:

  List. Report settings

- is.public:

  Logical. If TRUE, uses public-facing disease names.

## Value

Dataframe with one row per disease containing:

- Group: Disease group name

- Disease: Disease name

- Monthly counts and rates for current year/month

- Historical monthly averages and medians

- Year-to-date counts and historical averages and medians

- YTD trend indicators

## Details

Uses the following config options:

- `current_population`

- `avg_5yr_population`

- `rounding_decimals`

- `trend_threshold`

## See also

[`create_report_monthly_counts()`](https://epiforesite.github.io/epitraxr/reference/create_report_monthly_counts.md),
[`create_report_monthly_avgs()`](https://epiforesite.github.io/epitraxr/reference/create_report_monthly_avgs.md),
[`create_report_monthly_medians()`](https://epiforesite.github.io/epitraxr/reference/create_report_monthly_medians.md),
[`create_report_ytd_counts()`](https://epiforesite.github.io/epitraxr/reference/create_report_ytd_counts.md),
[`create_report_ytd_medians()`](https://epiforesite.github.io/epitraxr/reference/create_report_ytd_medians.md)
which this function uses and
[`epitraxr_config()`](https://epiforesite.github.io/epitraxr/reference/epitraxr_config.md)
for config options

## Examples

``` r
data <- data.frame(
  disease = c("A", "A", "B", "B"),
  year = c(2023, 2024, 2023, 2024),
  month = c(1, 1, 2, 2),
  counts = c(10, 20, 15, 25)
)
diseases <- data.frame(
  EpiTrax_name = c("A", "B", "C"),
  Group_name = c("Group1", "Group1", "Group2")
)
config <- list(
  current_population = 100000,
  avg_5yr_population = 100000,
  rounding_decimals = 1,
  trend_threshold = 0.15
)
create_report_grouped_stats(data, diseases, 2024, 2, config)
#>    Group Disease February 2024 February 2024 Rate Historical February Avg
#> 1 Group1       A             0                  0                       0
#> 2 Group1       B            25                 25                      15
#> 3 Group2       C             0                  0                       0
#>   Historical February Median 2024 YTD Historical 2024 YTD Avg
#> 1                          0       20                      10
#> 2                         15       25                      15
#> 3                          0        0                       0
#>   Historical 2024 YTD Median YTD Trend
#> 1                         10  Elevated
#> 2                         15  Elevated
#> 3                          0  Expected
```
