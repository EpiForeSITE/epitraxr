
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epitraxr

<!-- badges: start -->

[![ForeSITE
Group](https://github.com/EpiForeSITE/software/raw/e82ed88f75e0fe5c0a1a3b38c2b94509f122019c/docs/assets/foresite-software-badge.svg)](https://github.com/EpiForeSITE)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/EpiForeSITE/epitraxr/actions/workflows/r-check.yaml/badge.svg)](https://github.com/EpiForeSITE/epitraxr/actions/workflows/r-check.yaml)
[![pkgdown](https://github.com/EpiForeSITE/epitraxr/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/EpiForeSITE/epitraxr/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

The goal of `epitraxr` is to simplify the process of manipulating
Epitrax data and generating reports. The package is targeted toward
public health officials.

Work to create this software tool was made possible by cooperative
agreement CDC-RFA-FT-23-0069 from the CDC’s Center for Forecasting and
Outbreak Analytics.

## Installation

You can install the development version of epitraxr from
[GitHub](https://github.com/EpiForeSITE/epitraxr) with:

``` r
# install.packages("devtools")
devtools::install_github("EpiForeSITE/epitraxr")
```

## Examples

``` r
# Read the sample EpiTrax dataset included with the package
sample_file <- system.file("sample_data", "sample_epitrax_data.csv", package = "epitraxr")
epitrax_data <- read_epitrax_data(sample_file)

# Get the list of diseases in the dataset
diseases <- unique(epitrax_data$disease)

# Set up configuration
config <- list(
  current_population = 102000,
  avg_5yr_population = 97000,
  rounding_decimals = 1
)

# Generate monthly counts report for 2024
monthly_counts <- create_report_monthly_counts(
  data = epitrax_data,
  y = 2024,
  disease_names = diseases
)
monthly_counts
#>      disease Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 1 Chickenpox  26  19  22  40  16  23  37  16  21  27  14  31
#> 2   COVID-19  81  99  84 120  89 101 125  93  80 117  99 103
#> 3  Influenza 116  96 130 151 115 100 139 135 113 138  96 137
#> 4    Measles  22  21  26  27  16  19  35  25  18  32  25  38
#> 5   Syphilis  22  22  23  24  18  29  15  20  13  23  21  39

# Generate YTD counts with rates per 100k (through May)
ytd_report <- create_report_ytd_counts(
  data = epitrax_data,
  disease_names = diseases,
  y = 2024,
  m = 5,
  config = config,
  as.rates = TRUE
)
ytd_report
#>      disease Current_YTD_Rate_per_100k Avg_5yr_YTD_Rate_per_100k
#> 1 Chickenpox                     286.3                     107.6
#> 2   COVID-19                    1167.6                     630.9
#> 3  Influenza                    1437.3                     693.8
#> 4    Measles                     298.0                     151.8
#> 5   Syphilis                     263.7                     166.8
```
