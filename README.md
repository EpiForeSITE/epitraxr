
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epitraxr

<!-- badges: start -->

[![ForeSITE
Group](https://github.com/EpiForeSITE/software/raw/e82ed88f75e0fe5c0a1a3b38c2b94509f122019c/docs/assets/foresite-software-badge.svg)](https://github.com/EpiForeSITE)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/EpiForeSITE/epitraxr/blob/master/LICENSE.md)
[![R-CMD-check](https://github.com/EpiForeSITE/epitraxr/actions/workflows/r-check.yaml/badge.svg)](https://github.com/EpiForeSITE/epitraxr/actions/workflows/r-check.yaml)
[![pkgdown](https://github.com/EpiForeSITE/epitraxr/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/EpiForeSITE/epitraxr/actions/workflows/pkgdown.yaml)
[![codecov](https://codecov.io/gh/EpiForeSITE/epitraxr/graph/badge.svg?token=KC48SFPX39)](https://codecov.io/gh/EpiForeSITE/epitraxr)
<!-- badges: end -->

## Overview

EpiTrax is a central repository for epidemiological data developed by
Utah State’s Department of Health and Human Services (DHHS). It is now
used by several other states. Through EpiTrax, public health officials
have access to many different types of disease surveillance data, which
they use to produce regular (e.g., weekly, monthly, annual) reports on
their respective jurisdictions. This can be a tedious, time-intensive
process, often involving multiple spreadsheets.

The `epitraxr` package makes it fast and easy to process EpiTrax data
and generate reports. With `epitraxr` it is simple to setup a report
pipeline, then you simply hit “run” and select your latest EpiTrax
export—`epitraxr` will do the rest!

The package is targeted toward public health officials. Work to create
this package was made possible by cooperative agreement
CDC-RFA-FT-23-0069 from the CDC’s Center for Forecasting and Outbreak
Analytics.

## Installation

You can install the development version of `epitraxr` from
[GitHub](https://github.com/EpiForeSITE/epitraxr) with:

``` r
# install.packages("devtools")
devtools::install_github("EpiForeSITE/epitraxr")
```

## Usage

`epitraxr` can be used in either “standard” mode or “piped” mode
(recommended).

### Standard Mode

``` r
library(epitraxr)

data_file <- system.file(
  "sample_data/sample_epitrax_data.csv",
  package = "epitraxr"
)
epitrax_data <- read_epitrax_data(data_file)

report <- create_report_annual_counts(
  data = epitrax_data,
  disease_names = c("Chickenpox", "Measles", "Lyme disease")
)

report
#>        disease 2019 2020 2021 2022 2023 2024
#> 1   Chickenpox  218  318  263  234  249  292
#> 2 Lyme disease    0    0    0    0    0    0
#> 3      Measles  211  326  292  414  586  304
```

### Piped Mode (recommended)

``` r
library(epitraxr)

config_file <- system.file(
  "sample_data/sample_config.yml",
  package = "epitraxr"
)

disease_list_file <- system.file(
  "sample_data/sample_disease_list.csv",
  package = "epitraxr"
)

epitrax <- get_epitrax(data_file) |>
  epitrax_set_config_from_file(config_file) |>
  epitrax_add_report_diseases(list(
    internal = disease_list_file,
    public = disease_list_file
  )) |>
  epitrax_ireport_annual_counts()

epitrax$internal_reports$annual_counts
#>        disease 2019 2020 2021 2022 2023 2024
#> 1   Chickenpox  218  318  263  234  249  292
#> 2 Lyme disease    0    0    0    0    0    0
#> 3      Measles  211  326  292  414  586  304
```

Piped mode makes it super simple to add additional reports.

``` r
epitrax <- get_epitrax(data_file) |>
  epitrax_set_config_from_file(config_file) |>
  epitrax_add_report_diseases(list(
    internal = disease_list_file,
    public = disease_list_file
  )) |>
  epitrax_ireport_annual_counts() |>
  epitrax_ireport_monthly_avgs() |>
  epitrax_ireport_ytd_counts_for_month()

list(epitrax$internal_reports)
#> [[1]]
#> [[1]]$annual_counts
#>        disease 2019 2020 2021 2022 2023 2024
#> 1   Chickenpox  218  318  263  234  249  292
#> 2 Lyme disease    0    0    0    0    0    0
#> 3      Measles  211  326  292  414  586  304
#>
#> [[1]]$`monthly_avgs_2019-2024`
#>        disease  Jan    Feb    Mar    Apr    May    Jun    Jul  Aug    Sep
#> 1   Chickenpox 23.5 22.333 21.000 26.667 19.833 19.167 24.167 20.0 18.833
#> 2 Lyme disease  0.0  0.000  0.000  0.000  0.000  0.000  0.000  0.0  0.000
#> 3      Measles 27.0 31.500 24.667 34.833 25.667 24.667 37.333 27.5 28.167
#>      Oct    Nov    Dec
#> 1 25.333 19.000 22.500
#> 2  0.000  0.000  0.000
#> 3 36.333 29.167 28.667
#>
#> [[1]]$ytd_counts
#>        disease Current_YTD_Counts Avg_5yr_YTD_Counts
#> 1   Chickenpox                292              256.4
#> 2 Lyme disease                  0                0.0
#> 3      Measles                304              365.8
```

## Getting Help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/EpiForeSITE/epitraxr/issues).

## Code of Conduct

Please note that the epitraxr project is released with a [Contributor
Code of
Conduct](https://epiforesite.github.io/epitraxr/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
