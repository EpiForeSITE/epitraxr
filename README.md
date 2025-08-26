
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

## Installation

You can install the development version of `epitraxr` from
[GitHub](https://github.com/EpiForeSITE/epitraxr) with:

``` r
# install.packages("devtools")
devtools::install_github("EpiForeSITE/epitraxr")
```

For local health departments, we recommend going through the steps under
[Installation For Local Health
Departments](#installation-for-local-health-departments) (below). Our
experience is that LHD-managed machines need additional tools and
packages installed first. This list will walk you through those steps.

### Installing Package Dependencies

Many features of `epitraxr` will work with the standard installation.
However, there are several optional features, such as PDF report
generation and the R Shiny app, that require you to install additional
dependencies. You can do that with the command:

``` r
# install.packages("devtools")
devtools::install_github("EpiForeSITE/epitraxr", dependencies = TRUE)
```

For PDF reports, you may need to install a LaTeX distribution as well.
We recommend using [TinyTeX](https://yihui.org/tinytex/) as described in
the [R Markdown
Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/install-latex.html).
First, install the `tinytex` package:

``` r
install.packages("tinytex")
```

Then, install TinyTeX (the LaTeX distribution) using the `tinytex`
package:

``` r
tinytex::install_tinytex()
# to uninstall TinyTeX, run
# tinytex::uninstall_tinytex()
```

After that, you can use `epitraxr` to generate PDF reports.

### Installation For Local Health Departments

In our experience, machines managed by local health departments (LHDs)
are less likely to have installed all necessary packages and tools. For
these users, we recommend completing the following steps before
installing the package using the instructions above.

1.  Install the latest version of R: <https://cran.rstudio.com>
2.  Install RStudio: <https://posit.co/download/rstudio-desktop/>
3.  If on Windows, install the version of
    [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
    corresponding to the version of R you installed. This is needed to
    build and install certain types of packages.
4.  Install `devtools` (needed to install the package from GitHub)

``` r
install.packages("devtools")
```

5.  Install the `tinytex` package

``` r
install.packages("tinytex")
```

6.  Install TinyTeX (the LaTeX distribution) using the `tinytex` package

``` r
tinytex::install_tinytex()
```

7.  Install `epitraxr`

- **Note:** If you previously attempted to install `epitraxr` and ran
  into an error (such as one solved by the above steps), you will need
  to install the package again with the option `force = TRUE`. This
  ensures the full package is downloaded and installed properly.

``` r
devtools::install_github("EpiForeSITE/epitraxr", dependencies = TRUE, force = TRUE)
```

- This command will also update the package (e.g., if you want to get
  the latest version from GitHub.).

## Usage

### Inputs

#### Input Data

`epitraxr` expects input data in CSV format (not Excel) that contains
EpiTrax data exported with the following columns:

- `patient_mmwr_week` (integer)
- `patient_mmwr_year` (integer)
- `patient_disease` (character)

For example:

``` csv
"patient_mmwr_week","patient_mmwr_year","patient_disease"
"26","2020","Chlamydia trachomatis infection"
```

Your dataset can include other columns, but `epitraxr` will ignore them.

Also, rows with missing or NA values in the input dataset will be
ignored and a warning will be printed to the Console. The warning is
intended to inform the user that their input had missing/NA values, in
case the user wants to correct that (and re-run the report generation).
It does not mean the program was unable to generate reports from the
rest of the data.

#### Report Settings

To configure your reports, `epitraxr` accepts three **OPTIONAL** files:

1.  **Internal Disease List:** CSV file listing the diseases to include
    in internal reports as they are represented in EpiTrax.
    - This file is named `internal_report_diseases.csv` in our example
      scripts.
    - This file can have up to two columns:
      - `EpiTrax_name`: A list of all diseases to include in the report
        **AS THEY ARE NAMED IN EPITRAX**. This column is **required**.
      - `Group_name`: Containing the disease group (e.g., “Zoonotic
        Disease”) for each disease in the `EpiTrax_name` column. This
        column is **optional** and is only used by the grouped disease
        report functions (`create_report_grouped_stats()` and
        `epitrax_report_grouped_stats()`). If omitted, most `epitraxr`
        functions will operate normally, but the grouped disease report
        functions will set all diseases to the single group
        “Uncategorized”.
    - If the Internal Disease List is not provided, `epitraxr` will
      default to using whatever diseases are found in the input dataset.
2.  **Public Disease List:** CSV file listing the diseases to include in
    public reports as they are represented in EpiTrax.
    - This file is named `public_report_diseases.csv` in our example
      scripts.
    - This file should have two columns:
      - `EpiTrax_name`: A list of all diseases to include in the report
        **AS THEY ARE NAMED IN EPITRAX**.
      - `Public_name`: How each disease should be named in the public
        report. For example, converting “Chlamydia trachomatis
        infection” to “Chlamydia”.
        - **NOTE:** If multiple diseases have the same `Public_name`
          their report entries will be combined. For example, to combine
          “Syphilis, primary” and “Syphilis, secondary” into “Syphilis”,
          simply set the `Public_name` of both diseases to “Syphilis”.
    - If the Public Disease List is not provided, the program will
      default to using whatever diseases are found in the input dataset.
3.  **Report Config:** YAML file providing additional configuration
    information:
    - `current_population`: Population for converting case counts for
      the **current year** to Rates per 100k. Defaults to 100k.
    - `avg_5yr_population`: Population to use for converting case counts
      for the **5yr historical average** to Rates per 100k. Defaults to
      `current_population`.
    - `rounding_decimals`: How many digits to round decimal values.
      Defaults to 2.
    - `generate_csvs`: Whether to generate CSVs. When false, only Excel
      files will be generated. Defaults to `TRUE`.
    - `trend_threshold`: Threshold for determining the “Trend” column in
      reports that compute this statistic. A percent change above or
      below the `trend_threshold` will result in a change in the trend.
      Defaults to `0.15` (15%).
    - The Report Config file is named `report_config.yaml` in our
      example scripts.

Again, these files are optional, but significantly improve the quality
of your reports.

**Note:** Disease lists (both internal and public) might include
diseases for which there are no case data in the input dataset. In this
case, their values will simply be 0s in the generated reports.

#### Filesystem

`epitraxr` can output reports as CSVs, Excel files, or PDFs. To write
reports to outputs, you need to provide the output functions with
folders where `epitraxr` will write the outputs. Generally, we’ll use
three folders:

- `report_settings/`: Holds settings files for report generation.
- `internal_reports/`: Holds reports intended for internal use by the
  health department.
- `public_reports/`: Holds reports intended for public use.

You can create these manually or you can use the `setup_filesystem()`
function to create the directories. The example scripts in the
[`scripts/`](https://github.com/EpiForeSITE/epitraxr/tree/main/scripts)
folder all start by setting up the filesystem.

**WARNING:** Do not place any of your own files in the
`internal_reports` and `public_reports` folders you give `epitraxr` as
typically *EVERYTHING* in these folders is *DELETED* each time the
script runs (depending on how you configure `setup_filesystem()`). This
is intended to remove reports from previous runs so they don’t clutter
your filesystem. If you want to save old reports, move them out of the
`internal_reports`/`public_reports` folders before running your
`epitraxr` script again.

### Executing `epitraxr` Code

`epitraxr` can be used in either “standard” mode or “piped” mode
(recommended).

#### Standard Mode

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

#### Piped Mode (recommended)

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

### Shiny App

The epitraxr package includes a Shiny app, which provides a
user-friendly interface for running report generation operations with
epitraxr.

To run the Shiny app, you’ll need to install the optional dependencies,
such as `shiny` and `DT` (if you didn’t already do this during initial
package installation).

``` r
devtools::install_github("EpiForeSITE/epitraxr", dependencies = TRUE)
```

Next, load the package:

``` r
library(epitraxr)
```

Finally, use `run_app()` to launch the app:

``` r
run_app()
```

### Scripts

We’ve developed a couple of handy scripts that are included in the
[GitHub repo](https://github.com/EpiForeSITE/epitraxr) under the
[`scripts/`](https://github.com/EpiForeSITE/epitraxr/tree/main/scripts)
folder:

- [`epitraxr-piped.R`](https://github.com/EpiForeSITE/epitraxr/blob/main/scripts/epitraxr-piped.R):
  Showcases report generation using epitraxr’s Piped Mode
  **(recommended)**
- [`epitraxr-generator.R`](https://github.com/EpiForeSITE/epitraxr/blob/main/scripts/epitraxr-generator.R):
  Showcases report generation using the epitraxr’s Standard Mode

These scripts provide you with a full report generation process that
should run successfully if you have the package installed with all
additional dependencies (see above). They provide identical output to
the [original
project](https://github.com/EpiForeSITE/epitrax-report-automation)
(currently inactive).

### Troubleshooting: Common Usage Problems

- If `epitraxr` unexpectedly fails to generate reports after previously
  succeeding, check that none of the generated reports are open on your
  machine. Often, an `epitraxr` script will start by deleting existing
  reports from folders before generating new ones. If the files are
  opened, `epitraxr` cannot delete them. Close any open files and try
  running the script again.

## Getting Help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/EpiForeSITE/epitraxr/issues).

## Code of Conduct

Please note that the epitraxr project is released with a [Contributor
Code of
Conduct](https://epiforesite.github.io/epitraxr/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Acknowledgments

This project was made possible by cooperative agreement
CDC-RFA-FT-23-0069 from the CDC’s Center for Forecasting and Outbreak
Analytics. Its contents are solely the responsibility of the authors and
do not necessarily represent the official views of the Centers for
Disease Control and Prevention.
