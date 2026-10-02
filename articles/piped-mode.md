# Piped Mode

## Overview

While the package functions can all be called individually (standard
mode, described briefly in
[`vignette("epitraxr")`](https://epiforesite.github.io/epitraxr/articles/epitraxr.md)),
we **recommend using the piped mode** of the epitraxr package because it
results in much cleaner, more maintainable code. Instead of calling and
saving the results of each generated report, you use the pipe operator
`|>` to chain together multiple reports and all the reports are saved to
a single `epitrax` object. You can then either manipulate all the
reports from the `epitrax` object, or add to your pipe one of epitraxr’s
export functions to write reports to one of the supported formats (e.g.,
CSV).

In the epitraxr package, functions that expect a “piped” input are
identified by the prefix `epitrax_`. Within this family, report
generators are typically prefixed with either `epitrax_preport_` or
`epitrax_ireport_` (corresponding to public and internal reports
respectively), but can also have the prefix `epitrax_report` (can do
both public and internal reports). Export functions are prefixed with
`epitrax_write_` (e.g.,
[`epitrax_write_csvs()`](https://epiforesite.github.io/epitraxr/reference/epitrax_write_csvs.md)).

This vignette will walk you through each step of an epitraxr pipe then
show the completed pipe running from end to end.

### Pipe Setup

#### Create an `epitrax` object

The first step in piped mode is always to create an `epitrax` object
with the
[`create_epitrax_from_file()`](https://epiforesite.github.io/epitraxr/reference/create_epitrax_from_file.md)
function. This object will contain the data, configuration options, and
report settings needed for all reports in the pipe. The `epitrax` object
is passed through each function in the pipe. When a report is generated,
it is appended to the appropriate list (public or internal) in the
`epitrax` object before the object is passed to the next function in the
pipe.

``` r

data_fp <- "vignette-data/epitrax_data.csv"

epitrax <- create_epitrax_from_file(filepath = data_fp)

names(epitrax)
#> [1] "data"             "diseases"         "yrs"              "report_year"     
#> [5] "report_month"     "internal_reports" "public_reports"
```

The
[`create_epitrax_from_file()`](https://epiforesite.github.io/epitraxr/reference/create_epitrax_from_file.md)
function reads the data in the provided data file, validates, and
formats it. It then adds the data to the `epitrax` object as
`epitrax$data`. The function also extracts key information and summary
statistics from the data and adds those to the epitrax object as well: -
`epitrax$diseases`: All diseases found in the data - `epitrax$yrs`:
Years included in the data - `epitrax$report_year` and
`epitrax$report_month`: The year and month treated as the “current” date
for reports. Default to the latest year/month in the data. -
`epitrax$internal_reports` and `epitrax$public_reports`: Lists to hold
generated reports. Initially empty.

**Note:** All further functions in the pipe will expect an object of
class `epitrax` as their first argument. Thus,
[`create_epitrax_from_file()`](https://epiforesite.github.io/epitraxr/reference/create_epitrax_from_file.md)
is the start of the pipe.

#### Add Disease Lists

The next step is adding two disease lists, one for internal reports and
one for public reports. If a given disease is not in the EpiTrax data,
that means there were no reported cases of that disease in those years.
That is still useful data that you may want to include in your reports.
The epitraxr package uses two lists because public reports typically
include a subset of diseases, while internal reports typically include
all tracked diseases. Add the disease lists to the `epitrax` object
using the
[`epitrax_set_report_diseases()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_report_diseases.md)
function.

``` r

disease_lists = list(
  internal = "vignette-data/ireport_diseases.csv",
  public = "vignette-data/preport_diseases.csv"
)

epitrax <- epitrax_set_report_diseases(epitrax, disease_list_files = disease_lists)

names(epitrax)
#> [1] "data"             "diseases"         "yrs"              "report_year"     
#> [5] "report_month"     "internal_reports" "public_reports"   "report_diseases"
```

The `epitrax` object now contains `report_diseases` with
`report_diseases$internal` and `report_diseases$public` holding the
individual lists.

#### Add Config

The last step is adding a configuration options. These can be read from
a list
([`epitrax_set_config_from_list()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_config_from_list.md))
or from a file
([`epitrax_set_config_from_file()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_config_from_file.md)).
Configuration options provide report generators with important values,
such as your area’s current and previous population (used for converting
counts to rates per 100k) and the trend threshold (used to determine if
current counts are above or below historical counts).

``` r

config_file <- "vignette-data/config.yaml"
epitrax <- epitrax_set_config_from_file(epitrax, filepath = config_file)

names(epitrax)
#> [1] "data"             "diseases"         "yrs"              "report_year"     
#> [5] "report_month"     "internal_reports" "public_reports"   "report_diseases" 
#> [9] "config"
```

The `epitrax` object now contains the `config` details:

``` r

epitrax$config
#> $current_population
#> [1] 67000
#> 
#> $avg_5yr_population
#> [1] 65000
#> 
#> $rounding_decimals
#> [1] 2
#> 
#> $generate_csvs
#> [1] TRUE
#> 
#> $trend_threshold
#> [1] 0.15
```

#### Convenient Setup

Since these three operations must always occur before the report
generators can be run, epitraxr has the convenience function
[`setup_epitrax()`](https://epiforesite.github.io/epitraxr/reference/setup_epitrax.md).

``` r

epitrax <- setup_epitrax(
  filepath = data_fp,
  config_file = config_file,
  disease_list_files = disease_lists
)

names(epitrax)
#> [1] "data"             "diseases"         "yrs"              "report_year"     
#> [5] "report_month"     "internal_reports" "public_reports"   "report_diseases" 
#> [9] "config"
```

### Running Report Generators

At this point, the `epitrax` object is ready to be piped into report
generators. To start, run
[`epitrax_ireport_annual_counts()`](https://epiforesite.github.io/epitraxr/reference/epitrax_ireport_annual_counts.md)
and
[`epitrax_ireport_monthly_counts_all_yrs()`](https://epiforesite.github.io/epitraxr/reference/epitrax_ireport_monthly_counts_all_yrs.md),
then inspect the list of reports:

``` r

epitrax <- epitrax_ireport_annual_counts(epitrax)
epitrax <- epitrax_ireport_monthly_counts_all_yrs(epitrax)

names(epitrax$internal_reports)
#> [1] "annual_counts"       "monthly_counts_2019" "monthly_counts_2020"
#> [4] "monthly_counts_2021" "monthly_counts_2022" "monthly_counts_2023"
#> [7] "monthly_counts_2024"
```

Call a few more report generators:

``` r

epitrax <- epitrax_ireport_monthly_avgs(epitrax)
epitrax <- epitrax_ireport_ytd_counts_for_month(epitrax)
epitrax <- epitrax_preport_month_crosssections(epitrax)
epitrax <- epitrax_preport_ytd_rates(epitrax)
```

The object now contains these internal reports:

``` r

names(epitrax$internal_reports)
#> [1] "annual_counts"          "monthly_counts_2019"    "monthly_counts_2020"   
#> [4] "monthly_counts_2021"    "monthly_counts_2022"    "monthly_counts_2023"   
#> [7] "monthly_counts_2024"    "monthly_avgs_2019-2024" "ytd_counts"
```

And these public reports:

``` r

names(epitrax$public_reports)
#> [1] "public_report_Dec2024" "public_report_Nov2024" "public_report_Oct2024"
#> [4] "public_report_Sep2024" "public_report_YTD"
```

As you can see, each report generator simply appends the created reports
to the appropriate list.

### Exporting Reports

While you may want to process the reports contained in the `epitrax`
object in R, you will often export the generated reports to one of the
formats supported by epitraxr.

#### Setup Filesystem

To use export functions in epitraxr, you need to provide folder paths
for internal and public reports. These are organized as a list. The
[`setup_filesystem()`](https://epiforesite.github.io/epitraxr/reference/setup_filesystem.md)
function creates the folders (if they don’t already exist) and
optionally clears out any old reports from previous runs:

``` r

tmpdir <- tempdir()
fsys <- list(
  internal = file.path(tmpdir, "internal_reports"),
  public = file.path(tmpdir, "public_reports")
)

fsys <- setup_filesystem(folders = fsys, clear.reports = TRUE)
```

You can skip the
[`setup_filesystem()`](https://epiforesite.github.io/epitraxr/reference/setup_filesystem.md)
function, if you know your folders are created and ready to receive
reports.

You will pass this `fsys` list to epitraxr export functions.

#### Export to CSV

The most common export format is CSV using the
[`epitrax_write_csvs()`](https://epiforesite.github.io/epitraxr/reference/epitrax_write_csvs.md)
function.

``` r

epitrax <- epitrax_write_csvs(epitrax, fsys = fsys)

list.files(fsys$internal)
#> [1] "annual_counts.csv"          "monthly_avgs_2019-2024.csv"
#> [3] "monthly_counts_2019.csv"    "monthly_counts_2020.csv"   
#> [5] "monthly_counts_2021.csv"    "monthly_counts_2022.csv"   
#> [7] "monthly_counts_2023.csv"    "monthly_counts_2024.csv"   
#> [9] "ytd_counts.csv"
list.files(fsys$public)
#> [1] "public_report_Dec2024.csv" "public_report_Nov2024.csv"
#> [3] "public_report_Oct2024.csv" "public_report_Sep2024.csv"
#> [5] "public_report_YTD.csv"
```

Typically, export functions are called at the end of the pipe. However,
since export functions do not modify the `epitrax` object, you can
safely insert these functions anywhere in the pipe.

### Full Pipe: Putting It All Together

Here is the full pipe described above:

``` r

# Data and config files
data_fp <- "vignette-data/epitrax_data.csv"
disease_lists = list(
  internal = "vignette-data/ireport_diseases.csv",
  public = "vignette-data/preport_diseases.csv"
)
config_file <- "vignette-data/config.yaml"

# Setup filesystem
tmpdir <- tempdir()
fsys <- list(
  internal = file.path(tmpdir, "internal_reports"),
  public = file.path(tmpdir, "public_reports")
)

fsys <- setup_filesystem(folders = fsys, clear.reports = TRUE)

# Run report generation pipe
epitrax <- setup_epitrax(
    filepath = data_fp,
    config_file = config_file,
    disease_list_files = disease_lists
  ) |>
  epitrax_ireport_annual_counts() |>
  epitrax_ireport_monthly_counts_all_yrs() |>
  epitrax_ireport_monthly_avgs() |>
  epitrax_ireport_ytd_counts_for_month() |>
  epitrax_preport_month_crosssections() |>
  epitrax_preport_ytd_rates() |>
  epitrax_write_csvs(fsys = fsys)

length(epitrax$internal_reports)
#> [1] 9
list.files(fsys$internal)
#> [1] "annual_counts.csv"          "monthly_avgs_2019-2024.csv"
#> [3] "monthly_counts_2019.csv"    "monthly_counts_2020.csv"   
#> [5] "monthly_counts_2021.csv"    "monthly_counts_2022.csv"   
#> [7] "monthly_counts_2023.csv"    "monthly_counts_2024.csv"   
#> [9] "ytd_counts.csv"

length(epitrax$public_reports)
#> [1] 5
list.files(fsys$public)
#> [1] "public_report_Dec2024.csv" "public_report_Nov2024.csv"
#> [3] "public_report_Oct2024.csv" "public_report_Sep2024.csv"
#> [5] "public_report_YTD.csv"
```
