# Changelog

## epitraxr (development version)

- Added the ForeSITE pkgdown brand and weekly brand-sync workflow.

## epitraxr 0.5.0

CRAN release: 2025-09-14

- Add formatted PDF reports
- Add option to produce trend-only PDF reports
- Update installation instructions in README
- Increase file upload size limit in Shiny app
- Significant overall of function APIs for consistency, including
  renaming several functions:
  - `get_trend()` -\>
    [`compute_trend()`](https://epiforesite.github.io/epitraxr/reference/compute_trend.md)
  - `get_epitrax()` -\>
    [`create_epitrax_from_file()`](https://epiforesite.github.io/epitraxr/reference/create_epitrax_from_file.md)
  - `epitrax_add_report_diseases()` -\>
    [`epitrax_set_report_diseases()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_report_diseases.md)
  - `read_report_config()` -\>
    [`get_report_config()`](https://epiforesite.github.io/epitraxr/reference/get_report_config.md)
  - `get_report_disease_lists()` -\>
    [`get_report_diseases()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases.md)
  - `get_internal_disease_list()` -\>
    [`get_report_diseases_internal()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases_internal.md)
  - `get_public_disease_list()` -\>
    [`get_report_diseases_public()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases_public.md)
  - `format_week_num()` -\>
    [`mmwr_week_to_month()`](https://epiforesite.github.io/epitraxr/reference/mmwr_week_to_month.md)
    and
    [`format_epitrax_data()`](https://epiforesite.github.io/epitraxr/reference/format_epitrax_data.md)
    (separated different functionality into two functions)
  - `write_grouped_report_pdf()` -\>
    [`write_report_pdf_grouped()`](https://epiforesite.github.io/epitraxr/reference/write_report_pdf_grouped.md)

## epitraxr 0.4.0

### Major Changes

- Add Shiny app to package

### Minor Changes

- Allow user to choose how many years of data to include in reports
  (defaults is 5 + the latest year)
- Rename `prep_report_data()` to
  [`standardize_report_diseases()`](https://epiforesite.github.io/epitraxr/reference/standardize_report_diseases.md)
- Add
  [`vignette("piped-mode")`](https://epiforesite.github.io/epitraxr/articles/piped-mode.md)
- Update
  [`vignette("epitraxr")`](https://epiforesite.github.io/epitraxr/articles/epitraxr.md)
  with clearer language and examples

### Bug Fixes

- Fix bug in
  [`epitrax_write_xlsxs()`](https://epiforesite.github.io/epitraxr/reference/epitrax_write_xlsxs.md)
  where it would write Excel sheets even if report lists were empty

## epitraxr 0.3.2

- Create grouped reports
- Enable PDF output for some reports
- Add threshold parameter to `get_trend()`
- Add new report combining monthly and YTD stats
- Complete R package checklist

## epitraxr 0.2.2

- Minor fixes to verify piped script matches original script behavior

## epitraxr 0.2.1

- Add method for setting config programmatically

## epitraxr 0.2.0

- Fully support pipe operator add report generation functions for
  EpiTrax object

## epitraxr 0.1.3

- Add pkgdown website
- Create new EpiTrax data object

## epitraxr 0.1.0

- Initial release of epitraxr package
- Core modules: Data Processing, Report Generation, Filesystem
  Management, Helper Functions
- Sample data included for testing and demonstration purposes
- Comprehensive testing with tinytest framework
- Full roxygen documentation for all functions
