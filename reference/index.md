# Package index

## Piped mode functions

### Setup

Functions to create the epitrax object and prepare it for report
generation

- [`setup_epitrax()`](https://epiforesite.github.io/epitraxr/reference/setup_epitrax.md)
  : Setup EpiTrax object with configuration and disease lists
- [`create_epitrax_from_file()`](https://epiforesite.github.io/epitraxr/reference/create_epitrax_from_file.md)
  : Create an EpiTrax object from data file
- [`epitrax_set_config_from_file()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_config_from_file.md)
  : Set report configuration of EpiTrax object from config file
- [`epitrax_set_config_from_list()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_config_from_list.md)
  : Set report configuration of EpiTrax object from list
- [`epitrax_set_report_diseases()`](https://epiforesite.github.io/epitraxr/reference/epitrax_set_report_diseases.md)
  : Set report diseases in EpiTrax object

### Internal reports

Functions that generate exclusively internal reports

- [`epitrax_ireport_annual_counts()`](https://epiforesite.github.io/epitraxr/reference/epitrax_ireport_annual_counts.md)
  : Create annual counts internal report from an EpiTrax object
- [`epitrax_ireport_monthly_avgs()`](https://epiforesite.github.io/epitraxr/reference/epitrax_ireport_monthly_avgs.md)
  : Create monthly averages internal report from an EpiTrax object
- [`epitrax_ireport_monthly_counts_all_yrs()`](https://epiforesite.github.io/epitraxr/reference/epitrax_ireport_monthly_counts_all_yrs.md)
  : Create monthly counts internal report for all years from an EpiTrax
  object
- [`epitrax_ireport_ytd_counts_for_month()`](https://epiforesite.github.io/epitraxr/reference/epitrax_ireport_ytd_counts_for_month.md)
  : Create year-to-date (YTD) counts internal report for a given month
  from an EpiTrax object

### Public reports

Functions that generate exclusively public reports

- [`epitrax_preport_combined_month_ytd()`](https://epiforesite.github.io/epitraxr/reference/epitrax_preport_combined_month_ytd.md)
  : Create combined monthly/YTD stats public report from an EpiTrax
  object
- [`epitrax_preport_month_crosssections()`](https://epiforesite.github.io/epitraxr/reference/epitrax_preport_month_crosssections.md)
  : Create monthly cross-section reports from an EpiTrax object
- [`epitrax_preport_ytd_rates()`](https://epiforesite.github.io/epitraxr/reference/epitrax_preport_ytd_rates.md)
  : Create year-to-date (YTD) rates public report from an EpiTrax object

### Other reports

Functions that generate both internal and public reports

- [`epitrax_report_grouped_stats()`](https://epiforesite.github.io/epitraxr/reference/epitrax_report_grouped_stats.md)
  : Create grouped disease statistics report from an EpiTrax object
- [`epitrax_report_monthly_medians()`](https://epiforesite.github.io/epitraxr/reference/epitrax_report_monthly_medians.md)
  : Create monthly medians report from an EpiTrax object
- [`epitrax_report_ytd_medians()`](https://epiforesite.github.io/epitraxr/reference/epitrax_report_ytd_medians.md)
  : Create year-to-date (YTD) medians report from an EpiTrax object

### Export

Functions to export reports

- [`epitrax_write_csvs()`](https://epiforesite.github.io/epitraxr/reference/epitrax_write_csvs.md)
  : Write reports from EpiTrax object to CSV files
- [`epitrax_write_pdf_grouped_stats()`](https://epiforesite.github.io/epitraxr/reference/epitrax_write_pdf_grouped_stats.md)
  : Write grouped statistics reports from EpiTrax object to PDF files
- [`epitrax_write_pdf_public_reports()`](https://epiforesite.github.io/epitraxr/reference/epitrax_write_pdf_public_reports.md)
  : Create formatted PDF report of monthly cross-section reports
- [`epitrax_write_xlsxs()`](https://epiforesite.github.io/epitraxr/reference/epitrax_write_xlsxs.md)
  : Write reports from EpiTrax object to Excel files

## Standard mode functions

### Internal reports

Functions that generate reports intended for internal use

- [`create_report_annual_counts()`](https://epiforesite.github.io/epitraxr/reference/create_report_annual_counts.md)
  : Create annual counts report
- [`create_report_grouped_stats()`](https://epiforesite.github.io/epitraxr/reference/create_report_grouped_stats.md)
  : Create grouped disease statistics report
- [`create_report_monthly_avgs()`](https://epiforesite.github.io/epitraxr/reference/create_report_monthly_avgs.md)
  : Create monthly averages report
- [`create_report_monthly_counts()`](https://epiforesite.github.io/epitraxr/reference/create_report_monthly_counts.md)
  : Create monthly counts report
- [`create_report_monthly_medians()`](https://epiforesite.github.io/epitraxr/reference/create_report_monthly_medians.md)
  : Create monthly medians report
- [`create_report_ytd_counts()`](https://epiforesite.github.io/epitraxr/reference/create_report_ytd_counts.md)
  : Create year-to-date (YTD) counts report
- [`create_report_ytd_medians()`](https://epiforesite.github.io/epitraxr/reference/create_report_ytd_medians.md)
  : Create year-to-date (YTD) medians report

### Public reports

Functions that generate reports intended for public use

- [`create_public_report_combined_month_ytd()`](https://epiforesite.github.io/epitraxr/reference/create_public_report_combined_month_ytd.md)
  : Create combined monthly and year-to-date public report
- [`create_public_report_month()`](https://epiforesite.github.io/epitraxr/reference/create_public_report_month.md)
  : Create a monthly cross-section public report
- [`create_public_report_ytd()`](https://epiforesite.github.io/epitraxr/reference/create_public_report_ytd.md)
  : Create a YTD public report

## Data processing

Functions for importing and processing data

- [`read_epitrax_data()`](https://epiforesite.github.io/epitraxr/reference/read_epitrax_data.md)
  : Read in EpiTrax data
- [`mmwr_week_to_month()`](https://epiforesite.github.io/epitraxr/reference/mmwr_week_to_month.md)
  : Convert MMWR week to calendar month
- [`format_epitrax_data()`](https://epiforesite.github.io/epitraxr/reference/format_epitrax_data.md)
  : Format EpiTrax data for report generation
- [`reshape_monthly_wide()`](https://epiforesite.github.io/epitraxr/reference/reshape_monthly_wide.md)
  : Reshape data with each month as a separate column
- [`reshape_annual_wide()`](https://epiforesite.github.io/epitraxr/reference/reshape_annual_wide.md)
  : Reshape data with each year as a separate column
- [`standardize_report_diseases()`](https://epiforesite.github.io/epitraxr/reference/standardize_report_diseases.md)
  : Standardize diseases for report
- [`get_yrs()`](https://epiforesite.github.io/epitraxr/reference/get_yrs.md)
  : Get unique years from the data
- [`get_month_counts()`](https://epiforesite.github.io/epitraxr/reference/get_month_counts.md)
  : Get monthly counts for each disease

## Filesystem

### Setup

Functions for setting up the filesystem

- [`create_filesystem()`](https://epiforesite.github.io/epitraxr/reference/create_filesystem.md)
  : Create filesystem
- [`clear_old_reports()`](https://epiforesite.github.io/epitraxr/reference/clear_old_reports.md)
  : Clear out old reports before generating new ones.
- [`setup_filesystem()`](https://epiforesite.github.io/epitraxr/reference/setup_filesystem.md)
  : Setup the report filesystem

### Reading files

Functions for reading configuration and data files

- [`get_report_config()`](https://epiforesite.github.io/epitraxr/reference/get_report_config.md)
  : Read in the report config YAML file
- [`get_report_diseases()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases.md)
  : Get both internal and public disease lists
- [`get_report_diseases_internal()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases_internal.md)
  : Get the internal disease list
- [`get_report_diseases_public()`](https://epiforesite.github.io/epitraxr/reference/get_report_diseases_public.md)
  : Get the public disease list

### Export functions

Functions to export reports

- [`write_report_csv()`](https://epiforesite.github.io/epitraxr/reference/write_report_csv.md)
  : Write report CSV files
- [`write_report_pdf()`](https://epiforesite.github.io/epitraxr/reference/write_report_pdf.md)
  : Write general PDF report of disease stats from R Markdown template
- [`write_report_pdf_grouped()`](https://epiforesite.github.io/epitraxr/reference/write_report_pdf_grouped.md)
  : Write PDF grouped report from R Markdown template
- [`write_report_xlsx()`](https://epiforesite.github.io/epitraxr/reference/write_report_xlsx.md)
  : Write report Excel files

## Validation

Functions for validating input data and configurations

- [`validate_epitrax()`](https://epiforesite.github.io/epitraxr/reference/validate_epitrax.md)
  : Validate EpiTrax object
- [`validate_filesystem()`](https://epiforesite.github.io/epitraxr/reference/validate_filesystem.md)
  : Validate filesystem structure
- [`validate_config()`](https://epiforesite.github.io/epitraxr/reference/validate_config.md)
  : Validate config
- [`validate_data()`](https://epiforesite.github.io/epitraxr/reference/validate_data.md)
  : Validate input EpiTrax data

## Utilities

Miscellaneous utility functions

- [`epitraxr_config()`](https://epiforesite.github.io/epitraxr/reference/epitraxr_config.md)
  : Create epitraxr config object
- [`convert_counts_to_rate()`](https://epiforesite.github.io/epitraxr/reference/convert_counts_to_rate.md)
  : Convert case counts to rate
- [`compute_trend()`](https://epiforesite.github.io/epitraxr/reference/compute_trend.md)
  : Compute the report trend
- [`set_na_0()`](https://epiforesite.github.io/epitraxr/reference/set_na_0.md)
  : Set NA values to 0

## Shiny app

Functions related to the Shiny application

- [`run_app()`](https://epiforesite.github.io/epitraxr/reference/run_app.md)
  : Launch the epitraxr Shiny Application
