# ------------------------------------------------------------------------------
# title: EpiTraxr Report Generator (Piped Mode)
# author: Andrew Pulsipher
# date: 2025-07-10
#
# This script generates monthly and YTD reports from EpiTrax data using the
# epitraxr package functions and the piping method. It reads in EpiTrax data,
# processes it, and generates reports in both internal and public formats.
#
# The script is maintained by the Insight Net center ForeSITE. All the code is
# available on GitHub at
# https://github.com/EpiForeSITE/epitraxr.
#
# For questions, updates, or bug reports, please visit the GitHub repository.
# ------------------------------------------------------------------------------

# Load required libraries
library(epitraxr)

# Set up file system -----------------------------------------------------------
fsys <- list(
  internal = "internal_reports",
  public = "public_reports",
  settings = "report_settings"
)

fsys <- setup_filesystem(folders = fsys, clear.reports = TRUE)

# Generate reports -------------------------------------------------------------
message("Generating reports...")

epitrax <- setup_epitrax(
    config_file = file.path(fsys$settings, "report_config.yaml"),
    disease_list_files = list(
      internal = file.path(fsys$settings, "internal_report_diseases.csv"),
      public = file.path(fsys$settings, "public_report_diseases.csv")
    )) |>
  epitrax_ireport_annual_counts() |>
  epitrax_ireport_monthly_counts_all_yrs() |>
  epitrax_ireport_monthly_avgs(exclude.report.year = TRUE) |>
  epitrax_ireport_ytd_counts_for_month(as.rates = FALSE) |>
  epitrax_ireport_ytd_counts_for_month(as.rates = TRUE) |>
  epitrax_report_grouped_stats() |>
  epitrax_preport_combined_month_ytd() |>
  epitrax_preport_month_crosssections(month_offsets = 0:3) |>
  epitrax_preport_ytd_rates() |>
  epitrax_write_csvs(fsys = fsys) |>
  epitrax_write_xlsxs(fsys = fsys) |>
  epitrax_write_pdf_public_reports(fsys = fsys) |>
  epitrax_write_pdf_grouped_stats(
    params = list(
      title = "Grouped Disease Surveillance Report"
    ),
    fsys = fsys
  )

message(".......Done.")
