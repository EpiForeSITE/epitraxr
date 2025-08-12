# ------------------------------------------------------------------------------
# title: Test Script for Epitrax Reports with Pretty Formatting
# author: Andrew Pulsipher
# date: 2025-07-10
#
# This script is for testing the pretty formatting and PDF exports of EpiTrax
# reports.
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
  internal = "scripts/test_format_files/internal_reports",
  public = "scripts/test_format_files/public_reports",
  settings = "scripts/test_format_files/report_settings"
)

fsys <- setup_filesystem(folders = fsys, clear.reports = TRUE)

# Generate reports -------------------------------------------------------------
message("Generating reports...")

# epitrax <- setup_epitrax(
#   epitrax_file = "scripts/test_format_files/tricounty-data.csv",
#   config_file = file.path(fsys$settings, "report_config.yaml"),
#   disease_list_files = list(
#     internal = file.path(fsys$settings, "internal_report_diseases.csv"),
#     public = file.path(fsys$settings, "public_report_diseases.csv")
#   )) |>
#   epitrax_preport_month_crosssections(month_offsets = 0:1) |>
#   epitrax_write_pdf_month_crosssections(fsys = fsys)

epitrax <- setup_epitrax(
  epitrax_file = "scripts/test_format_files/tricounty-data.csv",
  config_file = file.path(fsys$settings, "report_config.yaml"),
  disease_list_files = list(
    internal = file.path(fsys$settings, "grouped_internal_report_diseases.csv"),
    public = file.path(fsys$settings, "public_report_diseases.csv")
  )) |>
  epitrax_ireport_monthly_counts_all_yrs() |>
  epitrax_ireport_annual_counts() |>
  epitrax_report_grouped_stats() |>
  epitrax_write_xlsxs(fsys = fsys) |>
  epitrax_write_pdf_grouped_stats(params = list(author = "LHD", title = "GR"),
                                  fsys = fsys)

message(".......Done.")
