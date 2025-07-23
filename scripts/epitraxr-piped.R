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
epitrax <- setup_epitrax(
    config_file = file.path(fsys$settings, "report_config.yaml"),
    disease_list_files = list(
      internal = file.path(fsys$settings, "internal_report_diseases.csv"),
      public = file.path(fsys$settings, "public_report_diseases.csv")
    ))
  epitrax_ireport_annual_counts() |>
  epitrax_ireport_monthly_counts_all_yrs() |>
  epitrax_ireport_monthly_avgs(exclude.report.year = TRUE) |>
  epitrax_ireport_ytd_counts_for_month(as.rates = FALSE) |>
  epitrax_ireport_ytd_counts_for_month(as.rates = TRUE) |>
  epitrax_preport_month_crosssections(month_offsets = 0:3) |>
  epitrax_preport_ytd_rates() |>
  epitrax_write_csvs(fsys = fsys) |>
  epitrax_write_xlsxs(fsys = fsys)
