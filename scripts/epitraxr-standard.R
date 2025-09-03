# ------------------------------------------------------------------------------
# title: EpiTraxr Report Generator (Standard Mode)
# author: Andrew Pulsipher
# date: 2025-07-10
#
# This script generates monthly and YTD reports from EpiTrax data using the
# epitraxr package functions. It reads in EpiTrax data, processes it, and
# generates reports in both internal and public formats.
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

xl_files <- list() # Internal reports to combine into single .xlsx file

report_config <- get_report_config(file.path(fsys$settings,
                                              "report_config.yaml"))

# Read in EpiTrax data ---------------------------------------------------------
epitrax <- get_epitrax()

report_diseases <- get_report_diseases(
  internal = file.path(fsys$settings, "internal_report_diseases.csv"),
  public = file.path(fsys$settings, "public_report_diseases.csv"),
  defaults = epitrax$diseases
)

# Annual counts for each disease -----------------------------------------------
annual_counts <- create_report_annual_counts(
  epitrax$data,
  report_diseases$internal$EpiTrax_name
)
# - Write to CSV
if (report_config$generate_csvs) {
  write_report_csv(annual_counts, "annual_counts.csv", fsys$internal)
}
# - Add to Excel List
xl_files[["annual_counts"]] <- annual_counts


# Monthly counts for each year -------------------------------------------------
for (y in epitrax$yrs) {
  # Create monthly counts report
  m_df <- create_report_monthly_counts(
    data = epitrax$data,
    diseases = report_diseases$internal$EpiTrax_name,
    y = y
  )

  # - Write to CSV
  fname <- paste0("monthly_counts_", y)
  if (report_config$generate_csvs) {
    write_report_csv(m_df, paste0(fname, ".csv"), fsys$internal)
  }

  # - Add to Excel List
  xl_files[[fname]] = m_df
}


# Monthly average counts for all years except current year ---------------------
# - Extract all previous years
epitrax_data_prev_yrs <- epitrax$data[epitrax$data$year != epitrax$report_year,]

internal_monthly_avgs <- create_report_monthly_avgs(
  data = epitrax_data_prev_yrs,
  diseases = report_diseases$internal$EpiTrax_name,
  config = report_config
)

# - Write to CSV
if (report_config$generate_csvs) {
  avgs_fname <- with(epitrax_data_prev_yrs,
                     paste0("monthly_avgs_", min(year), "-", max(year), ".csv"))
  write_report_csv(internal_monthly_avgs, avgs_fname, fsys$internal)
}

# - Add to Excel List
xl_files[["monthly_avgs"]] <- internal_monthly_avgs


# YTD reports for current month ------------------------------------------------
ytd_report_counts <- create_report_ytd_counts(
  data = epitrax$data,
  diseases = report_diseases$internal$EpiTrax_name,
  y = epitrax$report_year,
  m = epitrax$report_month,
  config = report_config,
  as.rates = FALSE
)

ytd_report_rates <- create_report_ytd_counts(
  data = epitrax$data,
  diseases = report_diseases$internal$EpiTrax_name,
  y = epitrax$report_year,
  m = epitrax$report_month,
  config = report_config,
  as.rates = TRUE
)

# - Write to CSV
if (report_config$generate_csvs) {
  write_report_csv(ytd_report_counts, "ytd_counts.csv", fsys$internal)
  write_report_csv(ytd_report_rates, "ytd_rates.csv", fsys$internal)
}

# - Add to Excel List
xl_files[["ytd_counts"]] <- ytd_report_counts
xl_files[["ytd_rates"]] <- ytd_report_rates

# Combine internal reports into single .xlsx file ------------------------------
write_report_xlsx(
  data = xl_files,
  filename = "internal_reports_combined.xlsx",
  folder = fsys$internal
)


# Prepare Public Reports -------------------------------------------------------
xl_files <- list()

# - Create monthly cross-section report for most recent 4 months
for (offset in 0:3) {
  r <- create_public_report_month(
    data = epitrax$data,
    diseases = report_diseases$public,
    y = epitrax$report_year,
    m = epitrax$report_month - offset,
    config = report_config
  )
  if (report_config$generate_csvs) {
    write_report_csv(r$report, paste0(r$name, ".csv"), fsys$public)
  }
  xl_files[[r$name]] <- r$report
}

# - Create current YTD report
r <- create_public_report_ytd(
  data = epitrax$data,
  diseases = report_diseases$public,
  y = epitrax$report_year,
  m = epitrax$report_month,
  config = report_config
)
if (report_config$generate_csvs) {
  write_report_csv(r$report, paste0(r$name, ".csv"), fsys$public)
}
xl_files[[r[["name"]]]] <- r[["report"]]

# - Combine public reports into single .xlsx file
write_report_xlsx(
  data = xl_files,
  filename = "public_reports_combined.xlsx",
  folder = fsys$public
)

# Clean up script
rm(fsys, xl_files, report_config, epitrax, report_diseases, annual_counts, m_df,
   fname, epitrax_data_prev_yrs, internal_monthly_avgs, avgs_fname,
   ytd_report_counts, ytd_report_rates, r, offset, y)
