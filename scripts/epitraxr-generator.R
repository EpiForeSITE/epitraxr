# ------------------------------------------------------------------------------
# title: EpiTraxr Report Generator (Package Version)
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
# It is based on the epitrax-report-automation ForeSITE project, which is
# available on GitHub at
# https://github.com/EpiForeSITE/epitrax-report-automation.
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

report_config <- read_report_config(file.path(fsys$settings,
                                              "report_config.yaml"))

# Read in EpiTrax data ---------------------------------------------------------
epitrax_data <- read_epitrax_data()
epitrax_data_yrs <- get_yrs(epitrax_data$year)
epitrax_data_diseases <- unique(epitrax_data$disease)
report_year <- max(epitrax_data_yrs)
report_month <- max(epitrax_data[epitrax_data$year == report_year,]$month)

diseases <- get_internal_disease_list(
  file.path(fsys$settings, "internal_report_diseases.csv"),
  default_diseases = epitrax_data_diseases
)


# Annual counts for each disease -----------------------------------------------
annual_counts <- create_report_annual_counts(
  epitrax_data,
  diseases$EpiTrax_name
)
# - Write to CSV
if (report_config$generate_csvs) {
  write_report_csv(annual_counts, "annual_counts.csv", fsys$internal)
}
# - Add to Excel List
xl_files[["annual_counts"]] <- annual_counts


# Monthly counts for each year -------------------------------------------------
for (y in epitrax_data_yrs) {
  # Create monthly counts report
  m_df <- create_report_monthly_counts(
    data = epitrax_data,
    y = y,
    disease_names = diseases$EpiTrax_name
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
epitrax_data_prev_yrs <- epitrax_data[epitrax_data$year != report_year,]

internal_monthly_avgs <- create_report_monthly_avgs(
  data = epitrax_data_prev_yrs,
  disease_names = diseases$EpiTrax_name,
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
  data = epitrax_data,
  disease_names = diseases$EpiTrax_name,
  y = report_year,
  m = report_month,
  config = report_config,
  as.rates = FALSE
)

ytd_report_rates <- create_report_ytd_counts(
  data = epitrax_data,
  disease_names = diseases$EpiTrax_name,
  y = report_year,
  m = report_month,
  config = report_config,
  as.rates = TRUE
)

# - Write to CSV
if (report_config$generate_csvs) {
  write_report_csv(ytd_report_counts, "ytd_report_counts.csv", fsys$internal)
  write_report_csv(ytd_report_rates, "ytd_report_rates.csv", fsys$internal)
}

# - Add to Excel List
xl_files[["ytd_report_counts"]] <- ytd_report_counts
xl_files[["ytd_report_rates"]] <- ytd_report_rates

# Combine internal reports into single .xlsx file ------------------------------
write_report_xlsx(
  data = xl_files,
  filename = "internal_reports_combined.xlsx",
  folder = fsys$internal
)


# Prepare Public Reports -------------------------------------------------------
xl_files <- list()

diseases <- get_public_disease_list(
  file.path(fsys$settings, "public_report_diseases.csv"),
  default_diseases = epitrax_data_diseases
)

monthly_avgs <- create_report_monthly_avgs(
  data = epitrax_data_prev_yrs,
  disease_names = diseases$EpiTrax_name,
  config = report_config
)

# - Create monthly cross-section report for most recent 4 months
for (offset in 0:3) {
  r <- create_public_report_month(
    cases = month_counts,
    avgs = monthly_avgs,
    d_list = diseases,
    m = report_month - offset,
    y = report_year,
    config = report_config,
    r_folder = fsys$public
  )
  xl_files[[r[["name"]]]] <- r[["report"]]
}

# - Create current YTD report
ytd_report_rates <- prep_report_data(ytd_report_rates, diseases$EpiTrax_name)

r <- create_public_report_ytd(
  ytd_rates <- ytd_report_rates,
  d_list = diseases,
  config = report_config,
  r_folder = fsys$public
)
xl_files[[r[["name"]]]] <- r[["report"]]

# - Combine public reports into single .xlsx file
write_report_xlsx(
  data = xl_files,
  filename = "public_reports_combined.xlsx",
  folder = fsys$public
)
