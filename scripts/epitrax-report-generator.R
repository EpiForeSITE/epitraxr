# ------------------------------------------------------------------------------
# title: EpiTrax Report Generator
# author: Andrew Pulsipher
# date: 2025-06-11
#
# This script generates monthly and YTD reports from EpiTrax data.
# It reads in EpiTrax data, processes it, and generates reports in both internal
# and public formats.
#
# The script is maintained by the Insight Net center ForeSITE. All the code is
# available on GitHub at
# https://github.com/EpiForeSITE/epitrax-report-automation.
#
# For questions, updates, or bug reports, please visit the GitHub repository.
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(lubridate)
library(writexl)
library(yaml)

# Define Helper Functions ------------------------------------------------------

#' Set up filesystem
#'
#' `create_filesystem` creates the given folders if they don't already exist.
#'
#' @param internal Filepath. Folder to hold internal reports.
#' @param public Filepath. Folder to hold public reports.
#' @param settings Filepath. Folder to hold report settings.
#'
#' @returns NULL.
create_filesystem <- function(internal, public, settings) {
  # - Create folders if needed
  for (f in c(internal, public, settings)) {
    if (!dir.exists(f)) {
      dir.create(f)
    }
  }
}

#' Read in the report config YAML file
#'
#' 'read_report_config' reads in the config YAML file
#'
#' @param config_filepath Filepath. Path to report config file.
#'
#' @returns a named list with an attribute of 'keys' from the file.
read_report_config <- function(config_filepath) {

  if (file.exists(config_filepath)) {
    config <- read_yaml(config_filepath)

    # - Validate or set defaults
    if (is.null(config$current_population) ||
        class(config$current_population) != "integer") {
      warning("In '", config_filepath, "', 'current_population' is missing or
            invalid. Using default value of 100,000 instead.")
      config$current_population <- 100000
    }

    if (is.null(config$avg_5yr_population) ||
        class(config$avg_5yr_population) != "integer") {
      warning("In '", config_filepath, "', 'avg_5yr_population' is missing or
            invalid. Using default value of 'current_population' instead.")
      config$avg_5yr_population <- config$current_population
    }

    if (is.null(config$rounding_decimals) ||
        class(config$rounding_decimals) != "integer") {
      warning("In '", config_filepath, "', 'rounding_decimals' is missing or
            invalid. Using default value of 2 instead.")
      config$rounding_decimals <- 2
    }

    if (is.null(config$generate_csvs) ||
        class(config$generate_csvs) != "logical") {
      warning("In '", config_filepath, "', 'generate_csvs' is missing or
            invalid. Using default value of TRUE instead.")
      config$generate_csvs <- TRUE
    }

    config
  } else {

    warning("No report configuration file provided. Using default values:
            'current_population' = 100,000
            'avg_5yr_population' = 100,000
            'rounding_decimals' = 2
            'generate_csvs' = TRUE")

    config <- list(current_population = 100000,
                   avg_5yr_population = 100000,
                   rounding_decimals = 2,
                   generate_csvs = TRUE)

    config
  }
}

#' Clear out old reports before generating new ones.
#'
#' `clear_old_reports` deletes reports from previous runs and returns a list of
#' the reports that were deleted.
#'
#' @param i_folder Filepath. Folder containing internal reports.
#' @param p_folder Filepath. Folder containing public reports.
#'
#' @returns The list of old reports that were cleared.
clear_old_reports <- function(i_folder, p_folder) {
  # - Remove old internal reports
  i_reports <- list(list.files(i_folder, full.names = TRUE))
  p_reports <- list(list.files(p_folder, full.names = TRUE))

  old_reports <- c(i_reports, p_reports)

  do.call(file.remove, old_reports)

  old_reports
}

#' Write report CSV files
#'
#' `write_report_csv` writes the given data to the specified folder with the
#' given filename.
#'
#' @param data Dataframe. Report data.
#' @param filename String. Report filename.
#' @param folder Filepath. Report destination folder.
#'
#' @returns NULL.
write_report_csv <- function(data, filename, folder) {
  write.csv(data, file.path(folder, filename), row.names = FALSE)
}

#' Validate input EpiTrax data
#'
#' 'validate_data' checks the data for expected columns and data types, removes
#' unneeded columns, and returns the resulting data.
#'
#' @param data Dataframe. EpiTrax data to validate.
#'
#' @returns The validated data with all unneeded columns removed.
validate_data <- function(data) {

  # Check column names
  expected_cols <- c(
    integer = "patient_mmwr_year",
    integer = "patient_mmwr_week",
    character = "patient_disease"
    )

  actual_cols <- colnames(data)

  if (!all(expected_cols %in% actual_cols)) {
    stop(
      "The EpiTrax data is missing one of the following fields:\n\n\t'",
      paste(expected_cols, collapse="', '"),
      "'",
      "\n\nThe following fields were found:\n\n\t'",
      paste(actual_cols, collapse="', '"),
      "'",
      "\n\nPlease add the missing fields to the file and try again."
    )
  }

  # Check column data types
  test_tmp <- Map(\(col, cls) {
    class(data[[col]]) != cls
  }, col = expected_cols, cls = names(expected_cols)) |> unlist()

  if (any(test_tmp)) {
    stop(
      "One or more columns in the EpiTrax dataset has an incorrect data type:",
      "\n\n",
      paste0(
        "\t- '",
        expected_cols[test_tmp], "' should be of type '",
        names(expected_cols)[test_tmp],
        "' but it is of type '",
        sapply(data[expected_cols[test_tmp]], class),
        "'",
        collapse = "\n\t"
        ),
      "\n\nPlease try again with a valid dataset."
    )
  }

  # Remove all columns we're not using
  # - Note this also rearranges the columns into the order of expected_cols
  data <- data[expected_cols]

  # Remove rows with missing or NA values
  if (any(is.na(data))) {
    warning("The EpiTrax dataset contains missing or NA values which will be ",
            "ignored when generating reports.")
  }
  data <- na.omit(data)

  data
}

#' Format input EpiTrax data
#'
#' 'format_week_num' formats the input EpiTrax dataset with month numbers
#' using the field 'patient_mmwr_week' and filters rows older than five years.
#'
#' @param data Dataframe. Data to format.
#'
#' @returns The formatted data.
format_week_num <- function(data) {
  # Format data
  data$month <- with(data, month(
    ymd(patient_mmwr_year * 10000 + 0101) +
      patient_mmwr_week * 7
  ))
  data$patient_mmwr_week <- NULL
  data$counts <- 1 # Makes easier to use aggregate()
  colnames(data) <- c("year", "disease", "month", "counts")
  # - Rearrange columns for easier debugging
  data <- data[c("disease", "month", "year", "counts")]

  # - Extract last years of data
  data <- with(data, data[year >= (max(year) - 5), ])

  data
}

#' Read in input EpiTrax data
#'
#' 'read_epitrax_data' reads EpiTrax data from a CSV, validates and formats it,
#' then returns the data.
#'
#' @returns The validated and formatted EpiTrax data from the input file.
read_epitrax_data <- function() {

  # Have user choose a file
  fpath <- file.choose()

  if (!file.exists(fpath) || !grepl("\\.csv$", fpath)) {
    stop("Please select an EpiTrax data file (.csv).")
  }

  # Read data from file
  input_data <- read.csv(fpath, header = TRUE)

  # Validate and format data
  input_data <- validate_data(input_data)
  input_data <- format_week_num(input_data)

  # Return data from file
  input_data
}

#' Reshape data frame with each month as a separate column
#'
#' 'reshape_monthly_wide' reshapes a given data frame with diseases for rows and
#' months for columns.
#'
#' @param df Dataframe. Data to reshape with months as columns.
#'
#' @returns The reshaped data frame.
reshape_monthly_wide <- function(df) {
  m_df <- with(df, reshape(
    merge(
      df,
      expand.grid(
        disease = unique(disease),
        month = unique(month)
      ),
      all = TRUE
    ),
    direction = "wide",
    idvar = "disease",
    timevar = "month"
  ))
  # - Set NA values to 0
  m_df[is.na(m_df)] <- 0
  # - Update column names to more human-readable format
  colnames(m_df) <- c("disease", month.abb[1:(ncol(m_df) - 1)])

  m_df
}

#' Get the internal disease list
#'
#' 'get_internal_disease_list' reads the internal list from a given CSV file or
#' uses the default diseases, if the file doesn't exist.
#'
#' The provided internal disease list file must contain one column of EpiTrax
#' disease names (EpiTrax_name) to include in internal reports.
#' @param filepath Filepath. Internal disease list CSV file.
#' @param default_diseases String vector. List of default diseases to use if the
#' above file doesn't exist.
#'
#' @returns A dataframe containing the diseases to include in the public report
#' and the name to use for each disease in the public report.
get_internal_disease_list <- function(filepath, default_diseases) {

  if (file.exists(filepath)) {

    d_list <- read.csv(filepath, header = TRUE)

    # Validate file
    if (is.null(d_list$EpiTrax_name)) {
      stop("File '", filepath, "' missing required column 'EpiTrax_name'.")
    }

    d_list

  } else {
    # If the file doesn't exist, use the default list of diseases provided
    warning("You have not provided a disease list for internal reports.",
            "\n - The program will default to using only the diseases ",
            "found in the input dataset.",
            "\n - If you would like to use a different list, ",
            "please include a file named \n\n\t'",
            filepath,
            "'\n\n - The file must have a column named",
            "\n\n\t'EpiTrax_name'")

    default_diseases <- sort(default_diseases)

    d_list <- data.frame(
      EpiTrax_name = default_diseases
    )

    d_list
  }
}

#' Get the public disease list
#'
#' 'get_public_disease_list' reads the public list from a given CSV file or uses
#' the default diseases if the file doesn't exist.
#'
#' The provided public disease list file must contain two columns that map the
#' EpiTrax disease name to a public-facing name for the public report.
#' @param filepath Filepath. Public disease list CSV file.
#' @param default_diseases String vector. List of default diseases to use if the
#' above file doesn't exist.
#'
#' @returns A dataframe containing the diseases to include in the public report
#' and the name to use for each disease in the public report.
get_public_disease_list <- function(filepath, default_diseases) {

  if (file.exists(filepath)) {

    d_list <- read.csv(filepath, header = TRUE)

    # Validate file
    if (is.null(d_list$EpiTrax_name) || is.null(d_list$Public_name)) {
      stop("File '", filepath, "' is incorrectly formatted. Please use the ",
           "column names: 'EpiTrax_name' and 'Public_name'.")
    }

    d_list

  } else {
    # If the file doesn't exist, use the default list of diseases provided
    warning("You have not provided a disease list for public reports.",
            "\n - The program will default to using only the diseases ",
            "found in the input dataset.",
            "\n - If you would like to use a different list, ",
            "please include a file named \n\n\t'",
            filepath,
            "'\n\n - The file must have columns named",
            "\n\n\t'EpiTrax_name' and 'Public_name'")

    default_diseases <- sort(default_diseases)

    d_list <- data.frame(
      EpiTrax_name = default_diseases,
      Public_name = default_diseases
    )

    d_list
  }
}

#' Prepare data for report
#'
#' 'prep_report_data' removes rows from the data that shouldn't appear in the
#' report and adds rows for diseases that should be in the report, but weren't
#' in the input dataset. Added rows are filled with 0s.
#'
#' @param data Dataframe. Current report data.
#' @param report_d_list String vector. Diseases to include in the report.
#'
#' @returns Report data with rows for all diseases to report.
prep_report_data <- function(data, report_d_list) {

  # - Remove rows from data that aren't going into the public report
  data <- subset(data, disease %in% report_d_list)

  # - Get diseases from report list that weren't in the data
  missing_diseases <- report_d_list[!(report_d_list %in% data$disease)]

  # If there are any missing diseases, add them
  if (length(missing_diseases) > 0) {
    # - Fill the missing diseases in with 0
    missing_data <- data.frame(
      disease = missing_diseases
    )

    missing_cols <- colnames(data)[2:length(colnames(data))]
    missing_data[, missing_cols] <- 0.0

    # - Combine data with missing_data
    data <- rbind(data, missing_data)

    # - Sort alphabetically so missing diseases are correctly placed
    data <- data[order(data$disease),]
  }

  data
}

#' Convert case counts to rate
#'
#' 'convert_counts_to_rate' converts case counts for a given population to an
#' adjusted per population of size X and rounds to the given number of digits.
#'
#' @param counts Integer(s). Case counts to convert.
#' @param pop Integer. Population size where cases were counted.
#' @param digits Integer. Number of decimals to round to.
#' @param rate_adj_pop Integer. Optional target population to use for rate.
#' Defaults to 100k.
#'
#' @returns The count(s) as rates per rate_adj_pop.
convert_counts_to_rate <- function(counts, pop, digits, rate_adj_pop = 100000) {
  round(counts / pop * rate_adj_pop, digits = digits)
}

#' Get Trend column of report
#'
#' 'get_trend' compares values of two columns and produces a new column
#' containing the trend result.
#'
#' @param col1 List. Current data.
#' @param col2 List. Historical comparison data.
#'
#' @returns Column containing the Trend markers.
get_trend <- function(col1, col2) {
  mapply(function(x, y) {
    ifelse(x > y, "Elevated", ifelse(x < y, "Less Than Expected", "Expected"))
  }, col1, col2)
}

#' Create a monthly cross-section public report
#'
#' 'create_public_report_month' creates a public report for the given month.
#'
#' @param cases Dataframe. Disease case counts for each month and year.
#' @param avgs Dataframe. Disease case count averages for each month.
#' @param d_list Dataframe. List of diseases to use for the report.
#' @param m Integer. The report month.
#' @param y Integer. The report year.
#' @param config List. Settings to use for report.
#' @param r_folder Filepath. Destination folder for the public report.
#'
#' @returns List containing the report name and data.
create_public_report_month <- function(cases, avgs, d_list, m, y, config, r_folder) {

  month_name <- month.abb[[m]]

  m_counts <- with(cases, cases[year == y & month == m, c("disease", "counts")])

  # - Only take the rows with data in the final report
  m_counts <- subset(m_counts, disease %in% avgs$disease)

  # - Convert monthly average counts to rate per 100k
  m_rates <- convert_counts_to_rate(avgs[[month_name]],
                                    pop = config$avg_5yr_population,
                                    digits = config$rounding_decimals)
  # - Create the report data frame initializing the Rate_per_100k column to 0
  m_report <- data.frame(
    Disease = avgs$disease,
    Rate_per_100k = 0,
    Avg_5yr_Rate = m_rates
  )

  # - Update the Rate_per_100k column with values from m_counts
  for (i in 1:length(m_counts$disease)) {
    d <- m_counts$disease[i]
    rate <- convert_counts_to_rate(m_counts$counts[i],
                                   pop = config$current_population,
                                   digits = config$rounding_decimals)
    m_report[m_report$Disease == d, ]$Rate_per_100k <- rate
  }

  # - Convert disease names to public-facing versions
  m_report <- merge(m_report, d_list,
                    by.x = "Disease", by.y = "EpiTrax_name",
                    all.x = TRUE, all.y = FALSE)
  m_report$Disease <- m_report$Public_name
  m_report$Public_name <- NULL
  m_report <- m_report[order(m_report$Disease),]

  # - Combine diseases with same public name (if any)
  m_report <- aggregate(m_report[ , -1], by = list(Disease = m_report$Disease), "sum")

  # - Add Trends column last
  m_report$Trend <- get_trend(m_report$Rate_per_100k, m_report$Avg_5yr_Rate)

  # - Write to CSV file
  r_name <- paste0("public_report_", month_name, report_year)
  if (config$generate_csvs) {
    write_report_csv(m_report, paste0(r_name, ".csv"), r_folder)
  }

  list("name" = r_name, "report" = m_report)
}

#' Create a YTD public report
#'
#' 'create_public_report_ytd' creates a public report for YTD rates.
#'
#' @param ytd_rates Dataframe. YTD case rates per 100k.
#' @param d_list Dataframe. List of diseases to use for the report.
#' @param config List. Settings to use for report.
#' @param r_folder Filepath. Destination folder for the public report.
#'
#' @returns List containing the report name and data.
create_public_report_ytd <- function(ytd_rates, d_list, config, r_folder) {

  # - Create the report data frame initializing the Rate_per_100k column to 0
  m_report <- data.frame(
    Disease = ytd_rates$disease,
    YTD_Rate_per_100k = ytd_rates$Current_YTD_Rate_per_100k,
    Avg_5yr_Rate = ytd_rates$Avg_5yr_YTD_Rate_per_100k
  )

  # - Convert disease names to public-facing versions
  m_report <- merge(m_report, d_list, by.x = "Disease", by.y = "EpiTrax_name")
  m_report$Disease <- m_report$Public_name
  m_report$Public_name <- NULL
  m_report <- m_report[order(m_report$Disease),]

  # - Combine diseases with same public name (if any)
  m_report <- aggregate(m_report[ , -1], by = list(Disease = m_report$Disease), "sum")

  # - Add Trends column last
  m_report$Trend <- get_trend(m_report$YTD_Rate_per_100k, m_report$Avg_5yr_Rate)

  # - Write to CSV file
  r_name <- "public_report_YTD"
  if (config$generate_csvs) {
    write_report_csv(m_report, paste0(r_name, ".csv"), r_folder)
  }

  list("name" = r_name, "report" = m_report)
}


# Set up file system -----------------------------------------------------------
internal_folder <- "internal_reports"
public_folder <- "public_reports"
settings_folder <- "report_settings"

xl_files <- list() # Internal reports to combine into single .xlsx file

create_filesystem(
  internal = internal_folder,
  public = public_folder,
  settings = settings_folder
)

clear_old_reports(internal_folder, public_folder)

report_config <- read_report_config(file.path(settings_folder,
                                              "report_config.yaml"))

# Read in EpiTrax data ---------------------------------------------------------
epitrax_data <- read_epitrax_data()
epitrax_data_yrs <- sort(unique(epitrax_data$year))
epitrax_data_diseases <- unique(epitrax_data$disease)
report_year <- max(epitrax_data_yrs)
report_month <- max(epitrax_data[epitrax_data$year == report_year,]$month)

diseases <- get_internal_disease_list(
  file.path(settings_folder, "internal_report_diseases.csv"),
  default_diseases = epitrax_data_diseases
)


# Annual counts for each disease -----------------------------------------------
annual_counts <- aggregate(counts ~ disease + year,
                           data = epitrax_data,
                           FUN = sum)
# - Reshape data to use years as columns and diseases as rows
annual_counts <- with(annual_counts, reshape(
  merge(
    annual_counts,
    expand.grid(
      disease = unique(disease),
      year = unique(year)
    ),
    all = TRUE
  ),
  direction = "wide",
  idvar = "disease",
  timevar = "year"
))
# - Set NA values to 0
annual_counts[is.na(annual_counts)] <- 0
# - Update column names to more human-readable format
colnames(annual_counts) <- c("disease", epitrax_data_yrs)
# - Add missing diseases
annual_counts <- prep_report_data(annual_counts, diseases$EpiTrax_name)
# - Write to CSV
if (report_config$generate_csvs) {
  write_report_csv(annual_counts, "annual_counts.csv", internal_folder)
}
# - Add to Excel List
xl_files[["annual_counts"]] <- annual_counts


# Monthly counts for each year -------------------------------------------------
month_counts <- aggregate(counts ~ disease + year + month,
                          data = epitrax_data,
                          FUN = sum)

for (y in epitrax_data_yrs) {
  # - Extract counts for given year
  m_df <- month_counts[month_counts$year == y, ]

  # - Remove year column (don't want to save it to CSV)
  m_df$year <- NULL

  # - Reshape data to use months as columns and disease as rows
  m_df <- reshape_monthly_wide(m_df)

  # - Add missing diseases
  m_df <- prep_report_data(m_df, diseases$EpiTrax_name)

  # - Write to CSV
  fname <- paste0("monthly_counts_", y)
  if (report_config$generate_csvs) {
    write_report_csv(m_df, paste0(fname, ".csv"), internal_folder)
  }

  # - Add to Excel List
  xl_files[[fname]] = m_df
}


# Monthly average counts for all years except current year ---------------------
# - Extract all previous years
epitrax_data_prev_yrs <- epitrax_data[epitrax_data$year != report_year,]
num_yrs <- length(unique(epitrax_data_prev_yrs$year))

# - Compute average counts for each month
monthly_avgs <- aggregate(counts ~ disease + month,
                          data = epitrax_data_prev_yrs,
                          FUN = sum)

monthly_avgs$counts <- round(monthly_avgs$counts / num_yrs,
                             digits = report_config$rounding_decimals)

# - Reshape data to use months as columns and disease as rows
monthly_avgs <- reshape_monthly_wide(monthly_avgs)

# - Write to CSV
avgs_fname <- with(epitrax_data_prev_yrs,
                   paste0("monthly_avgs_", min(year), "-", max(year), ".csv"))
# - Add missing diseases
internal_monthly_avgs <- prep_report_data(monthly_avgs, diseases$EpiTrax_name)
if (report_config$generate_csvs) {
  write_report_csv(internal_monthly_avgs, avgs_fname, internal_folder)
}

# - Add to Excel List
xl_files[["monthly_avgs"]] <- internal_monthly_avgs


# YTD reports for current month ------------------------------------------------
current_ytd <- month_counts[month_counts$year == report_year, ]
current_ytd <- aggregate(counts ~ disease, data = current_ytd, FUN = sum)
current_ytd <- prep_report_data(current_ytd, diseases$EpiTrax_name)

avg_5yr_ytd <- with(month_counts, month_counts[year != report_year &
                                                 month <= report_month, ])
avg_5yr_ytd <- aggregate(counts ~ disease, data = avg_5yr_ytd, FUN = sum)
avg_5yr_ytd$counts <- avg_5yr_ytd$counts / num_yrs
avg_5yr_ytd <- prep_report_data(avg_5yr_ytd, diseases$EpiTrax_name)

ytd_report_counts <- data.frame(
  disease = current_ytd$disease,
  Current_YTD_Counts = current_ytd$counts,
  Avg_5yr_YTD_Counts = avg_5yr_ytd$counts
)

ytd_report_rates <- data.frame(
  disease = current_ytd$disease,
  Current_YTD_Rate_per_100k = convert_counts_to_rate(
    current_ytd$counts,
    pop = report_config$current_population,
    digits = report_config$rounding_decimals),
  Avg_5yr_YTD_Rate_per_100k = convert_counts_to_rate(
    avg_5yr_ytd$counts,
    pop = report_config$avg_5yr_population,
    digits = report_config$rounding_decimals)
)

# - Write to CSV
if (report_config$generate_csvs) {
  write_report_csv(ytd_report_counts, "ytd_report_counts.csv", internal_folder)
  write_report_csv(ytd_report_rates, "ytd_report_rates.csv", internal_folder)
}

# - Add to Excel List
xl_files[["ytd_report_counts"]] <- ytd_report_counts
xl_files[["ytd_report_rates"]] <- ytd_report_rates

# Combine internal reports into single .xlsx file ------------------------------
write_xlsx(xl_files, file.path(internal_folder,
                               "internal_reports_combined.xlsx"))


# Prepare Public Reports -------------------------------------------------------
xl_files <- list()

diseases <- get_public_disease_list(
  file.path(settings_folder, "public_report_diseases.csv"),
  default_diseases = epitrax_data_diseases
)

monthly_avgs <- prep_report_data(monthly_avgs, diseases$EpiTrax_name)

# - Create monthly cross-section report for most recent 4 months
r <- create_public_report_month(
  cases = month_counts,
  avgs = monthly_avgs,
  d_list = diseases,
  m = report_month,
  y = report_year,
  config = report_config,
  r_folder = public_folder
)
xl_files[[r[["name"]]]] <- r[["report"]]

r <- create_public_report_month(
  cases = month_counts,
  avgs = monthly_avgs,
  d_list = diseases,
  m = report_month - 1,
  y = report_year,
  config = report_config,
  r_folder = public_folder
)
xl_files[[r[["name"]]]] <- r[["report"]]

r <- create_public_report_month(
  cases = month_counts,
  avgs = monthly_avgs,
  d_list = diseases,
  m = report_month - 2,
  y = report_year,
  config = report_config,
  r_folder = public_folder
)
xl_files[[r[["name"]]]] <- r[["report"]]

r <- create_public_report_month(
  cases = month_counts,
  avgs = monthly_avgs,
  d_list = diseases,
  m = report_month - 3,
  y = report_year,
  config = report_config,
  r_folder = public_folder
)
xl_files[[r[["name"]]]] <- r[["report"]]

# - Create current YTD report
ytd_report_rates <- prep_report_data(ytd_report_rates, diseases$EpiTrax_name)

r <- create_public_report_ytd(
  ytd_rates <- ytd_report_rates,
  d_list = diseases,
  config = report_config,
  r_folder = public_folder
)
xl_files[[r[["name"]]]] <- r[["report"]]

# - Combine public reports into single .xlsx file
write_xlsx(xl_files, file.path(public_folder, "public_reports_combined.xlsx"))
