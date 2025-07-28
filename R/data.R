#' Format input EpiTrax data
#'
#' 'format_week_num' formats the input EpiTrax dataset with month numbers
#' using the field 'patient_mmwr_week' and filters rows older than five years.
#'
#' @param data Dataframe. Data to format.
#'
#' @returns The formatted data.
#' @export
#'
#' @importFrom lubridate month ymd
#'
#' @examples
#' df <- data.frame(patient_mmwr_year=2020L, patient_mmwr_week=1L, patient_disease="A")
#' format_week_num(df)
format_week_num <- function(data) {
  # Format data
  data$month <- with(data, lubridate::month(
    lubridate::ymd(patient_mmwr_year * 10000 + 0101) +
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
#' @param data_file Optional filepath. Data file should be a CSV. If this parameter
#' is NULL, the user will be prompted to choose a file interactively.
#'
#' @returns The validated and formatted EpiTrax data from the input file.
#' @export
#'
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' # Interactive file chooser:
#' read_epitrax_data()
#' }
#'
#' # Using a file path:
#' read_epitrax_data(system.file("sample_data/sample_epitrax_data.csv",
#'                               package = "epitraxr"))
read_epitrax_data <- function(data_file = NULL) {

  # If data_file is provided, use it; otherwise, prompt user to choose a file
  fpath <- ifelse(!is.null(data_file), data_file, file.choose())

  if (!file.exists(fpath) || !grepl("\\.csv$", fpath)) {
    stop("Please select an EpiTrax data file (.csv).")
  }

  # Read data from file
  data <- utils::read.csv(fpath, header = TRUE)

  # Validate and format data
  data <- validate_data(data)
  data <- format_week_num(data)

  # Return data from file
  data
}


#' Create an EpiTrax object from data file
#'
#' `get_epitrax` reads an EpiTrax data file and creates a structured object
#' containing the data along with commonly used metadata and empty report lists.
#'
#' @param data_file Optional filepath. Data file should be a CSV. If this parameter
#'   is NULL, the user will be prompted to choose a file interactively.
#'
#' @returns An object of class "epitrax" containing:
#'   - data: The validated and formatted EpiTrax data
#'   - diseases: Vector of unique diseases in the dataset
#'   - yrs: Vector of years in the dataset
#'   - report_year: Most recent year in the dataset
#'   - report_month: Most recent month in report_year
#'   - internal_reports: Empty list to store internal reports
#'   - public_reports: Empty list to store public reports
#' @export
#'
#' @examples
#' \dontrun{
#' # Interactive file chooser:
#' get_epitrax()
#' }
#'
#' # Using sample data included with package
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' epitrax <- get_epitrax(data_file)
#'
#' # Access components
#' head(epitrax$data)
#' epitrax$diseases
#' epitrax$report_year
get_epitrax <- function(data_file = NULL) {
  # Read in EpiTrax data
  epitrax_data <- read_epitrax_data(data_file)

  # Compute common summary statistics and metadata
  data_diseases <- unique(epitrax_data$disease)
  data_yrs <- get_yrs(epitrax_data)
  r_year <- max(data_yrs)
  r_month <- max(epitrax_data[epitrax_data$year == r_year,]$month)

  # Return list of EpiTrax data and metadata
  epitrax_obj <- structure(
    list(
      data = epitrax_data,
      diseases = data_diseases,
      yrs = data_yrs,
      report_year = r_year,
      report_month = r_month,
      internal_reports = list(),
      public_reports = list()
    ),
    class = "epitrax"
  )

  epitrax_obj
}

#' Reshape data frame with each month as a separate column
#'
#' 'reshape_monthly_wide' reshapes a given data frame with diseases for rows and
#' months for columns.
#'
#' @param df Dataframe. Data to reshape with months as columns.
#'
#' @returns The reshaped data frame.
#' @export
#'
#' @examples
#' df <- data.frame(disease=c("A","B"), month=c(1,2), counts=c(5,6))
#' reshape_monthly_wide(df)
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
  m_df <- set_na_0(m_df)
  # - Update column names to more human-readable format
  colnames(m_df) <- c("disease", month.abb[1:(ncol(m_df) - 1)])

  m_df
}


#' Reshape data frame with each year as a separate column
#'
#' 'reshape_annual_wide' reshapes a given data frame with diseases for rows and
#' years for columns.
#'
#' @param df Dataframe. Data to reshape with years as columns. Must have
#' columns: disease, year, and counts.
#'
#' @returns The reshaped data frame.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   disease = c("A", "A", "B"),
#'   year = c(2020, 2021, 2020),
#'   counts = c(5, 7, 8)
#' )
#' reshape_annual_wide(df)
reshape_annual_wide <- function(df) {
  a_df <- with(df, reshape(
    merge(
      df,
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
  a_df <- set_na_0(a_df)
  # - Update column names to more human-readable format
  colnames(a_df) <- c("disease", get_yrs(df))

  a_df
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
#' @export
#'
#' @examples
#' df <- data.frame(disease=c("A","B","D"), counts=c(5,7,8))
#' prep_report_data(df, c("A","C"))
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



#' Calculate monthly counts by disease
#'
#' `get_month_counts` aggregates disease counts by month and year. This is a helper
#' function used internally by report generation functions to summarize monthly
#' disease counts.
#'
#' @param data Dataframe. Must contain columns:
#'   - disease: The disease name
#'   - year: The year of the counts
#'   - month: The month of the counts (1-12)
#'   - counts: The number of cases
#'
#' @returns A dataframe with the aggregated monthly counts
#' @export
#'
#' @examples
#' df <- data.frame(
#'   disease = c("Flu", "Flu", "Measles"),
#'   year = c(2020, 2020, 2020),
#'   month = c(1, 1, 2),
#'   counts = c(5, 3, 2)
#' )
#' get_month_counts(df)
get_month_counts <- function(data) {
  stats::aggregate(counts ~ disease + year + month, data = data, FUN = sum)
}
