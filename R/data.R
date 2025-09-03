#' Read in EpiTrax data
#'
#' 'read_epitrax_data' reads EpiTrax data from a CSV, validates, and formats it.
#' It also filters rows older than given number of years. The input file must
#' contain the columns:
#' - `patient_mmwr_year` (integer)
#' - `patient_mmwr_week` (integer)
#' - `patient_disease` (character)
#'
#' See the example file here:
#' `system.file("sample_data/sample_epitrax_data.csv", package = "epitraxr")`
#'
#' @param filepath Optional filepath. Data file should be a CSV. If this parameter
#' is NULL, the user will be prompted to choose a file interactively.
#' @param num_yrs Integer. Number of years of data to keep. Defaults to 5.
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
#' data <- read_epitrax_data(
#'  filepath = system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr"),
#'  num_yrs = 3
#' )
#' head(data)
read_epitrax_data <- function(filepath = NULL, num_yrs = 5) {

  if (is.null(num_yrs) || !is.numeric(num_yrs) || num_yrs < 0) {
    stop("In 'read_epitrax_data', 'num_yrs' must be an integer >= 0.")
  }

  # If filepath is provided, use it; otherwise, prompt user to choose a file
  fpath <- filepath %||% file.choose()

  if (!file.exists(fpath) || !grepl("\\.csv$", fpath)) {
    stop("Please select an EpiTrax data file (.csv).")
  }

  # Read data from file
  data <- utils::read.csv(fpath, header = TRUE)

  # Validate and format data
  data <- validate_data(data)
  data <- mmwr_week_to_month(data)
  data <- format_epitrax_data(data)

  # Extract last x years of data
  data <- with(data, data[year >= (max(year) - num_yrs), ])

  # Return data from file
  data
}


#' Convert MMWR week to calendar month
#'
#' `mmwr_week_to_month` calculates the calendar month from the
#' `patient_mmwr_week` and `patient_mmwr_year` fields of the EpiTrax data.
#' The result is stored in the `month` column and the `patient_mmwr_week`
#' column is removed.
#'
#' @param data Dataframe. Must contain columns:
#' - `patient_mmwr_year` (integer)
#' - `patient_mmwr_week` (integer)
#'
#' @returns The input data frame with an added "month" column (integer 1-12) and
#' removed `patient_mmwr_week` column.
#' @export
#'
#' @importFrom lubridate month ymd
#'
#' @examples
#' df <- data.frame(
#'  patient_mmwr_year = 2020L,
#'  patient_mmwr_week = 1L,
#'  patient_disease = "A"
#' )
#' mmwr_week_to_month(df)
mmwr_week_to_month <- function(data) {
  # Format data
  data$month <- with(data, lubridate::month(
    lubridate::ymd(patient_mmwr_year * 10000 + 0101) +
      patient_mmwr_week * 7
  ))
  data$patient_mmwr_week <- NULL

  data
}


#' Format EpiTrax data for report generation
#'
#' `format_epitrax_data` prepares the input EpiTrax data for use by report generation
#' functions in the package. It adds the `counts` column, renames columns to
#' standard names used by the package ("disease", "month", "year", "counts"),
#' and rearranges columns for consistency.
#'
#' @param data Dataframe. Must contain columns:
#' - `patient_disease` (character, unchanged from EpiTrax export)
#' - `patient_mmwr_year` (integer, unchanged from EpiTrax export)
#' - `month` (integer, converted from `patient_mmwr_week` by `mmwr_week_to_month()`)
#'
#' @returns A standardized data frame with columns "disease", "month", "year", and "counts".
#' @seealso [mmwr_week_to_month()]
#' @export
#'
#' @examples
#' df <- data.frame(
#'   patient_mmwr_year = c(2020L, 2020L),
#'   month = c(1, 2),
#'   patient_disease = c("A", "B")
#' )
#' df <- format_epitrax_data(df)
format_epitrax_data <- function(data) {
  # Add counts column to make it easier to use aggregate()
  data$counts <- 1

  # Rename columns to standard names and rearrange for better debugging
  data <- data[c("patient_disease", "month", "patient_mmwr_year", "counts")]
  colnames(data) <- c("disease", "month", "year", "counts")

  data
}


#' Reshape data with each month as a separate column
#'
#' 'reshape_monthly_wide' reshapes a given data frame with diseases for rows and
#' months for columns.
#'
#' @param data Dataframe. Must contain columns:
#' - `disease` (character)
#' - `month` (integer)
#' - `counts` (integer)
#'
#' @returns The reshaped data frame.
#' @export
#'
#' @examples
#' df <- data.frame(
#'  disease = c("A", "B"),
#'  month = c(1, 2),
#'  counts = c(5, 6)
#' )
#' reshape_monthly_wide(df)
reshape_monthly_wide <- function(data) {
  m_df <- with(data, stats::reshape(
    merge(
      data,
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


#' Reshape data with each year as a separate column
#'
#' 'reshape_annual_wide' reshapes a given data frame with diseases for rows and
#' years for columns.
#'
#' @param data Dataframe. Must have columns:
#' - `disease` (character)
#' - `year` (integer)
#' - `counts` (integer)
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
reshape_annual_wide <- function(data) {
  a_df <- with(data, stats::reshape(
    merge(
      data,
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
  colnames(a_df) <- c("disease", get_yrs(data))

  a_df
}

#' Standardize diseases for report
#'
#' 'standardize_report_diseases' removes rows from the data that
#' shouldn't appear in the report and adds rows for diseases that
#' should be in the report, but weren't in the input dataset.
#' Added rows are filled with 0s.
#'
#' @param data Dataframe. Current report data.
#' @param diseases Character vector. Diseases to include in the report.
#'
#' @returns Report data with rows for all diseases to report.
#' @export
#'
#' @examples
#' df <- data.frame(disease=c("A","B","D"), counts=c(5,7,8))
#' standardize_report_diseases(df, c("A","C"))
standardize_report_diseases <- function(data, diseases) {

  # - Remove rows from data that aren't going into the public report
  data <- data[data$disease %in% diseases, ]

  # - Get diseases from report list that weren't in the data
  missing_diseases <- diseases[!(diseases %in% data$disease)]

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


#' Get unique years from the data
#'
#' 'get_yrs' extracts and returns the sorted unique years from the 'year' column
#' of a data frame.
#'
#' @param data Dataframe. Must contain the column:
#' - `year` (integer)
#'
#' @returns Integer vector of sorted unique years present in the data.
#' @export
#'
#' @examples
#' df <- data.frame(year = c(2020, 2021, 2020, 2022))
#' get_yrs(df)
get_yrs <- function(data) {
  sort(unique(data$year))
}


#' Get monthly counts for each disease
#'
#' `get_month_counts` aggregates disease counts by month and year. This is a helper
#' function used internally by report generation functions to summarize monthly
#' disease counts.
#'
#' @param data Dataframe. Must contain columns:
#'   - `disease` (character)
#'   - `year` (integer)
#'   - `month` (integer)
#'   - `counts` (integer)
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
