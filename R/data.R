#' Validate input EpiTrax data
#'
#' 'validate_data' checks the data for expected columns and data types, removes
#' unneeded columns, and returns the resulting data.
#'
#' @param data Dataframe. EpiTrax data to validate.
#'
#' @returns The validated data with all unneeded columns removed.
#' @export
#'
#' @importFrom stats na.omit
#'
#' @examples
#' df <- data.frame(patient_mmwr_year=2020L, patient_mmwr_week=1L, patient_disease="A")
#' validate_data(df)
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
  data <- stats::na.omit(data)

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
