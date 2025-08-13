#' Validate an EpiTrax object
#'
#' `validate_epitrax` checks that the EpiTrax object is valid.
#'
#' @param epitrax Object of class `epitrax`.
#' @param report.check Logical indicating whether to check report-related fields.
#'
#' @returns NULL if valid, otherwise throws an error.
#' @export
#'
#' @examples
#' epitrax <- structure(
#'   list(
#'     data = c(1,2,3),
#'     config = list(rounding_decimals = 2, generate_csvs = TRUE),
#'     report_diseases = list(internal = "internal_list", public = "public_list")
#'   ),
#'   class = "epitrax"
#' )
#' validate_epitrax(epitrax, report.check = TRUE)
validate_epitrax <- function(epitrax, report.check = TRUE) {
    stopifnot(inherits(epitrax, "epitrax"))

    if (report.check) {
        stopifnot(is.list(epitrax$config))
        stopifnot(is.list(epitrax$report_diseases))
    }
}


#' Validate the filesystem structure
#'
#' `validate_filesystem` checks that the filesystem structure is valid.
#'
#' @param fsys List. Contains paths to report folders with elements:
#'   - internal: Folder for internal reports
#'   - public: Folder for public reports
#'   - settings: Folder for settings files
#'
#' @returns NULL if valid, otherwise throws an error.
#' @export
#'
#' @examples
#' fsys <- list(
#'   internal = "internal_reports",
#'   public = "public_reports",
#'   settings = "report_settings"
#' )
#' validate_filesystem(fsys)
validate_filesystem <- function(fsys) {
  stopifnot(is.character(fsys$internal))
  stopifnot(is.character(fsys$public))
  stopifnot(is.character(fsys$settings))
}


#' Validate a config list
#'
#' `validate_config` checks the values of the given config list. If any values
#' are missing or invalid, they are set to default values.
#'
#' @param config Named list.
#'
#' @returns A named list with 'keys' corresponding to config options.
#' @export
#'
#' @examples
#' validate_config(config = list())
validate_config <- function(config) {

    warnings <- c()

    if (is.null(config$current_population) ||
        !inherits(config$current_population, c("numeric", "integer"))) {
      warnings <- c(warnings, "\n - 'current_population' set to 100,000")
      config$current_population <- 100000
    }

    if (is.null(config$avg_5yr_population) ||
        !inherits(config$avg_5yr_population, c("numeric", "integer"))) {
      warnings <- c(warnings,
                    "\n - 'avg_5yr_population' set to 'current_population'")
      config$avg_5yr_population <- config$current_population
    }

    if (is.null(config$rounding_decimals) ||
        !inherits(config$rounding_decimals, c("numeric", "integer"))) {
      warnings <- c(warnings, "\n - 'rounding_decimals' set to 2")
      config$rounding_decimals <- 2
    }

    if (is.null(config$generate_csvs) ||
        !inherits(config$generate_csvs, "logical")) {
      warnings <- c(warnings, "\n - 'generate_csvs' set to TRUE")
      config$generate_csvs <- TRUE
    }

    if (is.null(config$trend_threshold) ||
        !inherits(config$trend_threshold, "numeric")) {
      warnings <- c(warnings, "\n - 'trend_threshold' set to 0.15")
      config$trend_threshold <- 0.15
    }

    if (length(warnings) > 0) {
        warning("These config fields are missing/invalid and will be set to
                defaults:\n", warnings)
    }

    config
}


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
#' df <- data.frame(
#'   patient_mmwr_year = 2020L,
#'   patient_mmwr_week = 1L,
#'   patient_disease = "A"
#' )
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
