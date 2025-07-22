#' Create filesystem
#'
#' `create_filesystem` creates the given folders if they don't already exist.
#'
#' @param internal Filepath. Folder to hold internal reports.
#' @param public Filepath. Folder to hold public reports.
#' @param settings Filepath. Folder to hold report settings.
#'
#' @returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#'   internal_folder <- "internal_reports"
#'   public_folder <- "public_reports"
#'   settings_folder <- "report_settings"
#'
#'   create_filesystem(
#'     internal = internal_folder,
#'     public = public_folder,
#'     settings = settings_folder
#'   )
#' }
create_filesystem <- function(internal, public, settings) {
  # - Create folders if needed
  for (f in c(internal, public, settings)) {
    if (!dir.exists(f)) {
      dir.create(f)
    }
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
#' @export
#'
#' @examples
#' \dontrun{
#'   clear_old_reports("internal_reports", "public_reports")
#' }
clear_old_reports <- function(i_folder, p_folder) {
  # - Remove old internal reports
  i_reports <- list(list.files(i_folder, full.names = TRUE))
  p_reports <- list(list.files(p_folder, full.names = TRUE))

  old_reports <- c(i_reports, p_reports)

  do.call(file.remove, old_reports)

  old_reports
}


#' Read in the report config YAML file
#'
#' 'read_report_config' reads in the config YAML file
#'
#' @param config_filepath Filepath. Path to report config file.
#'
#' @returns a named list with an attribute of 'keys' from the file.
#' @export
#'
#' @importFrom yaml read_yaml
#'
#' @examples
#' \dontrun{
#'   config_file <- "path/to/config_file"
#'   report_config <- read_report_config(config_file)
#' }
read_report_config <- function(config_filepath) {

  if (file.exists(config_filepath)) {
    config <- yaml::read_yaml(config_filepath)

    # - Validate or set defaults
    if (is.null(config$current_population) ||
        !inherits(config$current_population, "integer")) {
      warning("In '", config_filepath, "', 'current_population' is missing or
            invalid. Using default value of 100,000 instead.")
      config$current_population <- 100000
    }

    if (is.null(config$avg_5yr_population) ||
        !inherits(config$avg_5yr_population, "integer")) {
      warning("In '", config_filepath, "', 'avg_5yr_population' is missing or
            invalid. Using default value of 'current_population' instead.")
      config$avg_5yr_population <- config$current_population
    }

    if (is.null(config$rounding_decimals) ||
        !inherits(config$rounding_decimals, "integer")) {
      warning("In '", config_filepath, "', 'rounding_decimals' is missing or
            invalid. Using default value of 2 instead.")
      config$rounding_decimals <- 2
    }

    if (is.null(config$generate_csvs) ||
        !inherits(config$generate_csvs, "logical")) {
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
#' @export
#'
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#'   r_data <- data.frame(
#'     Disease = c("Measles", "Chickenpox"),
#'     Counts = c(20, 43)
#'   )
#'   r_file <- "report.csv"
#'   r_folder <- "reports"
#'
#'   write_report_csv(r_data, r_file, r_folder)
#' }
write_report_csv <- function(data, filename, folder) {
  utils::write.csv(data, file.path(folder, filename), row.names = FALSE)
}


#' Write report Excel files
#'
#' `write_report_xlsx` writes the given data to the specified folder with the
#' given filename in Excel (.xlsx) format.
#'
#' @param data List. Named list of dataframes. The name will be used as the
#' sheet name.
#' @param filename String. Report filename.
#' @param folder Filepath. Report destination folder.
#'
#' @returns NULL.
#' @export
#'
#' @importFrom writexl write_xlsx
#'
#' @examples
#' \dontrun{
#'   r_data <- data.frame(
#'     Disease = c("Measles", "Chickenpox"),
#'     Counts = c(20, 43)
#'   )
#'   r_file <- "report.xlsx"
#'   r_folder <- "reports"
#'
#'   write_report_xlsx(r_data, r_file, r_folder)
#' }
write_report_xlsx <- function(data, filename, folder) {
  writexl::write_xlsx(data, file.path(folder, filename))
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
#' @export
#'
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#'   list_file <- "path/to/file"
#'   default_list <- c("Measles", "Chickenpox")
#'
#'   disease_list <- get_internal_disease_list(list_file, default_list)
#' }
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
#' @export
#'
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#'   list_file <- "path/to/file"
#'   default_list <- c("Measles", "Chickenpox")
#'
#'   disease_list <- get_public_disease_list(list_file, default_list)
#' }
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
