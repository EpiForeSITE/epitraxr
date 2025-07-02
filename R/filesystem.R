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

    config
  } else {

    warning("No report configuration file provided. Using default values:
            'current_population' = 100,000
            'avg_5yr_population' = 100,000
            'rounding_decimals' = 2")

    config <- list(current_population = 100000,
                   avg_5yr_population = 100000,
                   rounding_decimals = 2)

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
