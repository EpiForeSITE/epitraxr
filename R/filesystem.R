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
#'  internal_folder = file.path(tempdir(), "internal")
#'  public_folder = file.path(tempdir(), "public")
#'  settings_folder = file.path(tempdir(), "settings")
#'
#'  create_filesystem(
#'    internal = internal_folder,
#'    public = public_folder,
#'    settings = settings_folder
#'  )
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
#' clear_old_reports(tempdir(), tempdir())
clear_old_reports <- function(i_folder, p_folder) {
  # - Remove old internal reports
  i_reports <- list(list.files(i_folder, full.names = TRUE))
  p_reports <- list(list.files(p_folder, full.names = TRUE))

  old_reports <- c(i_reports, p_reports)

  do.call(file.remove, old_reports)

  old_reports
}


#' Setup the report filesystem
#'
#' `setup_filesystem` creates the necessary folder structure and optionally clears
#' old reports. This is a convenience function that combines `create_filesystem`
#' and `clear_old_reports`.
#'
#' @param folders List. Contains paths to report folders with elements:
#'   - internal: Folder for internal reports
#'   - public: Folder for public reports
#'   - settings: Folder for settings files
#' @param clear.reports Logical. Whether to clear old reports from the internal
#'   and public folders. Defaults to FALSE.
#'
#' @returns The input folders list, unchanged.
#' @export
#'
#' @examples
#' # Create folders in a temporary directory
#' folders <- list(
#'   internal = file.path(tempdir(), "internal"),
#'   public = file.path(tempdir(), "public"),
#'   settings = file.path(tempdir(), "settings")
#' )
#' setup_filesystem(folders)
setup_filesystem <- function(folders, clear.reports = FALSE) {
  # Create the filesystem if it doesn't exist
  create_filesystem(
    internal = folders$internal,
    public = folders$public,
    settings = folders$settings
  )

  # Clear old reports if requested
  if (clear.reports) {
    clear_old_reports(folders$internal, folders$public)
  }

  folders
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


#' Read in the report config YAML file
#'
#' 'read_report_config' reads in the config YAML file
#'
#' @param config_filepath Filepath. Path to report config file.
#'
#' @returns A named list with an attribute of 'keys' from the file.
#' @export
#'
#' @importFrom yaml read_yaml
#'
#' @examples
#' # Using default values (when file doesn't exist)
#' report_config <- read_report_config("")
#'
#' # Using a config file
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                           package = "epitraxr")
#' report_config <- read_report_config(config_file)
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
#' # Create sample data
#' r_data <- data.frame(
#'   Disease = c("Measles", "Chickenpox"),
#'   Counts = c(20, 43)
#' )
#'
#' # Write to temporary directory
#' write_report_csv(r_data, "report.csv", tempdir())
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
#' # Create sample data with multiple sheets
#' r_data1 <- data.frame(
#'   Disease = c("Measles", "Chickenpox"),
#'   Counts = c(20, 43)
#' )
#' r_data2 <- data.frame(
#'   Disease = c("Measles", "Chickenpox"),
#'   Rate = c(10.5, 22.7)
#' )
#' r_xl <- list(
#'   counts = r_data1,
#'   rates = r_data2
#' )
#'
#' # Write to temporary directory
#' write_report_xlsx(r_xl, "report.xlsx", tempdir())
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
#' # Using default list (when file doesn't exist)
#' default_list <- c("Measles", "Chickenpox")
#' disease_list <- get_internal_disease_list("", default_list)
#'
#' # Using a disease list file
#' list_file <- system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                         package = "epitraxr")
#' disease_list <- get_internal_disease_list(list_file, default_list)
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
#' # Using default list (when file doesn't exist)
#' default_list <- c("Measles", "Chickenpox")
#' disease_list <- get_public_disease_list("", default_list)
#'
#' # Using a disease list file
#' list_file <- system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                         package = "epitraxr")
#' disease_list <- get_public_disease_list(list_file, default_list)
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


#' Get both internal and public disease lists
#'
#' `get_report_disease_lists` is a convenience function that combines
#' `get_internal_disease_list` and `get_public_disease_list`.
#'
#' @param internal_list_fp Filepath. Path to internal disease list CSV file.
#' @param public_list_fp Filepath. Path to public disease list CSV file.
#' @param default_diseases String vector. List of default diseases to use if
#'   either file doesn't exist.
#'
#' @returns A list with two elements:
#'   - internal: Dataframe with EpiTrax_name column
#'   - public: Dataframe with EpiTrax_name and Public_name columns
#' @export
#'
#' @examples
#' # Using default lists (when files don't exist)
#' default_list <- c("Measles", "Chickenpox")
#' disease_lists <- get_report_disease_lists("", "", default_list)
#'
#' # Using disease list files
#' i_file <- system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                        package = "epitraxr")
#' p_file <- system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                        package = "epitraxr")
#' disease_lists <- get_report_disease_lists(
#'   internal_list_fp = i_file,
#'   public_list_fp = p_file,
#'   default_diseases = default_list
#' )
get_report_disease_lists <- function(internal_list_fp, public_list_fp, default_diseases) {
  # Get internal disease list
  internal_diseases <- get_internal_disease_list(
    filepath = internal_list_fp,
    default_diseases = default_diseases
  )

  # Get public disease list
  public_diseases <- get_public_disease_list(
    filepath = public_list_fp,
    default_diseases = default_diseases
  )

  # Return both lists
  list(
    internal = internal_diseases,
    public = public_diseases
  )
}
