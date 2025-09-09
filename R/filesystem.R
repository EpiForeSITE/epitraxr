#' Create filesystem
#'
#' `create_filesystem` creates the given folders if they don't already exist.
#'
#' @param internal Filepath. Folder for internal reports.
#' @param public Filepath. Folder for public reports.
#' @param settings Filepath. Folder for report settings.
#'
#' @returns NULL.
#' @export
#'
#' @examples
#' internal_folder = file.path(tempdir(), "internal")
#' public_folder = file.path(tempdir(), "public")
#' settings_folder = file.path(tempdir(), "settings")
#'
#' create_filesystem(
#'   internal = internal_folder,
#'   public = public_folder,
#'   settings = settings_folder
#' )
#'
#' unlink(c(internal_folder, public_folder, settings_folder), recursive = TRUE)
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
#' @inheritParams create_filesystem
#'
#' @returns The list of old reports that were cleared.
#' @export
#'
#' @examples
#' ireports_folder <- file.path(tempdir(), "internal")
#' preports_folder <- file.path(tempdir(), "public")
#' dir.create(ireports_folder)
#' dir.create(preports_folder)
#'
#' clear_old_reports(ireports_folder, preports_folder)
#' unlink(c(ireports_folder, preports_folder), recursive = TRUE)
clear_old_reports <- function(internal, public) {
  # - Remove old internal reports
  i_reports <- list(list.files(internal, full.names = TRUE))
  p_reports <- list(list.files(public, full.names = TRUE))

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
#'   - `internal`: Folder for internal reports
#'   - `public`: Folder for public reports
#'   - `settings`: Folder for settings files
#' @param clear.reports Logical. Whether to clear old reports from the internal
#'   and public folders. Defaults to FALSE.
#'
#' @returns The input folders list, unchanged.
#' @seealso [create_filesystem()], [clear_old_reports()] which this function wraps.
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
#' unlink(unlist(folders, use.names = FALSE), recursive = TRUE)
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


#' Read in the report config YAML file
#'
#' 'get_report_config' reads in the config YAML file. Missing fields
#' will be set to default values and a warning will be issued. The
#' config file can have the following fields:
#' - `current_population`: Integer. Current population size.
#' - `avg_5yr_population`: Integer. Average population over the last 5 years.
#' - `rounding_decimals`: Integer. Number of decimals to round report values to.
#' - `generate_csvs`: Logical. Whether to generate CSV files.
#' - `trend_threshold`: Numeric. Threshold for trend calculations.
#'
#' See the example config file here:
#' `system.file("sample_data/sample_config.yml", package = "epitraxr")`.
#'
#' @param filepath Filepath. Path to report config file.
#'
#' @returns A named list with an attribute of 'keys' from the file.
#' @export
#'
#' @importFrom yaml read_yaml
#'
#' @examples
#' config_file <- system.file("sample_data/sample_config.yml",
#'                           package = "epitraxr")
#' report_config <- get_report_config(config_file)
get_report_config <- function(filepath) {

  if (file.exists(filepath)) {
    config <- yaml::read_yaml(filepath)

    config <- validate_config(config)

    config
  } else {
    stop(
      "No report configuration file provided. Please provide a valid file or",
      "use `epitraxr_config()` to create a config programmatically."
    )
  }
}


#' Get the internal disease list
#'
#' 'get_report_diseases_internal' reads the internal list from a given CSV file or
#' uses the default diseases, if the file doesn't exist.
#'
#' The provided internal disease list file must contain at least a column named
#' `EpiTrax_name` which contains EpiTrax disease names to include in the report.
#' The file can optionally contain a column named `Group_name`, which maps the
#' diseases in `EpiTrax_name` to a disease group. This is only used for reports
#' that include disease groupings.
#'
#' See the example file here:
#' `system.file("sample_data/sample_disease_list.csv", package = "epitraxr")`
#'
#' @param filepath Filepath. Internal disease list CSV file.
#' @param defaults String vector. List of default diseases to use if the
#' above file doesn't exist.
#'
#' @returns A dataframe containing the diseases to include in the internal report
#' and possibly the disease groupings.
#' @export
#'
#' @importFrom utils read.csv
#'
#' @examples
#' # Using default list (when file doesn't exist)
#' default_list <- c("Measles", "Chickenpox")
#' disease_list <- get_report_diseases_internal("", default_list)
#'
#' # Using a disease list file
#' list_file <- system.file("sample_data/sample_disease_list.csv",
#'                         package = "epitraxr")
#' disease_list <- get_report_diseases_internal(list_file, default_list)
get_report_diseases_internal <- function(filepath, defaults) {

  if (file.exists(filepath)) {

    diseases <- utils::read.csv(filepath, header = TRUE)

    # Validate file
    if (is.null(diseases$EpiTrax_name)) {
      stop("File '", filepath, "' missing required column 'EpiTrax_name'.")
    }

    diseases

  } else {
    # If the file doesn't exist, use the default list of diseases provided
    warning(
      "You have not provided a disease list for internal reports.",
      "\n - The program will default to using only the diseases ",
      "found in the input dataset.",
      "\n - If you would like to use a different list, ",
      "please include a file with a column named",
      "\n\n\t'EpiTrax_name'\n"
    )

    defaults <- sort(defaults)

    diseases <- data.frame(
      EpiTrax_name = defaults
    )

    diseases
  }
}


#' Get the public disease list
#'
#' 'get_report_diseases_public' reads the public list from a given CSV file or uses
#' the default diseases if the file doesn't exist.
#'
#' The provided public disease list file must contain two columns named
#' `EpiTrax_name` and `Public_name` which map EpiTrax disease names to
#' a public-facing name for the public report. The file can optionally
#' contain a column named `Group_name`, which maps the diseases in
#' `EpiTrax_name` to a disease group. This is only used for reports that
#' include disease groupings.
#'
#' See the example file here:
#' `system.file("sample_data/sample_disease_list.csv", package = "epitraxr")`
#'
#' @param filepath Filepath. Public disease list CSV file.
#' @param defaults String vector. List of default diseases to use if the
#' above file doesn't exist.
#'
#' @returns A dataframe containing the diseases to include in the public report
#' and the name to use for each disease in the public report. It may also contain
#' the disease groupings.
#' @export
#'
#' @importFrom utils read.csv
#'
#' @examples
#' # Using default list (when file doesn't exist)
#' default_list <- c("Measles", "Chickenpox")
#' disease_list <- get_report_diseases_public("", default_list)
#'
#' # Using a disease list file
#' list_file <- system.file("sample_data/sample_disease_list.csv",
#'                         package = "epitraxr")
#' disease_list <- get_report_diseases_public(list_file, default_list)
get_report_diseases_public <- function(filepath, defaults) {

  if (file.exists(filepath)) {

    diseases <- utils::read.csv(filepath, header = TRUE)

    # Validate file
    if (is.null(diseases$EpiTrax_name) || is.null(diseases$Public_name)) {
      stop("File '", filepath, "' is incorrectly formatted. Please use the ",
           "column names: 'EpiTrax_name' and 'Public_name'.")
    }

    diseases

  } else {
    # If the file doesn't exist, use the default list of diseases provided
    warning(
      "You have not provided a disease list for public reports.",
      "\n - The program will default to using only the diseases ",
      "found in the input dataset.",
      "\n - If you would like to use a different list, ",
      "please include a file with columns named",
      "\n\n\t'EpiTrax_name' and 'Public_name'\n"
    )

    defaults <- sort(defaults)

    diseases <- data.frame(
      EpiTrax_name = defaults,
      Public_name = defaults
    )

    diseases
  }
}


#' Get both internal and public disease lists
#'
#' `get_report_diseases` is a convenience function that combines
#' `get_report_diseases_internal` and `get_report_diseases_public`.
#'
#' @param internal Filepath. Path to internal disease list CSV file.
#' @param public Filepath. Path to public disease list CSV file.
#' @param defaults String vector. List of default diseases to use if
#'   either file doesn't exist.
#'
#' @returns A list with two elements:
#'   - `internal`: Dataframe with EpiTrax_name column
#'   - `public`: Dataframe with EpiTrax_name and Public_name columns
#' @seealso [get_report_diseases_internal()], [get_report_diseases_public()] which this function wraps.
#' @export
#'
#' @examples
#' # Using default lists (when files don't exist)
#' default_list <- c("Measles", "Chickenpox")
#' disease_lists <- get_report_diseases("", "", default_list)
#'
#' # Using disease list files
#' i_file <- system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                        package = "epitraxr")
#' p_file <- system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                        package = "epitraxr")
#' disease_lists <- get_report_diseases(
#'   internal = i_file,
#'   public = p_file,
#'   defaults = default_list
#' )
get_report_diseases <- function(internal, public, defaults) {
  # Get internal disease list
  internal_diseases <- get_report_diseases_internal(
    filepath = internal,
    defaults = defaults
  )

  # Get public disease list
  public_diseases <- get_report_diseases_public(
    filepath = public,
    defaults = defaults
  )

  # Return both lists
  list(
    internal = internal_diseases,
    public = public_diseases
  )
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
#' unlink(file.path(tempdir(), "report.csv"), recursive = TRUE)
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
#' @inheritParams write_report_csv
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
#' unlink(file.path(tempdir(), "report.xlsx"), recursive = TRUE)
write_report_xlsx <- function(data, filename, folder) {
  writexl::write_xlsx(data, file.path(folder, filename))
}


#' Write general PDF report of disease stats from R Markdown template
#'
#' `write_report_pdf` renders a report as a PDF using a R Markdown
#' template. It is relatively flexible and can be used for various
#' types of report.
#'
#' @inheritParams write_report_csv
#' @param params List. Report parameters containing:
#'   - `title`: Report title (defaults to "Disease Report")
#'   - `report_year`: Report year (defaults to 2025)
#'   - `report_month`: Report month (defaults to 1)
#'   - `trend_threshold`: Threshold for trend calculations (defaults to 0.15)
#' @param trend.only Logical. Whether to show only trend in the PDF report.
#' If TRUE, "trend_only_" will be prepended to the filename.
#'
#' @returns NULL (called for side effects - creates the report file).
#' @export
#'
#' @examples
#' # Don't run PDF examples in case missing LaTeX
#' if (requireNamespace("rmarkdown")) {
#'   # Create sample report data
#'   r_data <- data.frame(
#'     Disease = c("COVID", "Flu", "Measles"),
#'     `March 2024` = c(0, 25, 5),
#'     `Historical March Avg` = c(0, 15, 8),
#'     `Trend` = compute_trend(c(0, 25, 5), c(0, 15, 8)),
#'     check.names = FALSE
#'   )
#'
#'   # Set report parameters
#'   params <- list(
#'     title = "Monthly Disease Surveillance Report",
#'     report_year = 2024,
#'     report_month = 3,
#'     trend_threshold = 0.20
#'   )
#'
#'   # Write to temporary directory
#'   write_report_pdf(
#'     data = r_data,
#'     params = params,
#'     filename = "monthly_disease_report.pdf",
#'     folder = tempdir()
#'   )
#' }
write_report_pdf <- function(data, params, filename, folder, trend.only = FALSE) {
  # Check if rmarkdown is available
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "Package 'rmarkdown' is required to run 'write_report_pdf()',",
      "but is not available"
    )
  }

  # Get template path
  template_path <- system.file(
    "report_formats/general_report.Rmd",
    package = "epitraxr"
  )

  # Check if output directory exists, create if not
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }

  # Correct filename if trend.only is TRUE
  corrected_filename <- ifelse(trend.only,
                               paste0("trend_only_", filename),
                               filename)

  # Render the report
  rmarkdown::render(
    input = template_path,
    params = list(
      title = params$title %||% "Disease Report",
      report_year = params$report_year %||% 2025,
      report_month = params$report_month %||% 1,
      trend_threshold = params$trend_threshold %||% 0.15,
      report_data = data,
      trend_only = trend.only
    ),
    output_file = corrected_filename,
    output_dir = folder,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
}


#' Write PDF grouped report from R Markdown template
#'
#' `write_report_pdf_grouped` renders a grouped disease statistics report
#' as a PDF using a R Markdown template. The report includes comprehensive
#' disease statistics organized by groups with current and historical data.
#'
#' @inheritParams write_report_pdf
#' @param params List. Report parameters containing:
#'   - `title`: Report title (defaults to "Grouped Report")
#'   - `report_year`: Report year (defaults to 2025)
#'   - `report_month`: Report month (defaults to 1)
#'   - `trend_threshold`: Threshold for trend calculations (defaults to 0.15)
#'
#' @returns NULL (called for side effects - creates the report file).
#' @export
#'
#' @examples
#' # Don't run PDF examples in case missing LaTeX
#' if (requireNamespace("rmarkdown")) {
#'   # Create sample grouped report data
#'   r_data <- data.frame(
#'     Group = c("Respiratory", "Respiratory", "Vaccine-Preventable"),
#'     Disease = c("COVID", "Flu", "Measles"),
#'     `March 2024` = c(0, 25, 5),
#'     `March 2024 Rate` = c(0, 25, 5),
#'     `Historical March Avg` = c(0, 15, 8),
#'     `Historical March Median` = c(0, 15, 8),
#'     `2024 YTD` = c(0, 37, 9),
#'     `Historical 2024 YTD Avg` = c(20, 25, 14),
#'     `Historical 2024 YTD Median` = c(20, 25, 14),
#'     `YTD Trend` = compute_trend(c(0, 37, 9), c(20, 25, 14)),
#'     check.names = FALSE
#'   )
#'
#'   # Set report parameters
#'   params <- list(
#'     title = "Grouped Disease Surveillance Report",
#'     report_year = 2024,
#'     report_month = 3,
#'     trend_threshold = 0.20
#'   )
#'
#'   # Write to temporary directory
#'   write_report_pdf_grouped(
#'     data = r_data,
#'     params = params,
#'     filename = "grouped_disease_report.pdf",
#'     folder = tempdir()
#'   )
#' }
write_report_pdf_grouped <- function(data, params, filename, folder, trend.only = FALSE) {
  # Check if rmarkdown is available
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "Package 'rmarkdown' is required to run 'write_report_pdf_grouped()',",
      "but is not available"
    )
  }

  # Get template path
  template_path <- system.file(
    "report_formats/grouped_report.Rmd",
    package = "epitraxr"
  )

  # Check if output directory exists, create if not
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }

  # Correct filename if trend.only is TRUE
  corrected_filename <- ifelse(trend.only,
                              paste0("trend_only_", filename),
                              filename)

  # Render the report
  rmarkdown::render(
    input = template_path,
    params = list(
      title = params$title %||% "Grouped Report",
      report_year = params$report_year %||% 2025,
      report_month = params$report_month %||% 1,
      trend_threshold = params$trend_threshold %||% 0.15,
      report_data = data,
      trend_only = trend.only
    ),
    output_file = corrected_filename,
    output_dir = folder,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
}
