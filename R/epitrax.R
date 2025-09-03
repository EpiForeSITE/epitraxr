#' Create an EpiTrax object from data file
#'
#' `get_epitrax` reads an EpiTrax data file and creates a structured object
#' containing the data along with commonly used metadata and empty report lists.
#'
#' @param data_file Optional filepath. Data file should be a CSV. If this parameter
#'   is NULL, the user will be prompted to choose a file interactively.
#' @param num_yrs Integer. Number of years of data to keep. Defaults to 5.
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
get_epitrax <- function(data_file = NULL, num_yrs = 5) {
  # Read in EpiTrax data
  epitrax_data <- read_epitrax_data(data_file, num_yrs = num_yrs)

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


#' Set report configuration of EpiTrax object from config file
#'
#' `epitrax_set_config_from_file` reads a report configuration file and adds it to the
#' EpiTrax object.
#'
#' @param epitrax Object of class `epitrax`.
#' @param filepath Path to the report configuration file.
#'
#' @returns Updated EpiTrax object with `config` field set.
#' @export
#'
#' @examples
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' epitrax <- structure(
#'   list(data = c(1,2,3)),
#'   class = "epitrax"
#' )
#' epitrax <- epitrax_set_config_from_file(epitrax, config_file)
epitrax_set_config_from_file <- function(epitrax, filepath) {

    validate_epitrax(epitrax, report.check = FALSE)

    epitrax$config <- get_report_config(filepath)

    epitrax
}


#' Set report configuration of EpiTrax object from list
#'
#' `epitrax_set_config_from_list` sets the report configuration from the given list.
#'
#' @param epitrax Object of class `epitrax`.
#' @param config Optional list of config parameters. If omitted, default values will be used.
#'
#' @returns Updated EpiTrax object with `config` field set.
#' @export
#'
#' @examples
#' config <- list(
#'  current_population = 56000,
#'  avg_5yr_population = 57000,
#'  rounding_decimals = 3,
#'  generate_csvs = FALSE
#' )
#' epitrax <- structure(
#'   list(data = c(1,2,3)),
#'   class = "epitrax"
#' )
#' epitrax <- epitrax_set_config_from_list(epitrax, config)
epitrax_set_config_from_list <- function(epitrax, config = NULL) {

    validate_epitrax(epitrax, report.check = FALSE)

    config <- config %||% list()

    if (inherits(config, "list")) {
        epitrax$config <- do.call(epitraxr_config, config)
    } else {
        stop("'config' must be a list.")
    }

    epitrax
}


#' Add report diseases to EpiTrax object
#'
#' `epitrax_add_report_diseases` reads internal and public disease lists and
#' adds them to the EpiTrax object.
#'
#' @param epitrax Object of class `epitrax`.
#' @param disease_list_files Optional list containing filepaths to internal and public
#'  disease lists. If omitted, the default lists will be used and a warning will be thrown.
#'
#' @returns Updated EpiTrax object with `report_diseases` field set.
#' @export
#'
#' @examples
#' i_file <- system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                        package = "epitraxr")
#' p_file <- system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                        package = "epitraxr")
#'
#' epitrax <- structure(
#'   list(data = c(1,2,3)),
#'   class = "epitrax"
#' )
#'
#' epitrax <- epitrax_add_report_diseases(
#'   epitrax,
#'   disease_list_files = list(
#'     internal = i_file,
#'     public = p_file
#'   )
#' )
epitrax_add_report_diseases <- function(epitrax, disease_list_files = NULL) {

    validate_epitrax(epitrax, report.check = FALSE)

    # Get internal and public disease lists
    diseases <- get_report_diseases(
        internal = disease_list_files$internal %||% "use_defaults",
        public = disease_list_files$public %||% "use_defaults",
        defaults = epitrax$diseases
    )

    # Add to epitrax object
    epitrax$report_diseases <- list(
        internal = diseases$internal,
        public = diseases$public
    )

    epitrax
}


#' Setup EpiTrax object with configuration and disease lists
#'
#' `setup_epitrax` initializes an EpiTrax object with configuration and report
#' disease lists. It is a convenience function that combines `get_epitrax`,
#' `epitrax_set_config_from_file`, and `epitrax_add_report_diseases`.
#'
#' @param epitrax_file Optional path to the EpiTrax data file. Data file should
#' be a CSV. If omitted, the user will be prompted to choose a file interactively.
#' @param num_yrs Integer. Number of years of data to keep. Defaults to 5.
#' @param disease_list_files Optional list containing filepaths to internal and
#' public report disease lists. If omitted, the default lists will be used and
#' a warning will be thrown.
#' @param config_list,config_file Configuration options may be specified as a
#' list or as a path to a YAML config file, respectively. Only one can be
#' specified at a time. If both are specified, the function will return an
#' error. If both are omitted, the default config values will be used.
#'
#' @returns An EpiTrax object with configuration and report diseases set.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' disease_lists <- list(
#'   internal = system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                          package = "epitraxr"),
#'   public = system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                        package = "epitraxr")
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   disease_list_files = disease_lists
#' )
setup_epitrax <- function(epitrax_file = NULL, num_yrs = 5, disease_list_files = NULL, config_list = NULL, config_file = NULL) {

    if (!is.null(config_list) && !is.null(config_file)) {
        stop(
            "'config_list' and 'config_file' may not both be specified.",
            "Please specify one or the other."
        )
    }

    epitrax <- get_epitrax(epitrax_file, num_yrs = num_yrs) |>
        epitrax_add_report_diseases(disease_list_files)

    if (!is.null(config_file)) {
        epitrax <- epitrax_set_config_from_file(epitrax, filepath = config_file)
    } else {
        epitrax <- epitrax_set_config_from_list(epitrax, config = config_list)
    }

    epitrax
}


#' Create annual counts internal report from an EpiTrax object
#'
#' `epitrax_ireport_annual_counts` generates an internal report of annual
#' counts for each disease in the EpiTrax object data.
#'
#' @param epitrax Object of class `epitrax`.
#'
#' @returns Updated EpiTrax object with `annual_counts` added to the
#' `internal_reports` field.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_ireport_annual_counts()
#'
#' epitrax$internal_reports$annual_counts
epitrax_ireport_annual_counts <- function(epitrax) {

    validate_epitrax(epitrax)

    # Create annual counts report
    annual_counts <- create_report_annual_counts(
        epitrax$data,
        epitrax$report_diseases$internal$EpiTrax_name
    )

    # Add to epitrax object
    epitrax$internal_reports$annual_counts <- annual_counts

    epitrax
}


#' Create monthly counts internal report for all years from an EpiTrax object
#'
#' `epitrax_ireport_monthly_counts_all_yrs` generates internal reports of
#' monthly counts for each year in the EpiTrax object data.
#'
#' @param epitrax Object of class `epitrax`.
#'
#' @returns Updated EpiTrax object with monthly counts reports for each year
#' added to the `internal_reports` field.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_ireport_monthly_counts_all_yrs()
#'
#' names(epitrax$internal_reports)
epitrax_ireport_monthly_counts_all_yrs <- function(epitrax) {

    validate_epitrax(epitrax)

    # Create monthly counts for each year
    for (y in epitrax$yrs) {
        m_df <- create_report_monthly_counts(
            data = epitrax$data,
            diseases = epitrax$report_diseases$internal$EpiTrax_name,
            y = y
        )

        # Add to internal reports
        epitrax$internal_reports[[paste0("monthly_counts_", y)]] <- m_df
    }

    epitrax
}


#' Create monthly averages internal report from an EpiTrax object
#'
#' `epitrax_ireport_monthly_avgs` generates an internal report of monthly
#' averages for all years in the EpiTrax object data, with the option to exclude
#' the current report year.
#'
#' @param epitrax Object of class `epitrax`.
#' @param exclude.report.year Logical indicating whether to exclude the current
#' report year from the averages. Defaults to FALSE.
#'
#' @returns Updated EpiTrax object with monthly averages report added to the
#' `internal_reports` field.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_ireport_monthly_avgs()
#'
#' names(epitrax$internal_reports)
epitrax_ireport_monthly_avgs <- function(epitrax, exclude.report.year = FALSE) {

    validate_epitrax(epitrax)

    # Create monthly averages
    r_data <- epitrax$data
    if (exclude.report.year) {
        r_data <- r_data[r_data$year != epitrax$report_year,]
    }

    monthly_avgs <- create_report_monthly_avgs(
        data = r_data,
        diseases = epitrax$report_diseases$internal$EpiTrax_name,
        config = epitrax$config
    )

    # Add to internal reports
    r_name <- paste0("monthly_avgs_", min(r_data$year), "-", max(r_data$year))
    epitrax$internal_reports[[r_name]] <- monthly_avgs

    epitrax
}


#' Create year-to-date (YTD) counts internal report for a given month
#' from an EpiTrax object
#'
#' `epitrax_ireport_ytd_counts_for_month` generates an internal report of
#' year-to-date counts up to a specific month in the EpiTrax object data.
#'
#' @param epitrax Object of class `epitrax`.
#' @param as.rates Logical. If TRUE, returns rates per 100k instead of raw counts.
#'
#' @returns Updated EpiTrax object with report added to the `internal_reports` field.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_ireport_ytd_counts_for_month(as.rates = TRUE)
#'
#' names(epitrax$internal_reports)
epitrax_ireport_ytd_counts_for_month <- function(epitrax, as.rates = FALSE) {

    validate_epitrax(epitrax)

    # Create YTD counts
    ytd_counts <- create_report_ytd_counts(
        data = epitrax$data,
        diseases = epitrax$report_diseases$internal$EpiTrax_name,
        y = epitrax$report_year,
        m = epitrax$report_month,
        config = epitrax$config,
        as.rates = as.rates
    )

    # Add to internal reports
    r_name <- ifelse(as.rates, "ytd_rates", "ytd_counts")
    epitrax$internal_reports[[r_name]] <- ytd_counts

    epitrax
}


#' Create monthly cross-section reports from an EpiTrax object
#'
#' `epitrax_preport_month_crosssections` generates monthly cross-section
#' reports. These compare the counts for a given month against the
#' monthly averages for the same month across previous years.
#'
#' @param epitrax Object of class `epitrax`.
#' @param month_offsets Numeric vector of month offsets to create reports for.
#' Defaults to 0:3, which generates reports for the current month and the three
#' previous months.
#'
#' @returns Updated EpiTrax object with monthly cross-section reports added to
#' the `public_reports` field.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_preport_month_crosssections(month_offsets = 0:1)
#'
#' names(epitrax$public_reports)
epitrax_preport_month_crosssections <- function(epitrax, month_offsets = 0:3) {

    validate_epitrax(epitrax)

    # Create monthly cross-section reports
    for (offset in month_offsets) {
        r <- create_public_report_month(
            data = epitrax$data,
            diseases = epitrax$report_diseases$public,
            y = epitrax$report_year,
            m = epitrax$report_month - offset,
            config = epitrax$config
        )

        # Add to public reports
        epitrax$public_reports[[r$name]] <- r$report
    }

    epitrax
}


#' Create year-to-date (YTD) rates public report from an EpiTrax object
#'
#' `epitrax_preport_ytd_rates` generates a public report of year-to-date
#' rates for the current month in the EpiTrax object data.
#'
#' @param epitrax Object of class `epitrax`.
#'
#' @returns Updated EpiTrax object with YTD rates report added to the
#' `public_reports` field.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_preport_ytd_rates()
#'
#' names(epitrax$public_reports)
epitrax_preport_ytd_rates <- function(epitrax) {

    validate_epitrax(epitrax)

    # Create public report
    r <- create_public_report_ytd(
        data = epitrax$data,
        diseases = epitrax$report_diseases$public,
        y = epitrax$report_year,
        m = epitrax$report_month,
        config = epitrax$config
    )

    # Add to public reports
    epitrax$public_reports[[r$name]] <- r$report

    epitrax
}


#' Create combined monthly/YTD stats public report from an EpiTrax object
#'
#' `epitrax_preport_combined_month_ytd` generates a public report of
#' monthly and year-to-date (YTD) disease statistics for the report month
#' in the EpiTrax object data.
#'
#' @param epitrax Object of class `epitrax`.
#'
#' @returns Updated EpiTrax object with YTD rates report added to the
#' `public_reports` field.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_preport_combined_month_ytd()
#'
#' names(epitrax$public_reports)
epitrax_preport_combined_month_ytd <- function(epitrax) {

    validate_epitrax(epitrax)

    # Create combined month and YTD report
    r <- create_public_report_combined_month_ytd(
        data = epitrax$data,
        diseases = epitrax$report_diseases$public,
        y = epitrax$report_year,
        m = epitrax$report_month,
        config = epitrax$config
    )

    # Add to public reports
    epitrax$public_reports[[r$name]] <- r$report

    epitrax
}


#' Create monthly medians report from an EpiTrax object
#'
#' `epitrax_report_monthly_medians` generates a report of monthly medians for all years
#' in the EpiTrax object data, with the option to exclude the current report year.
#' It can be run for either internal or public reports.
#'
#' @param epitrax Object of class `epitrax`.
#' @param is.public Logical indicating whether to generate a public report using
#' the public disease list. If FALSE (default), generates an internal report using
#' the internal disease list.
#' @param exclude.report.year Logical indicating whether to exclude the current
#' report year from the medians calculation. Defaults to FALSE.
#'
#' @returns Updated EpiTrax object with monthly medians report added to either
#' the `internal_reports` or `public_reports` field, depending on the `is.public`
#' parameter.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_report_monthly_medians()
#'
#' names(epitrax$internal_reports)
epitrax_report_monthly_medians <- function(epitrax, is.public = FALSE, exclude.report.year = FALSE) {

    validate_epitrax(epitrax)

    # Extract data
    r_data <- epitrax$data
    if (exclude.report.year) {
        r_data <- r_data[r_data$year != epitrax$report_year,]
    }

    # Select disease list
    if (is.public) {
        report_diseases <- epitrax$report_diseases$public$EpiTrax_name
    } else {
        report_diseases <- epitrax$report_diseases$internal$EpiTrax_name
    }

    # Create monthly medians
    monthly_medians <- create_report_monthly_medians(
        data = r_data,
        diseases = report_diseases
    )

    # Add report to EpiTrax object
    r_name <- paste0("monthly_medians_", min(r_data$year), "-", max(r_data$year))
    if (is.public) {
        epitrax$public_reports[[r_name]] <- monthly_medians
    } else {
        epitrax$internal_reports[[r_name]] <- monthly_medians
    }


    epitrax
}


#' Create year-to-date (YTD) medians report from an EpiTrax object
#'
#' `epitrax_report_ytd_medians` generates a report of median year-to-date counts
#' for each disease up to the current report month across all years in the EpiTrax
#' object data, with the option to exclude the current report year. It can be run for
#' either internal or public reports.
#'
#' @param epitrax Object of class `epitrax`.
#' @param is.public Logical indicating whether to generate a public report using
#' the public disease list. If FALSE (default), generates an internal report using
#' the internal disease list.
#' @param exclude.report.year Logical indicating whether to exclude the current
#' report year from the medians calculation. Defaults to FALSE.
#'
#' @returns Updated EpiTrax object with YTD medians report added to either
#' the `internal_reports` or `public_reports` field, depending on the `is.public`
#' parameter.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_report_ytd_medians()
#'
#' names(epitrax$internal_reports)
epitrax_report_ytd_medians <- function(epitrax, is.public = FALSE, exclude.report.year = FALSE) {

    validate_epitrax(epitrax)

    # Extract data
    r_data <- epitrax$data
    if (exclude.report.year) {
        r_data <- r_data[r_data$year != epitrax$report_year,]
    }

    # Select disease list
    if (is.public) {
        report_diseases <- epitrax$report_diseases$public$EpiTrax_name
    } else {
        report_diseases <- epitrax$report_diseases$internal$EpiTrax_name
    }

    # Create YTD medians
    ytd_medians <- create_report_ytd_medians(
        data = r_data,
        diseases = report_diseases,
        m = epitrax$report_month
    )

    # Add report to EpiTrax object
    r_name <- paste0("ytd_medians_", min(r_data$year), "-", max(r_data$year))
    if (is.public) {
        epitrax$public_reports[[r_name]] <- ytd_medians
    } else {
        epitrax$internal_reports[[r_name]] <- ytd_medians
    }

    epitrax
}


#' Create grouped disease statistics report from an EpiTrax object
#'
#' `epitrax_report_grouped_stats` generates a comprehensive report with current
#' and historical statistics for diseases organized by group. The report includes
#' monthly counts/rates, historical averages and medians, year-to-date counts, and
#' trend analysis. It can be run for either internal or public reports.
#'
#' @param epitrax Object of class `epitrax`.
#' @param is.public Logical indicating whether to generate a public report using
#' the public disease list. If FALSE (default), generates an internal report using
#' the internal disease list.
#'
#' @returns Updated EpiTrax object with grouped statistics report added to either
#' the `internal_reports` or `public_reports` field, depending on the `is.public`
#' parameter.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_report_grouped_stats()
#'
#' names(epitrax$internal_reports)
epitrax_report_grouped_stats <- function(epitrax, is.public = FALSE) {

    validate_epitrax(epitrax)

    # Select disease list
    if (is.public) {
        report_diseases <- epitrax$report_diseases$public
    } else {
        report_diseases <- epitrax$report_diseases$internal
    }

    # Create grouped stats report
    grouped_stats <- create_report_grouped_stats(
        data = epitrax$data,
        diseases = report_diseases,
        y = epitrax$report_year,
        m = epitrax$report_month,
        config = epitrax$config
    )

    # Add report to EpiTrax object
    r_name <- paste0("grouped_stats_", min(epitrax$data$year), "-", max(epitrax$data$year))
    if (is.public) {
        epitrax$public_reports[[r_name]] <- grouped_stats
    } else {
        epitrax$internal_reports[[r_name]] <- grouped_stats
    }

    epitrax
}


#' Write reports from EpiTrax object to CSV files
#'
#' `epitrax_write_csvs` writes the internal and public reports from an EpiTrax
#' object to CSV files in the specified filesystem. Doesn't write files if
#' the EpiTrax config setting `generate_csvs` is set to false.
#'
#' @param epitrax Object of class `epitrax`.
#' @param fsys Filesystem list containing paths for internal and public reports.
#'
#' @returns The original EpiTrax object, unchanged.
#' @export
#'
#' @examples
#' fsys <- list(
#'   internal = file.path(tempdir(), "internal_reports"),
#'   public = file.path(tempdir(), "public_reports"),
#'   settings = file.path(tempdir(), "report_settings")
#' )
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_preport_ytd_rates() |>
#'  epitrax_write_csvs(fsys = fsys)
epitrax_write_csvs <- function(epitrax, fsys) {

    validate_epitrax(epitrax)
    validate_filesystem(fsys)

    # Verify config allows CSV generation
    if (epitrax$config$generate_csvs) {
        # Write internal reports to CSV
        for (name in names(epitrax$internal_reports)) {
            write_report_csv(
                epitrax$internal_reports[[name]],
                paste0(name, ".csv"),
                fsys$internal
            )
        }

        # Write public reports to CSV
        for (name in names(epitrax$public_reports)) {
            write_report_csv(
                epitrax$public_reports[[name]],
                paste0(name, ".csv"),
                fsys$public
            )
        }
    }

    epitrax
}


#' Write reports from EpiTrax object to Excel files
#'
#' `epitrax_write_xlsxs` writes the internal and public reports from an EpiTrax
#' object to Excel files in the specified filesystem. Combines all internal reports
#' into one Excel file with separate sheets for each report. Likewise with public
#' reports.
#'
#' @param epitrax Object of class `epitrax`.
#' @param fsys Filesystem list containing paths for internal and public reports.
#'
#' @returns The original EpiTrax object, unchanged.
#' @export
#'
#' @examples
#' fsys <- list(
#'   internal = file.path(tempdir(), "internal_reports"),
#'   public = file.path(tempdir(), "public_reports"),
#'   settings = file.path(tempdir(), "report_settings")
#' )
#' fsys <- setup_filesystem(fsys)
#'
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = "use_defaults",
#'   public = "use_defaults"
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' ) |>
#'  epitrax_preport_ytd_rates() |>
#'  epitrax_write_xlsxs(fsys = fsys)
epitrax_write_xlsxs <- function(epitrax, fsys) {

    validate_epitrax(epitrax)
    validate_filesystem(fsys)

    # Write internal reports to Excel
    if (length(epitrax$internal_reports) > 0) {
        write_report_xlsx(
            data = epitrax$internal_reports,
            filename = "internal_reports_combined.xlsx",
            folder = fsys$internal
        )
    }

    # Write public reports to Excel
    if (length(epitrax$public_reports) > 0) {
        write_report_xlsx(
            data = epitrax$public_reports,
            filename = "public_reports_combined.xlsx",
            folder = fsys$public
        )
    }

    epitrax
}


#' Create formatted PDF report of monthly cross-section reports
#'
#' `epitrax_write_pdf_public_reports` writes a PDF report for each public
#' report, excluding grouped stats reports (which are handled by
#' `epitrax_write_pdf_grouped_stats`). The PDF uses pretty formatting and adds
#' a header and footer.
#'
#' @param epitrax Object of class `epitrax`.
#' @param fsys Filesystem list containing path for public reports.
#' @param trend.only Logical. Whether to show only trend in the PDF report.
#'
#' @returns The original EpiTrax object, unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#'  fsys <- list(
#'    internal = file.path(tempdir(), "internal_reports"),
#'    public = file.path(tempdir(), "public_reports"),
#'    settings = file.path(tempdir(), "report_settings")
#'  )
#'  fsys <- setup_filesystem(fsys)
#'
#'  data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                           package = "epitraxr")
#'  config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                             package = "epitraxr")
#'  disease_lists <- list(
#'    internal = "use_defaults",
#'    public = "use_defaults"
#'  )
#'
#'  epitrax <- setup_epitrax(
#'    epitrax_file = data_file,
#'    config_file = config_file,
#'    disease_list_files = disease_lists
#'  ) |>
#'   epitrax_preport_month_crosssections(month_offsets = 0) |>
#'   epitrax_write_pdf_public_reports(fsys = fsys)
#' }
epitrax_write_pdf_public_reports <- function(epitrax, fsys, trend.only = FALSE) {

    validate_epitrax(epitrax)
    validate_filesystem(fsys)

    for (name in names(epitrax$public_reports)) {
        # Skip grouped stats reports
        if (grepl("^grouped_stats_", name)) {
            next
        }

        report <- epitrax$public_reports[[name]]

        params <- list()
        params$title <- paste("Report", name)
        params$report_year <- epitrax$report_year
        params$report_month <- epitrax$report_month
        params$trend_threshold <- epitrax$config$trend_threshold

        write_report_pdf(
            data = report,
            params = params,
            filename = paste0(name, ".pdf"),
            folder = fsys$public,
            trend.only = trend.only
        )

    }

    epitrax
}


#' Write grouped statistics reports from EpiTrax object to PDF files
#'
#' `epitrax_write_pdf_grouped_stats` writes the grouped statistics reports from
#' an EpiTrax object to PDF files using a formatted template. It processes both
#' internal and public grouped statistics reports.
#'
#' @param epitrax Object of class `epitrax`.
#' @param params List. Report parameters containing:
#'   - title: Report title (defaults to "Grouped Report")
#' @param fsys Filesystem list containing paths for internal and public reports.
#' @param trend.only Logical. Whether to show only trend in the PDF report.
#'
#' @returns The original EpiTrax object, unchanged.
#' @export
#'
#' @examples
#' # Don't run PDF examples in case missing LaTeX
#' \dontrun{
#'  fsys <- list(
#'    internal = file.path(tempdir(), "internal_reports"),
#'    public = file.path(tempdir(), "public_reports"),
#'    settings = file.path(tempdir(), "report_settings")
#'  )
#'  fsys <- setup_filesystem(fsys)
#'
#'  data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                           package = "epitraxr")
#'  config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                             package = "epitraxr")
#'  disease_lists <- list(
#'    internal = "use_defaults",
#'    public = "use_defaults"
#'  )
#'
#'  params <- list(
#'    title = "Monthly Grouped Disease Statistics"
#'  )
#'
#'  epitrax <- setup_epitrax(
#'    epitrax_file = data_file,
#'    config_file = config_file,
#'    disease_list_files = disease_lists
#'  ) |>
#'   epitrax_report_grouped_stats() |>
#'   epitrax_write_pdf_grouped_stats(params = params, fsys = fsys)
#' }
epitrax_write_pdf_grouped_stats <- function(epitrax, params, fsys, trend.only = FALSE) {

    validate_epitrax(epitrax)
    validate_filesystem(fsys)

    params$report_year <- epitrax$report_year
    params$report_month <- epitrax$report_month
    params$trend_threshold <- epitrax$config$trend_threshold

    # Write internal grouped stats reports to PDF
    for (name in names(epitrax$internal_reports)) {
        if (!grepl("^grouped_stats_", name)) {
            next
        }

        report <- epitrax$internal_reports[[name]]

        write_report_pdf_grouped(
            data = report,
            params = params,
            filename = paste0(name, ".pdf"),
            folder = fsys$internal,
            trend.only = trend.only
        )
    }

    # Write public grouped stats reports to PDF
    for (name in names(epitrax$public_reports)) {
        if (!grepl("^grouped_stats_", name)) {
            next
        }

        report <- epitrax$public_reports[[name]]

        write_report_pdf_grouped(
            data = report,
            params = params,
            filename = paste0(name, ".pdf"),
            folder = fsys$public,
            trend.only = trend.only
        )
    }

    epitrax
}
