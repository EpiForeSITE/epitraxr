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


#' Add report configuration to EpiTrax object
#'
#' `epitrax_add_config` reads a report configuration file and adds it to the
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
#' epitrax <- epitrax_add_config(epitrax, config_file)
epitrax_add_config <- function(epitrax, filepath) {

    validate_epitrax(epitrax, report.check = FALSE)

    epitrax$config <- read_report_config(filepath)

    epitrax
}


#' Add report diseases to EpiTrax object
#'
#' `epitrax_add_report_diseases` reads internal and public disease lists and
#' adds them to the EpiTrax object.
#'
#' @param epitrax Object of class `epitrax`.
#' @param disease_list_files List containing filepaths to internal and public
#'  disease lists.
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
epitrax_add_report_diseases <- function(epitrax, disease_list_files) {

    validate_epitrax(epitrax, report.check = FALSE)

    # Get internal and public disease lists
    diseases <- get_report_disease_lists(
        internal_list_fp = disease_list_files$internal,
        public_list_fp = disease_list_files$public,
        default_diseases = epitrax$diseases
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
#' `epitrax_add_config`, and `epitrax_add_report_diseases`.
#'
#' @param epitrax_file Optional path to the EpiTrax data file. Data file should
#' be a CSV. If this parameter is NULL, the user will be prompted to choose a
#' file interactively.
#' @param config_file Path to the report configuration file.
#' @param disease_list_files List containing filepaths to internal and public
#' report disease lists.
#'
#' @returns An EpiTrax object with configuration and report diseases set.
#' @export
#'
#' @examples
#' data_file <- system.file("sample_data/sample_epitrax_data.csv",
#'                          package = "epitraxr")
#' config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
#'                            package = "epitraxr")
#' disease_lists <- list(
#'   internal = system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                          package = "epitraxr"),
#'   public = system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                        package = "epitraxr")
#' )
#'
#' epitrax <- setup_epitrax(
#'   epitrax_file = data_file,
#'   config_file = config_file,
#'   disease_list_files = disease_lists
#' )
setup_epitrax <- function(epitrax_file, config_file, disease_list_files) {

    epitrax <- get_epitrax(epitrax_file) |>
        epitrax_add_config(config_file) |>
        epitrax_add_report_diseases(disease_list_files)

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
#'   internal = system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                          package = "epitraxr"),
#'   public = system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                        package = "epitraxr")
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
#'   internal = system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                          package = "epitraxr"),
#'   public = system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                        package = "epitraxr")
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
            y = y,
            disease_names = epitrax$report_diseases$internal$EpiTrax_name
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
#'   internal = system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                          package = "epitraxr"),
#'   public = system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                        package = "epitraxr")
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
        disease_names = epitrax$report_diseases$internal$EpiTrax_name,
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
#'   internal = system.file("tinytest/test_files/disease_lists/internal_list.csv",
#'                          package = "epitraxr"),
#'   public = system.file("tinytest/test_files/disease_lists/public_list.csv",
#'                        package = "epitraxr")
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
        disease_names = epitrax$report_diseases$internal$EpiTrax_name,
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
