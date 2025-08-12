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

    epitrax$config <- read_report_config(filepath)

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

    if (is.null(config)) {
        config <- list()
    }

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
    diseases <- get_report_disease_lists(
        internal_list_fp = disease_list_files$internal %||% "use_defaults",
        public_list_fp = disease_list_files$public %||% "use_defaults",
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
#' `epitrax_set_config_from_file`, and `epitrax_add_report_diseases`.
#'
#' @param epitrax_file Optional path to the EpiTrax data file. Data file should
#' be a CSV. If omitted, the user will be prompted to choose a file interactively.
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
setup_epitrax <- function(epitrax_file = NULL, disease_list_files = NULL, config_list = NULL, config_file = NULL) {

    if (!is.null(config_list) && !is.null(config_file)) {
        stop("'config_list' and 'config_file' may not both be specified. Please specify one or the other.")
    }

    epitrax <- get_epitrax(epitrax_file) |>
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

    # Get needed statistics
    month_counts <- get_month_counts(epitrax$data)
    r_data <- epitrax$data[epitrax$data$year != epitrax$report_year,]
    monthly_avgs <- create_report_monthly_avgs(
        data = r_data,
        disease_names = epitrax$report_diseases$public$EpiTrax_name,
        config = epitrax$config
    )

    # Create monthly cross-section reports
    for (offset in month_offsets) {
        r <- create_public_report_month(
            cases = month_counts,
            avgs = monthly_avgs,
            d_list = epitrax$report_diseases$public,
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

    # Create YTD report using public disease list
    ytd_rates <- create_report_ytd_counts(
        data = epitrax$data,
        disease_names = epitrax$report_diseases$public$EpiTrax_name,
        y = epitrax$report_year,
        m = epitrax$report_month,
        config = epitrax$config,
        as.rates = TRUE
    )

    # Create public report
    r <- create_public_report_ytd(
        ytd_rates = ytd_rates,
        d_list = epitrax$report_diseases$public,
        config = epitrax$config
    )

    # Add to public reports
    epitrax$public_reports[[r$name]] <- r$report

    epitrax
}


epitrax_monthly_medians <- function(epitrax, is.public = FALSE, exclude.report.year = FALSE) {

    validate_epitrax(epitrax)

    # Create monthly medians
    r_data <- epitrax$data
    if (exclude.report.year) {
        r_data <- r_data[r_data$year != epitrax$report_year,]
    }

    monthly_medians <- create_report_monthly_medians(
        data = r_data,
        disease_names = ifelse(
            is.public,
            epitrax$report_diseases$public$EpiTrax_name,
            epitrax$report_diseases$internal$EpiTrax_name
        )
    )

    # Add to internal reports
    r_name <- paste0("monthly_medians_", min(r_data$year), "-", max(r_data$year))
    if (is.public) {
        epitrax$public_reports[[r_name]] <- monthly_medians
    } else {
        epitrax$internal_reports[[r_name]] <- monthly_medians
    }


    epitrax
}


epitrax_report_ytd_medians <- function(epitrax, is.public = FALSE, exclude.report.year = FALSE) {

    validate_epitrax(epitrax)

    # Create YTD medians
    r_data <- epitrax$data
    if (exclude.report.year) {
        r_data <- r_data[r_data$year != epitrax$report_year,]
    }

    ytd_medians <- create_report_ytd_medians(
        data = r_data,
        disease_names = ifelse(
            is.public,
            epitrax$report_diseases$public$EpiTrax_name,
            epitrax$report_diseases$internal$EpiTrax_name
        ),
        m = epitrax$report_month
    )

    # Add to internal reports
    r_name <- paste0("ytd_medians_", min(r_data$year), "-", max(r_data$year))
    if (is.public) {
        epitrax$public_reports[[r_name]] <- ytd_medians
    } else {
        epitrax$internal_reports[[r_name]] <- ytd_medians
    }

    epitrax
}


epitrax_report_grouped_stats <- function(epitrax, is.public = FALSE) {

    validate_epitrax(epitrax)

    # Create grouped stats report
    grouped_stats <- create_report_grouped_stats(
        data = epitrax$data,
        disease_names = ifelse(
            is.public,
            epitrax$report_diseases$public$EpiTrax_name,
            epitrax$report_diseases$internal$EpiTrax_name
        ),
        y = epitrax$report_year,
        m = epitrax$report_month,
        config = epitrax$config
    )

    # Add to internal or public reports
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
    write_report_xlsx(
        data = epitrax$internal_reports,
        filename = "internal_reports_combined.xlsx",
        folder = fsys$internal
    )

    # Write public reports to Excel
    write_report_xlsx(
        data = epitrax$public_reports,
        filename = "public_reports_combined.xlsx",
        folder = fsys$public
    )

    epitrax
}


#' Create formatted PDF report of monthly cross-section reports
#'
#' `epitrax_write_pdf_month_crosssections` writes a PDF report for the monthly
#' cross-section reports. The PDF uses pretty formatting and adds a header and
#' footer.
#'
#' @param epitrax Object of class `epitrax`.
#' @param fsys Filesystem list containing paths for internal and public reports.
#'
#' @returns The original EpiTrax object, unchanged.
#' @export
#'
#' @examples
#' \dontrun{
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
#'  epitrax_preport_month_crosssections(month_offsets = 0) |>
#'  epitrax_write_pdf_month_crosssections(fsys = fsys)
#' }
epitrax_write_pdf_month_crosssections <- function(epitrax, fsys) {

  for (name in names(epitrax$public_reports)) {

    report <- epitrax$public_reports[[name]]

    colnames(report) <- c("Disease", "Rate", "Average Rate", "Trend")

    report |>
      gt::gt(
        rowname_col = "Disease",
        auto_align = FALSE
      ) |>
      gt::tab_header(
        title = "Infectious Diseases Surveillance Report",
        subtitle = paste(month.name[epitrax$report_month], epitrax$report_year)
      ) |>
      gt::tab_footnote(
        footnote = "Rate of cases per 100,000 people for the current year",
        locations = gt::cells_column_labels(columns = Rate)
      ) |>
      gt::tab_footnote(
        footnote = "5-yr average rate of cases per 100,000 people",
        locations = gt::cells_column_labels(columns = `Average Rate`)
      ) |>
      gt::tab_footnote(
        footnote = "How the current rate compares to the 5-yr average",
        locations = gt::cells_column_labels(columns = Trend)
      ) |>
      gt::opt_footnote_marks(
        marks = "standard"
      ) |>
      gt::data_color(
        method = "factor",
        columns = "Trend",
        rows = `Trend` == "Less Than Expected" | `Trend` == "Elevated",
        palette = c("green", "red")
      ) |>
      gt::opt_stylize(
        style = 1,
        add_row_striping = TRUE,
        color = "blue"
      ) |>
      gt::gtsave(
        filename = paste0(name, ".pdf"),
        path = fsys$public
      )

  }

  epitrax

}
