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
#' `epitrax_add_report_diseases` reads internal and public disease lists and adds
#' them to the EpiTrax object.
#'
#' @param epitrax Object of class `epitrax`.
#' @param disease_list_files List containing filepaths to internal and public disease
#'   lists.
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
