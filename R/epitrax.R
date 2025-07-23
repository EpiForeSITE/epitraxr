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
