#' Create epitraxr config object
#'
#' `epitraxr_config` creates a list of configuration options used for generating
#' reports.
#'
#' @param current_population Integer. Defaults to 100,000.
#' @param avg_5yr_population Integer. Defaults to 100,000.
#' @param rounding_decimals Integer. Defaults to 2.
#' @param generate_csvs Logical. Defaults to TRUE.
#' @param trend_threshold Numeric. Defaults to 0.15.
#'
#' @returns A named list with 'keys' corresponding to config options.
#' @export
#'
#' @examples
#' epitraxr_config(
#'   current_population = 56000,
#'   avg_5yr_population = 57000,
#'   rounding_decimals = 3,
#'   generate_csvs = FALSE,
#'   trend_threshold = 0.2
#' )
epitraxr_config <- function(
    current_population = 100000,
    avg_5yr_population = 100000,
    rounding_decimals = 2,
    generate_csvs = TRUE,
    trend_threshold = 0.15) {

  config <- list(
    current_population = current_population,
    avg_5yr_population = avg_5yr_population,
    rounding_decimals = rounding_decimals,
    generate_csvs = generate_csvs,
    trend_threshold = trend_threshold
  )

  config <- validate_config(config)

  config
}


#' Convert case counts to rate
#'
#' 'convert_counts_to_rate' converts case counts for a given population to an
#' adjusted per population of size X and rounds to the given number of digits.
#'
#' @param counts Integer(s). Case counts to convert.
#' @param pop Integer. Population size where cases were counted.
#' @param digits Integer. Number of decimals to round to.
#' @param rate_adj_pop Integer. Optional target population to use for rate.
#' Defaults to 100k for rate per 100k.
#'
#' @returns The count(s) as rates per rate_adj_pop.
#' @export
#'
#' @examples
#' convert_counts_to_rate(50, 200000, 2)
#' convert_counts_to_rate(c(10, 20), 100000, 1, 10000)
convert_counts_to_rate <- function(counts, pop, digits, rate_adj_pop = 100000) {
  round(counts / pop * rate_adj_pop, digits = digits)
}

#' Compute the report trend
#'
#' 'compute_trend' compares values of two columns and produces a new column
#' containing the trend result. The trend is represented by one of three values:
#' - "Elevated": increase from baseline
#' - "Less Than Expected": decrease from baseline
#' - "Expected": no change from baseline
#'
#' @param current List. Current data.
#' @param historical List. Historical comparison data.
#' @param threshold Numeric. Percentage threshold (as decimal) for determining
#' trend significance. Values within this percentage of the historical value
#' are considered "Expected". Defaults to 0.0 (any difference triggers trend).
#'
#' @returns Character vector containing the trend labels.
#' @export
#'
#' @examples
#' # Without threshold - any difference triggers trend
#' compute_trend(c(5, 10, 10), c(3, 10, 11))
#'
#' # With 15% threshold - small changes are "Expected"
#' compute_trend(c(5, 10, 10), c(3, 10, 11), threshold = 0.15)
compute_trend <- function(current, historical, threshold = 0.0) {
  mapply(function(x, y) {
    ifelse(x > y * (1 + threshold), "Elevated",
    ifelse(x < y * (1 - threshold), "Less Than Expected",
    "Expected"))
  }, current, historical)
}


#' Set NA values to 0
#'
#' 'set_na_0' sets NA values to 0 in a data frame.
#'
#' @param df Dataframe.
#'
#' @returns Dataframe with NA values replaced by 0.
#' @export
#'
#' @examples
#' df <- data.frame(year = c(2020, NA, 2022))
#' set_na_0(df)
set_na_0 <- function(df) {
  df[is.na(df)] <- 0
  df
}
