#' Convert case counts to rate
#'
#' 'convert_counts_to_rate' converts case counts for a given population to an
#' adjusted per population of size X and rounds to the given number of digits.
#'
#' @param counts Integer(s). Case counts to convert.
#' @param pop Integer. Population size where cases were counted.
#' @param digits Integer. Number of decimals to round to.
#' @param rate_adj_pop Integer. Optional target population to use for rate.
#' Defaults to 100k.
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

#' Get Trend column of report
#'
#' 'get_trend' compares values of two columns and produces a new column
#' containing the trend result. The trend is represented by arrows:
#' up, down, and right arrows for increase, decrease, and no change respectively.
#' Arrows are given using escape codes to generate Unicode characters.
#'
#' @param col1 List. Current data.
#' @param col2 List. Historical comparison data.
#'
#' @returns Column containing the Trend markers.
#' @export
#'
#' @examples
#' get_trend(c(5, 10, 10), c(3, 10, 12))
get_trend <- function(col1, col2) {
  mapply(function(x, y) {
    # Excape codes for up, down, and right arrows
    ifelse(x > y, "\U2191", ifelse(x < y, "\U2193", "\U2192"))
  }, col1, col2)
}
