#' Create a monthly cross-section public report
#'
#' 'create_public_report_month' creates a public report for the given month.
#'
#' @param cases Dataframe. Disease case counts for each month and year. Must
#' have columns: disease, year, month, counts.
#' @param avgs Dataframe. Disease case count averages for each month. Must
#' have columns: disease, Jan, Feb, ...
#' @param d_list Dataframe. List of diseases to use for the report. Must have
#' columns: EpiTrax_name, Public_name.
#' @param m Integer. The report month (1-12).
#' @param y Integer. The report year.
#' @param config List. Settings to use for report.
#' @param r_folder Filepath. Destination folder for the public report.
#'
 #' @returns List containing the report name and data.
 #' @export

 #' @importFrom stats aggregate
 #'
 #' @examples
 #' cases <- data.frame(
 #'   disease = c("A","B"),
 #'   year = 2024,
 #'   month = 1,
 #'   counts = c(10,20)
 #' )
 #' avgs <- data.frame(disease = c("A","B"), Jan = c(5,15))
 #' d_list <- data.frame(
 #'   EpiTrax_name = c("A","B"),
 #'   Public_name = c("Alpha","Beta")
 #' )
 #' config <- list(
 #'   current_population = 100000,
 #'   avg_5yr_population = 100000,
 #'   rounding_decimals = 1
 #' )
 #'
 #' create_public_report_month(cases, avgs, d_list, 1, 2024, config, tempdir())
create_public_report_month <- function(cases, avgs, d_list, m, y, config, r_folder) {

  month_name <- month.abb[[m]]

  m_counts <- with(cases, cases[year == y & month == m, c("disease", "counts")])

  # - Only take the rows with data in the final report
  m_counts <- subset(m_counts, disease %in% avgs$disease)

  # - Convert monthly average counts to rate per 100k
  m_rates <- convert_counts_to_rate(avgs[[month_name]],
                                    pop = config$avg_5yr_population,
                                    digits = config$rounding_decimals)

  # - Create the report data frame initializing the Rate_per_100k column to 0
  m_report <- data.frame(
    Disease = avgs$disease,
    Rate_per_100k = 0,
    Avg_5yr_Rate = m_rates
  )

  # - Update the Rate_per_100k column with values from m_counts
  for (i in 1:length(m_counts$disease)) {
    d <- m_counts$disease[i]
    rate <- convert_counts_to_rate(m_counts$counts[i],
                                   pop = config$current_population,
                                   digits = config$rounding_decimals)
    m_report[m_report$Disease == d, ]$Rate_per_100k <- rate
  }

  # - Convert disease names to public-facing versions
  m_report <- merge(m_report, d_list,
                    by.x = "Disease", by.y = "EpiTrax_name",
                    all.x = TRUE, all.y = FALSE)
  m_report$Disease <- m_report$Public_name
  m_report$Public_name <- NULL
  m_report <- m_report[order(m_report$Disease),]

  # - Combine diseases with same public name (if any)
  m_report <- stats::aggregate(m_report[ , -1], by = list(Disease = m_report$Disease), "sum")

  # - Add Trends column last
  m_report$Trend <- get_trend(m_report$Rate_per_100k, m_report$Avg_5yr_Rate)

  # - Write to CSV file
  r_name <- paste0("public_report_", month_name, y)
  write_report_csv(m_report, paste0(r_name, ".csv"), r_folder)

  list("name" = r_name, "report" = m_report)
}
