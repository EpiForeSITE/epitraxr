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
#'
#' @returns List containing the report name and data.
#' @export
#'
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
#' create_public_report_month(cases, avgs, d_list, 1, 2024, config)
create_public_report_month <- function(cases, avgs, d_list, m, y, config) {

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
  m_report <- stats::aggregate(m_report[ , -1],
                               by = list(Disease = m_report$Disease),
                               "sum")

  # - Add Trends column last
  m_report$Trend <- get_trend(m_report$Rate_per_100k, m_report$Avg_5yr_Rate)

  # - Name and return report
  r_name <- paste0("public_report_", month_name, y)
  list(name = r_name, report = m_report)
}


#' Create a YTD public report
#'
#' 'create_public_report_ytd' creates a public report for YTD rates.
#'
#' @param ytd_rates Dataframe. YTD case rates per 100k. Must have columns:
#' disease, Current_YTD_Rate_per_100k, Avg_5yr_YTD_Rate_per_100k.
#' @param d_list Dataframe. List of diseases to use for the report. Must have
#' columns: EpiTrax_name, Public_name.
#' @param config List. Settings to use for report.
#'
#' @returns List containing the report name and data.
#' @export
#'
#' @importFrom stats aggregate
#'
#' @examples
#' ytd_rates <- data.frame(
#'   disease = c("A","B"),
#'   Current_YTD_Rate_per_100k = c(12, 34),
#'   Avg_5yr_YTD_Rate_per_100k = c(10, 30)
#' )
#' d_list <- data.frame(
#'   EpiTrax_name = c("A","B"),
#'   Public_name = c("Alpha","Beta")
#' )
#' config <- list(generate_csvs = TRUE)
#' create_public_report_ytd(ytd_rates, d_list, config)
create_public_report_ytd <- function(ytd_rates, d_list, config) {

  # - Create the report data frame initializing the Rate_per_100k column to 0
  m_report <- data.frame(
    Disease = ytd_rates$disease,
    YTD_Rate_per_100k = ytd_rates$Current_YTD_Rate_per_100k,
    Avg_5yr_Rate = ytd_rates$Avg_5yr_YTD_Rate_per_100k
  )

  # - Convert disease names to public-facing versions
  m_report <- merge(m_report, d_list, by.x = "Disease", by.y = "EpiTrax_name")
  m_report$Disease <- m_report$Public_name
  m_report$Public_name <- NULL
  m_report <- m_report[order(m_report$Disease),]

  # - Combine diseases with same public name (if any)
  m_report <- stats::aggregate(m_report[ , -1],
                               by = list(Disease = m_report$Disease),
                               "sum")

  # - Add Trends column last
  m_report$Trend <- get_trend(m_report$YTD_Rate_per_100k, m_report$Avg_5yr_Rate)

  # - Name and return report
  r_name <- "public_report_YTD"
  list(name = r_name, report = m_report)
}


#' Create annual counts report
#'
#' 'create_report_annual_counts' generates a data frame of annual case
#' counts for each disease, with years as columns.
#'
#' @param data Dataframe. Input data with columns: disease, year, counts.
#' @param disease_names Character vector. List of diseases to include in the
#' report.
#'
#' @returns Dataframe of annual counts with one row per disease and one column
#' per year.
#' @export
#'
#' @importFrom stats aggregate
#'
#' @examples
#' data <- data.frame(
#'   disease = c("A", "A", "B"),
#'   year = c(2020, 2021, 2020),
#'   counts = c(5, 7, 8)
#' )
#' create_report_annual_counts(data, disease_names = c("A", "B", "C"))
create_report_annual_counts <- function(data, disease_names) {
  # - Aggregate annual counts by disease and year
  annual_counts <- stats::aggregate(counts ~ disease + year,
                            data = data,
                            FUN = sum)

  # - Reshape data to use years as columns and diseases as rows
  annual_counts <- reshape_annual_wide(annual_counts)

  # - Add missing diseases
  annual_counts <- prep_report_data(annual_counts, disease_names)

  # - Clear row names
  rownames(annual_counts) <- NULL

  annual_counts
}


#' Create monthly counts report
#'
#' 'create_report_monthly_counts' generates a data frame of monthly case
#' counts for each disease for a specific year, with months as columns.
#'
#' @param data Dataframe. Input data with columns: disease, year, month, counts.
#' @param y Integer. The year to generate the report for.
#' @param disease_names Character vector. List of diseases to include in the
#' report.
#'
#' @returns Dataframe of monthly counts with one row per disease and one column
#' per month (Jan through Dec).
#' @export
#'
#' @examples
#' data <- data.frame(
#'   disease = c("A", "A", "B", "B"),
#'   year = c(2024, 2024, 2024, 2023),
#'   month = c(1, 2, 1, 4),
#'   counts = c(5, 7, 8, 9)
#' )
#' create_report_monthly_counts(data, 2024, disease_names = c("A", "B", "C"))
create_report_monthly_counts <- function(data, y, disease_names) {
  # - Get monthly counts by disease, year, and month
  month_counts <- get_month_counts(data)

  # - Extract counts for given year
  month_counts <- month_counts[month_counts$year == y, ]

  # - Remove year column (don't want to include in report)
  # TODO: could just not return this column at the end of the function
  month_counts$year <- NULL

  # - Reshape data to use months as columns and diseases as rows
  month_counts <- reshape_monthly_wide(month_counts)

  # - Add missing diseases
  month_counts <- prep_report_data(month_counts, disease_names)

  # - Clear row names
  rownames(month_counts) <- NULL

  month_counts
}


#' Create monthly averages report
#'
#' 'create_report_monthly_avgs' generates a data frame of average monthly case
#' counts for each disease across all years in the input data.
#'
#' @param data Dataframe. Input data with columns: disease, year, month, counts.
#' @param disease_names Character vector. List of diseases to include in the
#' report.
#' @param config List. Settings to use for report.
#'
#' @returns Dataframe of monthly averages with one row per disease and one column
#' per month (Jan through Dec).
#' @export
#'
#' @importFrom stats aggregate
#'
#' @examples
#' data <- data.frame(
#'   disease = c("A", "A", "B", "B"),
#'   year = c(2023, 2024, 2023, 2024),
#'   month = c(1, 1, 2, 2),
#'   counts = c(10, 20, 15, 25)
#' )
#' config <- list(rounding_decimals = 1)
#' create_report_monthly_avgs(data, c("A", "B", "C"), config)
create_report_monthly_avgs <- function(data, disease_names, config) {
  # - Compute average counts for each month
  monthly_avgs <- stats::aggregate(counts ~ disease + month,
                                   data = data,
                                   FUN = sum)

  num_yrs <- length(get_yrs(data))

  monthly_avgs$counts <- round(monthly_avgs$counts / num_yrs,
                               digits = config$rounding_decimals)

  # - Reshape data to use months as columns and disease as rows
  monthly_avgs <- reshape_monthly_wide(monthly_avgs)

  # - Add missing diseases
  monthly_avgs <- prep_report_data(monthly_avgs, disease_names)

  # - Clear row names
  rownames(monthly_avgs) <- NULL

  monthly_avgs
}


create_report_monthly_medians <- function(data, disease_names, config) {
  # - Compute counts for each month
  monthly_meds <- stats::aggregate(counts ~ disease + month + year,
                                   data = data,
                                   FUN = sum)

  # TODO: median incorrect when there are no counts for a disease in a month

  # - Compute median counts for each month
  monthly_meds <- stats::aggregate(counts ~ disease + month,
                                   data = monthly_meds,
                                   FUN = median)

  # - Reshape data to use months as columns and disease as rows
  monthly_meds <- reshape_monthly_wide(monthly_meds)

  # - Add missing diseases
  monthly_meds <- prep_report_data(monthly_meds, disease_names)

  # - Clear row names
  rownames(monthly_meds) <- NULL

  monthly_meds
}


#' Create year-to-date (YTD) counts report
#'
#' 'create_report_ytd_counts' generates a data frame of year-to-date counts
#' for each disease up to the given month, comparing the given year to the
#' average of other years.
#'
#' @param data Dataframe. Input data with columns: disease, year, month, counts.
#' @param disease_names Character vector. List of diseases to include in the
#' report.
#' @param y Integer. Current report year.
#' @param m Integer. Current report month (1-12).
#' @param config List. Configuration with current_population, avg_5yr_population,
#' and rounding_decimals settings.
#' @param as.rates Logical. If TRUE, returns rates per 100k instead of raw counts.
#'
#' @returns Dataframe with one row per disease and columns for current YTD and
#' average YTD values (either counts or rates per 100k).
#' @export
#'
#' @importFrom stats aggregate
#'
#' @examples
#' data <- data.frame(
#'   disease = c("A", "A", "B", "B"),
#'   year = c(2024, 2023, 2024, 2023),
#'   month = c(1, 1, 2, 2),
#'   counts = c(10, 20, 15, 25)
#' )
#' config <- list(
#'   current_population = 100000,
#'   avg_5yr_population = 100000,
#'   rounding_decimals = 1
#' )
#' create_report_ytd_counts(data, c("A", "B", "C"), 2024, 2, config)
create_report_ytd_counts <- function(data, disease_names, y, m, config, as.rates = FALSE) {

  # - Aggregate monthly counts by disease, year, and month
  month_counts <- get_month_counts(data)

  num_prev_yrs <- length(unique(data[data$year != y,]$year))

  # Compute current year-to-date (YTD) counts
  current_ytd <- month_counts[month_counts$year == y, ]
  current_ytd <- stats::aggregate(counts ~ disease, data = current_ytd, FUN = sum)
  current_ytd <- prep_report_data(current_ytd, disease_names)

  # Compute average YTD counts for the previous years
  avg_5yr_ytd <- with(month_counts, month_counts[year != y & month <= m, ])
  avg_5yr_ytd <- stats::aggregate(counts ~ disease, data = avg_5yr_ytd, FUN = sum)
  avg_5yr_ytd$counts <- round(avg_5yr_ytd$counts / num_prev_yrs,
                              config$rounding_decimals)
  avg_5yr_ytd <- prep_report_data(avg_5yr_ytd, disease_names)

  # Assemble YTD report
  ytd_report <- data.frame(disease = current_ytd$disease)

  if (as.rates) {
    # Convert counts to rates per 100k
    ytd_report <- cbind(
      ytd_report,
      data.frame(
        Current_YTD_Rate_per_100k = convert_counts_to_rate(
          current_ytd$counts,
          pop = config$current_population,
          digits = config$rounding_decimals),
        Avg_5yr_YTD_Rate_per_100k = convert_counts_to_rate(
          avg_5yr_ytd$counts,
          pop = config$avg_5yr_population,
          digits = config$rounding_decimals)
    ))
  } else {
    # Use raw counts
    ytd_report <- cbind(
      ytd_report,
      data.frame(
        Current_YTD_Counts = current_ytd$counts,
        Avg_5yr_YTD_Counts = avg_5yr_ytd$counts
    ))
  }

  ytd_report
}


#' @export
create_report_grouped_stats <- function(data, diseases, y, m, config) {

  disease_names <- diseases$EpiTrax_name
  disease_groups <- diseases$Group_name

  month_abb <- month.abb[m]
  month_name <- month.name[m]

  grouped_r <- create_report_monthly_counts(data, y, disease_names)
  grouped_r <- grouped_r[, c("disease", month_abb)]
  colnames(grouped_r) <- c("disease", "m_counts")

  grouped_r$m_rates <- convert_counts_to_rate(
    counts = grouped_r$m_counts,
    pop = config$current_population,
    digits = config$rounding_decimals
  )

  m_hist_avg_count <- create_report_monthly_avgs(
    data = data[data$year != y,],
    disease_names = disease_names,
    config = config
  )
  m_hist_avg_count <- m_hist_avg_count[, c("disease", month_abb)]
  colnames(m_hist_avg_count) <- c("disease", "counts")
  grouped_r$m_hist_avg_count <- m_hist_avg_count$counts

  m_hist_median_count <- create_report_monthly_medians(
    data = data[data$year != y,],
    disease_names = disease_names,
    config = config
  )
  m_hist_median_count <- m_hist_median_count[, c("disease", month_abb)]
  colnames(m_hist_median_count) <- c("disease", "counts")
  grouped_r$m_hist_median_count <- m_hist_median_count$counts

  # TODO: use cbind()

  y_ytd_stats <- create_report_ytd_counts(
    data = data,
    disease_names = disease_names,
    y = y,
    m = m,
    config = config,
    as.rates = FALSE
  )
  colnames(y_ytd_stats) <- c("disease", "y_YTD_count", "hist_y_ytd_avg_count")
  grouped_r$y_YTD_count <- y_ytd_stats$y_YTD_count
  grouped_r$hist_y_ytd_avg_count <- y_ytd_stats$hist_y_ytd_avg_count

  # TODO: Compute hist_y_ytd_median_count

  # TODO: update get_trend to take a % modifier, default to ±15%
  grouped_r$y_ytd_trend <- get_trend(
    col1 = grouped_r$y_YTD_count,
    col2 = grouped_r$hist_y_ytd_avg_count
  )

  grouped_r <- cbind(disease_groups, grouped_r)

  # Update column names
  new_colnames <- c(
    "Group",
    "Disease",
    paste(month_name, y),
    paste(month_name, y, "Rate"),
    paste("Historical", month_name, "Avg"),
    paste("Historical", month_name, "Median"),
    paste(y, "YTD"),
    paste("Historical", y, "YTD Avg"),
    "YTD Trend"
  )

  colnames(grouped_r) <- new_colnames

  grouped_r
}
