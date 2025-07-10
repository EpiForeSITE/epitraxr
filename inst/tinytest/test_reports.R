# Test create_public_report_month() --------------------------------------------

tmp_dir <- tempdir()
cases <- data.frame(
  disease = c("A","B"),
  year = 2024,
  month = 1,
  counts = c(10,20)
)
avgs <- data.frame(disease = c("A", "B"), Jan = c(5, 15))
d_list <- data.frame(
  EpiTrax_name = c("A","B"),
  Public_name = c("Alpha","Beta")
)
config <- list(
  current_population = 100000,
  avg_5yr_population = 100000,
  rounding_decimals = 1
)

# Test with valid input
result <- create_public_report_month(cases, avgs, d_list, 1, 2024, config, tmp_dir)

# Check output structure
expect_true(is.list(result))
expect_true(all(c("name", "report") %in% names(result)))
expect_true(is.data.frame(result$report))

# Check report content
report <- result$report
expect_true(all(c("Alpha", "Beta") %in% report$Disease))
expect_true(all(c("Rate_per_100k", "Avg_5yr_Rate", "Trend") %in% colnames(report)))

# Check correct rates and trends
expect_equal(report[report$Disease == "Alpha", "Rate_per_100k"], 10)
expect_equal(report[report$Disease == "Alpha", "Avg_5yr_Rate"], 5)
expect_equal(report[report$Disease == "Alpha", "Trend"], get_trend(10, 5))

# Check file written
csv_file <- file.path(tmp_dir, paste0(result$name, ".csv"))
expect_true(file.exists(csv_file))

# Clean up
unlink(csv_file)


# Test create_public_report_ytd() ---------------------------------------------

tmp_dir <- tempdir()
ytd_rates <- data.frame(
  disease = c("A", "B"),
  Current_YTD_Rate_per_100k = c(12, 34),
  Avg_5yr_YTD_Rate_per_100k = c(10, 30)
)
d_list <- data.frame(
  EpiTrax_name = c("A", "B"),
  Public_name = c("Alpha", "Beta")
)

# Test with valid input
ytd_result <- create_public_report_ytd(ytd_rates, d_list, tmp_dir)

# Check output structure
expect_true(is.list(ytd_result))
expect_true(all(c("name", "report") %in% names(ytd_result)))
expect_true(is.data.frame(ytd_result$report))

# Check report content
ytd_report <- ytd_result$report
expect_true(all(c("Alpha", "Beta") %in% ytd_report$Disease))
expect_true(all(c("YTD_Rate_per_100k", "Avg_5yr_Rate", "Trend") %in% colnames(ytd_report)))

# Check correct rates and trends
expect_equal(ytd_report[ytd_report$Disease == "Alpha", "YTD_Rate_per_100k"], 12)
expect_equal(ytd_report[ytd_report$Disease == "Alpha", "Avg_5yr_Rate"], 10)
expect_equal(ytd_report[ytd_report$Disease == "Alpha", "Trend"], get_trend(12, 10))

# Check file written
ytd_csv_file <- file.path(tmp_dir, paste0(ytd_result$name, ".csv"))
expect_true(file.exists(ytd_csv_file))

# Clean up
unlink(ytd_csv_file)


