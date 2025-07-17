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
  rounding_decimals = 1,
  generate_csvs = TRUE
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

# Check file written (when generate_csvs = TRUE)
csv_file <- file.path(tmp_dir, paste0(result$name, ".csv"))
expect_true(file.exists(csv_file))
unlink(csv_file)

# Test with generate_csvs = FALSE
config$generate_csvs <- FALSE
result2 <- create_public_report_month(cases, avgs, d_list, 1, 2024, config, tmp_dir)
csv_file2 <- file.path(tmp_dir, paste0(result2$name, ".csv"))
expect_false(file.exists(csv_file2))


# Test create_public_report_ytd() ----------------------------------------------

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
config <- list(
  generate_csvs = TRUE
)

# Test with valid input
ytd_result <- create_public_report_ytd(ytd_rates, d_list, config, tmp_dir)

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

# Check file written (when generate_csvs = TRUE)
ytd_csv_file <- file.path(tmp_dir, paste0(ytd_result$name, ".csv"))
expect_true(file.exists(ytd_csv_file))
unlink(ytd_csv_file)

# Test with generate_csvs = FALSE
config$generate_csvs <- FALSE
ytd_result2 <- create_public_report_ytd(ytd_rates, d_list, config, tmp_dir)
ytd_csv_file2 <- file.path(tmp_dir, paste0(ytd_result2$name, ".csv"))
expect_false(file.exists(ytd_csv_file2))


# Test create_report_annual_counts() -------------------------------------------

data <- data.frame(
  disease = c("A", "A", "B", "C"),
  year = c(2020, 2021, 2020, 2021),
  counts = c(5, 7, 8, 0)
)

disease_names <- c("A", "B", "C", "D")

result <- create_report_annual_counts(data, disease_names)

expected_result <- data.frame(
  disease = disease_names,
  `2020` = c(5,8,0,0),
  `2021` = c(7,0,0,0),
  check.names = FALSE
)

expect_true(is.data.frame(result))
expect_equal(result, expected_result)


# Test create_report_monthly_counts() ------------------------------------------

data <- data.frame(
  disease = c("A", "A", "B", "B", "C"),
  year = c(2024, 2024, 2024, 2024, 2024),
  month = c(1, 2, 1, 3, 1),
  counts = c(5, 7, 8, 3, 2)
)

disease_names <- c("A", "B", "C", "D")

result <- create_report_monthly_counts(data, 2024, disease_names)

expected_result <- data.frame(
  disease = disease_names,
  Jan = c(5, 8, 2, 0),
  Feb = c(7, 0, 0, 0),
  Mar = c(0, 3, 0, 0)
)

expect_true(is.data.frame(result))
expect_equal(result, expected_result)


# Test create_report_monthly_avgs() ------------------------------------------

data <- data.frame(
  disease = c("A", "A", "B", "B", "C"),
  year = c(2023, 2024, 2023, 2024, 2023),
  month = c(1, 1, 2, 2, 1),
  counts = c(10, 20, 15, 25, 8)
)

disease_names <- c("A", "B", "C", "D")
config <- list(rounding_decimals = 1)

result <- create_report_monthly_avgs(data, disease_names, config)

expected_result <- data.frame(
  disease = disease_names,
  Jan = c(15.0, 0.0, 4.0, 0.0),  # (10+20)/2, 0, 8/2, 0
  Feb = c(0.0, 20.0, 0.0, 0.0)   # 0, (15+25)/2, 0, 0
)

expect_true(is.data.frame(result))
expect_equal(result, expected_result)