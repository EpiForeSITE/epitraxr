# Test create_public_report_month() --------------------------------------------

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
  trend_threshold = 0.15
)

# Test with valid input
result <- create_public_report_month(cases, avgs, d_list, 1, 2024, config)

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
expect_equal(report[report$Disease == "Alpha", "Trend"],
             get_trend(10, 5, threshold = 0.15))


# Test create_public_report_ytd() ----------------------------------------------

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
  generate_csvs = TRUE,
  trend_threshold = 0.15
)

# Test with valid input
ytd_result <- create_public_report_ytd(ytd_rates, d_list, config)

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
expect_equal(ytd_report[ytd_report$Disease == "Alpha", "Trend"],
             get_trend(12, 10, threshold = 0.15))


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
expect_equivalent(result, expected_result)


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
expect_equivalent(result, expected_result)


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
expect_equivalent(result, expected_result)


# Test create_report_monthly_medians() -----------------------------------------

data <- data.frame(
  disease = c("A", "A", "A", "B", "B", "B", "C"),
  year = c(2022, 2023, 2024, 2022, 2023, 2024, 2022),
  month = c(1, 1, 1, 2, 2, 2, 1),
  counts = c(10, 20, 30, 5, 15, 25, 8)
)

disease_names <- c("A", "B", "C", "D")

result <- create_report_monthly_medians(data, disease_names)

# With the fix, medians should be calculated across all years (2022-2024)
# Disease A: Jan has [10,20,30] → median = 20
# Disease B: Feb has [5,15,25] → median = 15, Jan has [0,0,0] → median = 0  
# Disease C: Jan has [8,0,0] → median = 0, Feb has [0,0,0] → median = 0
# Disease D: all months have [0,0,0] → median = 0
expected_result <- data.frame(
  disease = disease_names,
  Jan = c(20, 0, 0, 0),
  Feb = c(0, 15, 0, 0)
)

expect_true(is.data.frame(result))
expect_equal(result[,c("disease","Jan","Feb")], expected_result)

# Test the specific bug case: missing year/disease combinations
test_data <- data.frame(
  disease = c("A", "A", "B"),  # A has 2022,2023 but not 2024; B only has 2024
  year = c(2022, 2023, 2024),
  month = c(1, 1, 1),
  counts = c(10, 20, 5)
)

bug_result <- create_report_monthly_medians(test_data, c("A", "B"))

# A: Jan should have [10,20,0] → median = 10
# B: Jan should have [0,0,5] → median = 0  
bug_expected <- data.frame(
  disease = c("A", "B"),
  Jan = c(10, 0),
  Feb = c(0, 0)
)

expect_equal(bug_result[,c("disease","Jan","Feb")], bug_expected)


# Test create_report_ytd_counts() ----------------------------------------------

data <- data.frame(
  disease = c("A", "A", "B", "B", "C"),
  year = c(2024, 2023, 2024, 2023, 2023),
  month = c(1, 1, 2, 2, 1),
  counts = c(10, 20, 15, 25, 8)
)

disease_names <- c("A", "B", "C", "D")
config <- list(
  current_population = 56000,
  avg_5yr_population = 103000,
  rounding_decimals = 1
)

# Test with raw counts
result <- create_report_ytd_counts(
  data = data,
  disease_names = disease_names,
  y = 2024,
  m = 2,
  config = config,
  as.rates = FALSE
)

expected_result <- data.frame(
  disease = disease_names,
  Current_YTD_Counts = c(10, 15, 0, 0), # 2024 counts up to month 2
  Avg_5yr_YTD_Counts = c(20, 25, 8, 0)  # 2023 counts up to month 2
)

expect_true(is.data.frame(result))
expect_equal(result, expected_result)

# Test with rates
result <- create_report_ytd_counts(
  data = data,
  disease_names = disease_names,
  y = 2024,
  m = 2,
  config = config,
  as.rates = TRUE
)

expected_result <- data.frame(
  disease = disease_names,
  Current_YTD_Rate_per_100k = c(17.9, 26.8, 0.0, 0.0),
  Avg_5yr_YTD_Rate_per_100k = c(19.4, 24.3, 7.8, 0.0) 
)

expect_true(is.data.frame(result))
expect_equal(result, expected_result)


# Test create_report_ytd_medians() ---------------------------------------------

data <- data.frame(
  disease = c("A", "A", "A", "A", "B", "B", "B", "B", "C"),
  year = c(2020, 2021, 2022, 2023, 2020, 2021, 2022, 2023, 2020),
  month = c(1, 1, 2, 1, 2, 2, 3, 2, 1),
  counts = c(10, 20, 30, 25, 5, 15, 8, 12, 7)
)

disease_names <- c("A", "B", "C", "D")

# Test with m = 2 (Jan-Feb YTD)
result <- create_report_ytd_medians(data, disease_names, 2)

expected_result <- data.frame(
  disease = disease_names,
  median_counts = c(22.5, 8.5, 0, 0)
)

expect_true(is.data.frame(result))
expect_equal(result, expected_result)

# Test with m = 1 (January only)
result_jan <- create_report_ytd_medians(data, disease_names, 1)

expected_jan <- data.frame(
  disease = disease_names,
  median_counts = c(15, 0, 0, 0)
)

expect_true(is.data.frame(result_jan))
expect_equal(result_jan, expected_jan)


# Test create_report_grouped_stats() -------------------------------------------

data <- data.frame(
  disease = c("Flu", "Flu", "Flu", "Flu", "Measles", "Measles", "Measles", "Measles", "COVID"),
  year = c(2023, 2024, 2023, 2024, 2023, 2024, 2023, 2024, 2023),
  month = c(3, 3, 1, 1, 3, 3, 2, 2, 1),
  counts = c(15, 25, 10, 12, 8, 5, 6, 4, 20)
)

diseases <- data.frame(
  EpiTrax_name = c("Flu", "Measles", "COVID"),
  Group_name = c("Respiratory", "Vaccine-Preventable", "Respiratory")
)

config <- list(
  current_population = 100000,
  avg_5yr_population = 100000,
  rounding_decimals = 1,
  trend_threshold = 0.15
)

# Test with March (month 3) 2024
result <- create_report_grouped_stats(data, diseases, 2024, 3, config)

expected_result <- data.frame(
  Group = c("Respiratory", "Respiratory", "Vaccine-Preventable"),
  Disease = c("COVID", "Flu", "Measles"),
  `March 2024` = c(0, 25, 5),
  `March 2024 Rate` = c(0, 25, 5),
  `Historical March Avg` = c(0, 15, 8),
  `Historical March Median` = c(0, 15, 8),
  `2024 YTD` = c(0, 37, 9),
  `Historical 2024 YTD Avg` = c(20, 25, 14),
  `Historical 2024 YTD Median` = c(20, 25, 14),
  `YTD Trend` = get_trend(c(0, 37, 9), c(20, 25, 14), threshold = 0.15),
  check.names = FALSE
)

expect_true(is.data.frame(result))
expect_equal(result, expected_result)

# Test with missing Group_name column (NULL case)
diseases_no_group <- data.frame(
  EpiTrax_name = c("Flu", "Measles", "COVID")
)

# Should issue a warning and set all groups to "Uncategorized"
expect_warning(
  result_no_group <- create_report_grouped_stats(data, diseases_no_group, 2024, 3, config),
  "No disease groups were provided"
)

expected_result$Group <- c("Uncategorized", "Uncategorized", "Uncategorized")

expect_true(is.data.frame(result_no_group))
expect_equal(result_no_group, expected_result)

# Test with NA values in Group_name column
diseases_with_na <- data.frame(
  EpiTrax_name = c("Flu", "Measles", "COVID"),
  Group_name = c("Respiratory", NA, "Respiratory")
)

result_with_na <- create_report_grouped_stats(data, diseases_with_na, 2024, 3, config)

expected_result$Group <- c("Respiratory", "Respiratory", "Uncategorized")

expect_true(is.data.frame(result_with_na))
expect_equal(result_with_na, expected_result)
