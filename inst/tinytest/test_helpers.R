# Test epitraxr_config() -------------------------------------------------------
expected_config <- list(
  current_population = 56000,
  avg_5yr_population = 57000,
  rounding_decimals = 3,
  generate_csvs = FALSE,
  trend_threshold = 0.2
)

default_config <- list(
  current_population = 100000,
  avg_5yr_population = 100000,
  rounding_decimals = 2,
  generate_csvs = TRUE,
  trend_threshold = 0.15
)

expect_equal(epitraxr_config(
  current_population = 56000,
  avg_5yr_population = 57000,
  rounding_decimals = 3,
  generate_csvs = FALSE,
  trend_threshold = 0.2
), expected_config)
expect_equal(epitraxr_config(), default_config)
expect_equal(do.call(epitraxr_config, list()), default_config)
expect_warning(result_config <- epitraxr_config(
  current_population = "not numeric",
  avg_5yr_population = "not numeric",
  rounding_decimals = "not numeric",
  generate_csvs = "not logical",
  trend_threshold = "not numeric"
), "config fields are missing/invalid")
expect_equal(result_config, default_config)


# Test convert_counts_to_rate() ------------------------------------------------

# Basic conversion
expect_equal(convert_counts_to_rate(50, 200000, 2), 25.00)
# Vectorized input (and non-default rate_adj_pop)
expect_equal(convert_counts_to_rate(c(10, 20), 100000, 1, 10000), c(1, 2))
# Zero counts
expect_equal(convert_counts_to_rate(0, 100000, 2), 0)
# Rounding
expect_equal(convert_counts_to_rate(7, 30000, 3), 23.333)


# Test compute_trend() ---------------------------------------------------------

# Up, down, same
expect_equal(compute_trend(c(5, 10, 10), c(3, 10, 12)),
             c("Elevated", "Expected", "Less Than Expected"))
# Vector recycling
expect_equal(compute_trend(1, c(0, 2)), c("Elevated", "Less Than Expected"))

# Test threshold
expect_equal(compute_trend(c(5, 10, 11, 10), c(3, 10, 10, 12), threshold = 0.15),
             c("Elevated", "Expected", "Expected", "Less Than Expected"))
# Test threshold boundaries
expect_equal(compute_trend(11.5, 10, threshold = 0.15), "Expected") # Just within threshold
expect_equal(compute_trend(11.6, 10, threshold = 0.15), "Elevated") # Just over threshold


# Test get_yrs() --------------------------------------------------------------

df <- data.frame(year = c(2020, 2021, 2020, 2022, 2021))
expect_equal(get_yrs(df), c(2020, 2021, 2022))

# Works with only one year
single <- data.frame(year = 2023)
expect_equal(get_yrs(single), 2023)


# Test set_na_0() -------------------------------------------------------------

# Test with mixed NA and non-NA values
df <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 3))
res <- set_na_0(df)
expected_res <- data.frame(a = c(1, 0, 3), b = c(0, 2, 3))
expect_equal(res, expected_res)

# Test with all NA
all_na <- data.frame(x = c(NA, NA))
expect_equal(set_na_0(all_na)$x, c(0, 0))

# Test with no NA
no_na <- data.frame(y = c(1, 2, 3))
expect_equal(set_na_0(no_na), no_na)
