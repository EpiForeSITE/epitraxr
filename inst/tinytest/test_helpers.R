# Test convert_counts_to_rate() ------------------------------------------------

# Basic conversion
expect_equal(convert_counts_to_rate(50, 200000, 2), 25.00)
# Vectorized input (and non-default rate_adj_pop)
expect_equal(convert_counts_to_rate(c(10, 20), 100000, 1, 10000), c(1, 2))
# Zero counts
expect_equal(convert_counts_to_rate(0, 100000, 2), 0)
# Rounding
expect_equal(convert_counts_to_rate(7, 30000, 3), 23.333)


# Test get_trend() ------------------------------------------------------------

# Up, down, same
expect_equal(get_trend(c(5, 10, 10), c(3, 10, 12)),
             c("Elevated", "Expected", "Less Than Expected"))
# Vector recycling
expect_equal(get_trend(1, c(0, 2)), c("Elevated", "Less Than Expected"))


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
