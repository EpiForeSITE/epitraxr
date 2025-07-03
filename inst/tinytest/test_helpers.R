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
expect_equal(get_trend(c(5, 10, 10), c(3, 12, 10)), c("\U2191", "\U2193", "\U2192"))
# Vector recycling
expect_equal(get_trend(1, c(0, 2)), c("\U2191", "\U2193"))
