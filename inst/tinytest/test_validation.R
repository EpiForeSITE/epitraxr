# Test validate_epitrax() ---------------------------------------------------

expect_error(validate_epitrax(list(data = c(1,2,3))))

epitrax <- structure(
  list(data = c(1,2,3)),
  class = "epitrax"
)
expect_silent(validate_epitrax(epitrax, report.check = FALSE))
expect_error(validate_epitrax(epitrax, report.check = TRUE))

epitrax$config = list(rounding_decimals = 2, generate_csvs = TRUE)
epitrax$report_diseases = list(internal = "internal_list", public = "public_list")

expect_silent(validate_epitrax(epitrax, report.check = TRUE))


# Test validate_filesystem() ---------------------------------------------------
expect_silent(validate_filesystem(list(
  internal = "test_internal",
  public = "test_public",
  settings = "test_settings"
)))
expect_error(validate_filesystem(list(
  internal = "test_internal"
)))


# Test validate_data() ---------------------------------------------------------

expected_cols <- c("patient_mmwr_year", "patient_mmwr_week", "patient_disease")

# Test with valid data
test_data <- data.frame(
  patient_mmwr_year = c(2020L, 2021L),
  patient_mmwr_week = c(1L, 2L),
  patient_disease = c("A", "B")
)

validated <- validate_data(test_data)
expect_equal(colnames(validated), expected_cols)
expect_equal(nrow(validated), 2)

# Test with extra columns and different order
test_data <- data.frame(
  patient_mmwr_week = c(1L, 2L),
  patient_mmwr_year = c(2020L, 2021L),
  patient_disease = c("A", "B"),
  extra_column = c("extra1", "extra2")
)

validated <- validate_data(test_data)
expect_equal(colnames(validated), expected_cols)

# Test with missing columns
bad_data <- data.frame(
  patient_mmwr_year = 2020L,
  patient_disease = "A"
)
expect_error(validate_data(bad_data),
             "The EpiTrax data is missing one of the following fields")

# Test with wrong data types
bad_type <- data.frame(
  patient_mmwr_year = "2020",
  patient_mmwr_week = 1L,
  patient_disease = "A"
)
expect_error(validate_data(bad_type), "incorrect data type")

# Test with NA values
na_data <- data.frame(
  patient_mmwr_year = c(2020L, NA),
  patient_mmwr_week = c(1L, 2L),
  patient_disease = c("A", "B")
)
expect_warning(validated <- validate_data(na_data),
               "dataset contains missing or NA values")
expect_equal(nrow(validated), 1)
