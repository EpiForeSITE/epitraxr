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
  public = "test_public"
)))
expect_error(validate_filesystem(list(
  internal = "test_internal"
)))


# Test validate_config() -------------------------------------------------------
valid_config <- list(
  current_population = 56000,
  avg_5yr_population = 57000,
  rounding_decimals = 3,
  generate_csvs = FALSE,
  trend_threshold = 0.2
)

bad_config <- list(
  current_population = "not numeric",
  avg_5yr_population = "not numeric",
  rounding_decimals = "not numeric",
  generate_csvs = "not logical",
  trend_threshold = "not numeric"
)

default_config <- list(
  current_population = 100000,
  avg_5yr_population = 100000,
  rounding_decimals = 2,
  generate_csvs = TRUE,
  trend_threshold = 0.15
)

expect_silent(result_config <- validate_config(valid_config))
expect_equal(result_config, valid_config)

expect_warning(result_config <- validate_config(config = list()),
               "config fields are missing/invalid")
expect_equal(result_config, default_config)

expect_warning(result_config <- validate_config(config = bad_config),
               "config fields are missing/invalid")
expect_equal(result_config, default_config)


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


# Test validate_diseases() ------------------------------------------------------
valid_diseases <- data.frame(
  EpiTrax_name = c("Disease A", "Disease B"),
  Public_name = c("Public Disease A", "Public Disease B"),
  Group_name = c("Group 1", "Group 2")
)

# Test all valid
result <- validate_diseases(valid_diseases, is.public = TRUE, is.grouped = TRUE)
expect_equal(result, valid_diseases)

# Test only EpiTrax_name required
result <- validate_diseases(valid_diseases, is.public = FALSE, is.grouped = FALSE)
expect_equal(names(result), c("EpiTrax_name"))
expect_equal(result, valid_diseases[c("EpiTrax_name")])

# Test non-grouped public report
result <- validate_diseases(valid_diseases, is.public = TRUE, is.grouped = FALSE)
expect_equal(names(result), c("EpiTrax_name", "Public_name"))
expect_equal(result, valid_diseases[c("EpiTrax_name", "Public_name")])

# Test missing EpiTrax_name
missing_epitrax_diseases <- valid_diseases[c("Public_name", "Group_name")]
expect_error(
  validate_diseases(missing_epitrax_diseases, is.public = TRUE, is.grouped = TRUE),
  "'diseases' is missing the column 'EpiTrax_name'."
)

# Test missing Public_name
missing_epitrax_diseases <- valid_diseases[c("EpiTrax_name", "Group_name")]
expect_error(
  validate_diseases(missing_epitrax_diseases, is.public = TRUE, is.grouped = TRUE),
  "'diseases' is missing the column 'Public_name'."
)

# Test missing Group_name
missing_epitrax_diseases <- valid_diseases[c("EpiTrax_name", "Public_name")]
expect_error(
  validate_diseases(missing_epitrax_diseases, is.public = TRUE, is.grouped = TRUE),
  "'diseases' is missing the column 'Group_name'."
)
