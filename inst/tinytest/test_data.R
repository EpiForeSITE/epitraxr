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


# Test format_week_num() -------------------------------------------------------

# Test with data simulating 7 years (old years should be filtered out)
input <- data.frame(
  patient_mmwr_year = c(2017L, 2018L, 2019L, 2020L, 2021L, 2022L, 2023L),
  patient_mmwr_week = rep(1, 7),
  patient_disease = rep("A", 7)
)

expected_cols <- c("disease", "month", "year", "counts")

res <- format_week_num(input)

expect_equal(colnames(res), expected_cols)
expect_true(all(res$counts == 1))
expect_equal(nrow(res), 6) # 7 years - 1 year filtered out
expect_true(all(res$year >= max(input$patient_mmwr_year) - 5))


# Test read_epitrax_data() -----------------------------------------------------

# Test with valid file
test_file <- "test_files/data/test_epitrax_data.csv"
res <- read_epitrax_data(test_file)

expect_true(is.data.frame(res))
expect_equal(nrow(res), 24683)
expect_equal(res$year[1], 2020)

# Test with non-existent file
expect_error(read_epitrax_data("/tmp/this_file_does_not_exist.csv"),
             "Please select an EpiTrax data file \\(.csv\\)\\.")

# Test with wrong file extension
wrong_file <- tempfile(fileext = ".txt")
file.create(wrong_file)
expect_error(read_epitrax_data(wrong_file),
             "Please select an EpiTrax data file \\(.csv\\)\\.")
unlink(wrong_file)


# Test get_epitrax() ---------------------------------------------------------

# Test with valid data file
test_file <- "test_files/data/test_epitrax_data.csv"
expect_silent(epitrax <- get_epitrax(test_file))

# Check object class
expect_true(inherits(epitrax, "epitrax"))

# Check structure
expected_names <- c("data", "diseases", "yrs", "report_year",
                   "report_month", "internal_reports", "public_reports")
expect_true(all(expected_names %in% names(epitrax)))

# Check data component
expect_true(is.data.frame(epitrax$data))
expect_equal(colnames(epitrax$data),
            c("disease", "month", "year", "counts"))

# Check computed values
expect_equal(epitrax$diseases, unique(epitrax$data$disease))
expect_equal(epitrax$yrs, get_yrs(epitrax$data))
expect_equal(epitrax$report_year, max(epitrax$data$year))
expect_equal(epitrax$report_month,
            max(epitrax$data[epitrax$data$year == epitrax$report_year,]$month))

# Check report lists are empty
expect_equal(length(epitrax$internal_reports), 0)
expect_equal(length(epitrax$public_reports), 0)

# Test with invalid file
expect_error(get_epitrax("nonexistent.csv"),
            "Please select an EpiTrax data file")


# Test reshape_monthly_wide() --------------------------------------------------

df <- data.frame(
  disease = c("A", "A", "B", "B"),
  month = c(1, 2, 1, 2),
  counts = c(5, 6, 7, 8)
)

reshaped <- reshape_monthly_wide(df)
expected_cols <- c("disease", "Jan", "Feb")
expect_equal(colnames(reshaped), expected_cols)
expect_equal(nrow(reshaped), 2)
expect_equal(reshaped[reshaped$disease == "A", "Jan"], 5)
expect_equal(reshaped[reshaped$disease == "B", "Feb"], 8)


# Test with missing month for a disease (should fill with 0)
df2 <- data.frame(
  disease = c("A", "B"),
  month = c(1, 2),
  counts = c(10, 20)
)
reshaped2 <- reshape_monthly_wide(df2)
expect_equal(reshaped2[reshaped2$disease == "A", "Feb"], 0)
expect_equal(reshaped2[reshaped2$disease == "B", "Jan"], 0)


# Test reshape_annual_wide() ---------------------------------------------------

df <- data.frame(
  disease = c("A", "A", "B", "C"),
  year = c(2020, 2021, 2020, 2021),
  counts = c(5, 7, 8, 0)
)
result <- reshape_annual_wide(df)

expected_df <- data.frame(
  disease = c("A", "B", "C"),
  `2020` = c(5, 8, 0),
  `2021` = c(7, 0, 0),
  check.names = FALSE
)

expect_true(is.data.frame(result))
expect_equal(result$disease, expected_df$disease)
expect_equal(result$`2020`, expected_df$`2020`)
expect_equal(result$`2021`, expected_df$`2021`)


# Test prep_report_data() ------------------------------------------------------

df <- data.frame(disease=c("A","B","D"), Jan=c(5,7,8), Feb=c(6,8,9))
report_diseases <- c("A","C")

res <- prep_report_data(df, report_diseases)

# Check that only diseases in report_diseases are present
expect_equal(report_diseases, res$disease)
# Check that disease "C" is added with 0s for Jan and Feb
expect_true(all(res[res$disease == "C", c("Jan", "Feb")] == 0))
expect_equal(res[res$disease == "A", "Jan"], 5)
