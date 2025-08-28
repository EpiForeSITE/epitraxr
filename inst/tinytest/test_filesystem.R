# Test create_filesystem() -----------------------------------------------------
test_folders <- list(
  internal = file.path(tempdir(), "test_internal"),
  public = file.path(tempdir(), "test_public"),
  settings = file.path(tempdir(), "test_settings")
)

create_filesystem(
  internal = test_folders$internal,
  public = test_folders$public,
  settings = test_folders$settings
)

expect_true(dir.exists(test_folders$internal))
expect_true(dir.exists(test_folders$public))
expect_true(dir.exists(test_folders$settings))


# Test clear_old_reports() -----------------------------------------------------
# Copy test files into test folders
i_report_name <- "internal_report.csv"
p_report_name <- "public_report.csv"

file.copy(file.path("test_files/reports/", i_report_name), test_folders$internal)
file.copy(file.path("test_files/reports/", p_report_name), test_folders$public)

# Test files deleted
i_file <- file.path(test_folders$internal, i_report_name)
p_file <- file.path(test_folders$public, p_report_name)

expect_silent(old_files <- clear_old_reports(test_folders$internal, test_folders$public))
expect_equal(old_files[[1]], i_file)
expect_equal(old_files[[2]], p_file)

expect_false(file.exists(i_file))
expect_false(file.exists(p_file))


# Test setup_filesystem() ------------------------------------------------------

# Test basic setup without clearing reports
expect_silent(result <- setup_filesystem(test_folders))
expect_equal(result, test_folders)  # Should return input unchanged
expect_true(dir.exists(test_folders$internal))
expect_true(dir.exists(test_folders$public))
expect_true(dir.exists(test_folders$settings))

# Create some test files
test_file1 <- file.path(test_folders$internal, "test1.csv")
test_file2 <- file.path(test_folders$public, "test2.csv")
file.create(test_file1)
file.create(test_file2)

# Test with clear.reports = FALSE (should preserve files)
expect_silent(setup_filesystem(test_folders, clear.reports = FALSE))
expect_true(file.exists(test_file1))
expect_true(file.exists(test_file2))

# Test with clear.reports = TRUE (should remove files)
expect_silent(setup_filesystem(test_folders, clear.reports = TRUE))
expect_false(file.exists(test_file1))
expect_false(file.exists(test_file2))


# Test get_report_config() -----------------------------------------------------
# Test with valid config file
good_config_file <- "test_files/configs/good_config.yaml"

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

expect_silent(report_config <- get_report_config(good_config_file))
expect_equal(report_config, expected_config)

# Test with no config file
expect_error(report_config <- get_report_config(""),
               "No report configuration file provided.")

# Test missing config fields
empty_config_file <- "test_files/configs/empty_config.yaml"
expect_warning(report_config <- get_report_config(empty_config_file),
                "config fields are missing/invalid")
expect_equal(report_config, default_config)

# Test invalid config fields
invalid_config_file <- "test_files/configs/invalid_config.yaml"
expect_warning(report_config <- get_report_config(invalid_config_file),
               "config fields are missing/invalid")
expect_equal(report_config, default_config)


# Test writing to files --------------------------------------------------------
# Clear test directories
setup_filesystem(test_folders, clear.reports = TRUE)

# Test write_report_csv() ------------------------------------------------------
r_data <- data.frame(
  Disease = c("Measles", "Chickenpox"),
  Counts = c(20, 43)
)
r_file <- "report.csv"

expect_silent(write_report_csv(r_data, r_file, test_folders$public))

csv_fp <- file.path(test_folders$public, r_file)
expect_true(file.exists(csv_fp))
csv_data <- utils::read.csv(csv_fp)
expect_equal(csv_data, r_data)


# Test write_report_xlsx() -----------------------------------------------------
r_data <- data.frame(
  Disease = c("Measles", "Chickenpox"),
  Counts = c(20, 43)
)
r_xl <- list()
r_xl[["report"]] <- r_data
r_file <- "report.xlsx"

expect_silent(write_report_xlsx(r_data, r_file, test_folders$public))

xlsx_fp <- file.path(test_folders$public, r_file)
expect_true(file.exists(xlsx_fp))

# Read Excel file back and compare
xlsx_data <- readxl::read_excel(xlsx_fp)
expect_equal(as.data.frame(xlsx_data), r_xl$report)


# Test write_report_pdf_grouped() ----------------------------------------------
# - Don't run PDF tests if not at home (might be missing LaTeX)
if (at_home()) {
  # Create sample grouped report data
  r_data <- data.frame(
    Group = c("Respiratory", "Respiratory", "Vaccine-Preventable"),
    Disease = c("COVID", "Flu", "Measles"),
    `March 2024` = c(0, 25, 5),
    `March 2024 Rate` = c(0, 25, 5),
    `Historical March Avg` = c(0, 15, 8),
    `Historical March Median` = c(0, 15, 8),
    `2024 YTD` = c(0, 37, 9),
    `Historical 2024 YTD Avg` = c(20, 25, 14),
    `Historical 2024 YTD Median` = c(20, 25, 14),
    `YTD Trend` = get_trend(c(0, 37, 9), c(20, 25, 14)),
    check.names = FALSE
  )

  # Set report parameters
  params <- list(
    title = "Monthly Disease Surveillance Report",
    report_year = 2024,
    report_month = 3,
    trend_threshold = 0.20
  )

  # Write to temporary directory
  r_folder <- tempdir()
  r_name <- "grouped_disease_report.pdf"

  expect_silent(write_report_pdf_grouped(
    data = r_data,
    params = params,
    filename = r_name,
    folder = r_folder
  ))
  expect_true(file.exists(file.path(r_folder, r_name)))
}


# Test write_report_pdf() ------------------------------------------------------
# - Don't run PDF tests if not at home (might be missing LaTeX)
if (at_home()) {
  # Create sample grouped report data
  r_data <- data.frame(
    Disease = c("COVID", "Flu", "Measles"),
    `March 2024` = c(0, 25, 5),
    `Historical March Avg` = c(0, 15, 8),
    `Trend` = get_trend(c(0, 25, 5), c(0, 15, 8)),
    check.names = FALSE
  )

  # Set report parameters
  params <- list(
    title = "Monthly Disease Surveillance Report",
    report_year = 2024,
    report_month = 3,
    trend_threshold = 0.20
  )

  # Write to temporary directory
  r_folder <- tempdir()
  r_name <- "monthly_disease_report.pdf"

  expect_silent(write_report_pdf(
    data = r_data,
    params = params,
    filename = r_name,
    folder = r_folder
  ))
  expect_true(file.exists(file.path(r_folder, r_name)))
}


# Test get_report_diseases_internal() ------------------------------------------
list_file <- "test_files/disease_lists/internal_list.csv"
default_list <- c("Measles", "Chickenpox")

# Test with valid list file
expect_silent(d_list <- get_report_diseases_internal(list_file, default_list))
file_data <- utils::read.csv(list_file)
expect_equal(file_data, d_list)

# Test with no list file
expect_warning(d_list <- get_report_diseases_internal("", default_list),
               "You have not provided a disease list for internal reports.")
expect_equal(sort(default_list), d_list$EpiTrax_name)
# - Group_name is not provided by default
expect_true(is.null(d_list$Group_name))

# Test with invalid list file
list_file <-"test_files/disease_lists/invalid_list.csv"
expect_error(d_list <- get_report_diseases_internal(list_file, default_list),
             "missing required column")


# Test get_report_diseases_public() --------------------------------------------
list_file <-"test_files/disease_lists/public_list.csv"
default_list <- c("Measles", "Chickenpox")

# Test with valid list file
expect_silent(d_list <- get_report_diseases_public(list_file, default_list))
file_data <- utils::read.csv(list_file)
expect_equal(file_data, d_list)

# Test with no list file
expect_warning(d_list <- get_report_diseases_public("", default_list),
               "You have not provided a disease list for public reports.")
expect_equal(sort(default_list), d_list$EpiTrax_name)
expect_equal(sort(default_list), d_list$Public_name)
# - Group_name is not provided by default
expect_true(is.null(d_list$Group_name))

# Test with invalid list file
list_file <-"test_files/disease_lists/invalid_list.csv"
expect_error(d_list <- get_report_diseases_public(list_file, default_list),
             "is incorrectly formatted")


# Test get_report_diseases() ---------------------------------------------------

internal_file <- "test_files/disease_lists/internal_list.csv"
public_file <- "test_files/disease_lists/public_list.csv"
default_list <- c("Measles", "Chickenpox")

expect_silent(disease_lists <- get_report_diseases(
  internal_file, public_file, default_list
))

# Check structure
expect_true(is.list(disease_lists))
expect_true(all(c("internal", "public") %in% names(disease_lists)))

# Check contents
expect_equal(
  disease_lists$internal,
  get_report_diseases_internal(internal_file, default_list)
)
expect_equal(
  disease_lists$public,
  get_report_diseases_public(public_file, default_list)
)


# Cleanup after tests (NO TESTS AFTER THIS STEP) -------------------------------
unlink(unlist(test_folders, use.names = FALSE), recursive = TRUE)
