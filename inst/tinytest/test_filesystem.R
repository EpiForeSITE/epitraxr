# Test create_filesystem() -----------------------------------------------------
internal_folder <- "internal_reports"
public_folder <- "public_reports"
settings_folder <- "report_settings"

create_filesystem(
  internal = internal_folder,
  public = public_folder,
  settings = settings_folder
)

expect_true(dir.exists(internal_folder))
expect_true(dir.exists(public_folder))
expect_true(dir.exists(settings_folder))

# Test clear_old_reports() -----------------------------------------------------
# Copy test files into test folders
i_report_name <- "internal_report.csv"
p_report_name <- "public_report.csv"

file.copy(file.path("test_files/reports/", i_report_name), internal_folder)
file.copy(file.path("test_files/reports/", p_report_name), public_folder)

# Test files deleted
i_file <- file.path(internal_folder, i_report_name)
p_file <- file.path(public_folder, p_report_name)

expect_silent(old_files <- clear_old_reports(internal_folder, public_folder))
expect_equal(old_files[[1]], i_file)
expect_equal(old_files[[2]], p_file)

expect_false(file.exists(i_file))
expect_false(file.exists(p_file))

# Test read_report_config() ----------------------------------------------------
# Test with valid config file
good_config_file <- "test_files/configs/good_config.yaml"

expect_silent(report_config <- read_report_config(good_config_file))
expect_equal(report_config$current_population, 56000)
expect_equal(report_config$avg_5yr_population, 57000)
expect_equal(report_config$rounding_decimals, 3)
expect_equal(report_config$generate_csvs, FALSE)


has_config_defaults <- function(config) {
  expect_equal(config$current_population, 100000)
  expect_equal(config$avg_5yr_population, 100000)
  expect_equal(config$rounding_decimals, 2)
  expect_equal(config$generate_csvs, TRUE)
}


# Test with no config file
expect_warning(report_config <- read_report_config(""),
               "No report configuration file provided.")

has_config_defaults(report_config)

# Test missing config fields
empty_config_file <- "test_files/configs/empty_config.yaml"

expect_warning(report_config <- read_report_config(empty_config_file),
               "'current_population' is missing")
expect_warning(report_config <- read_report_config(empty_config_file),
               "'avg_5yr_population' is missing")
expect_warning(report_config <- read_report_config(empty_config_file),
               "'rounding_decimals' is missing")
expect_warning(report_config <- read_report_config(empty_config_file),
               "'generate_csvs' is missing")

has_config_defaults(report_config)


# Test invalid config fields
invalid_config_file <- "test_files/configs/invalid_config.yaml"

expect_warning(report_config <- read_report_config(invalid_config_file),
               "'current_population' is missing")
expect_warning(report_config <- read_report_config(invalid_config_file),
               "'avg_5yr_population' is missing")
expect_warning(report_config <- read_report_config(invalid_config_file),
               "'rounding_decimals' is missing")
expect_warning(report_config <- read_report_config(empty_config_file),
               "'generate_csvs' is missing")

has_config_defaults(report_config)


# Test write_report_csv() ------------------------------------------------------
r_data <- data.frame(
  Disease = c("Measles", "Chickenpox"),
  Counts = c(20, 43)
)
r_file <- "report.csv"

expect_silent(write_report_csv(r_data, r_file, public_folder))

csv_fp <- file.path(public_folder, r_file)
expect_true(file.exists(csv_fp))
csv_data <- utils::read.csv(csv_fp)
expect_equal(csv_data, r_data)


# Test get_internal_disease_list() ---------------------------------------------
list_file <-"test_files/disease_lists/internal_list.csv"
default_list <- c("Measles", "Chickenpox")

# Test with valid list file
expect_silent(d_list <- get_internal_disease_list(list_file, default_list))
file_data <- utils::read.csv(list_file)
expect_equal(file_data, d_list)

# Test with no list file
expect_warning(d_list <- get_internal_disease_list("", default_list),
               "You have not provided a disease list for internal reports.")
expect_equal(sort(default_list), d_list$EpiTrax_name)

# Test with invalid list file
list_file <-"test_files/disease_lists/invalid_list.csv"
expect_error(d_list <- get_internal_disease_list(list_file, default_list),
             "missing required column")


# Test get_public_disease_list() -----------------------------------------------
list_file <-"test_files/disease_lists/public_list.csv"
default_list <- c("Measles", "Chickenpox")

# Test with valid list file
expect_silent(d_list <- get_public_disease_list(list_file, default_list))
file_data <- utils::read.csv(list_file)
expect_equal(file_data, d_list)

# Test with no list file
expect_warning(d_list <- get_public_disease_list("", default_list),
               "You have not provided a disease list for public reports.")
expect_equal(sort(default_list), d_list$EpiTrax_name)
expect_equal(sort(default_list), d_list$Public_name)

# Test with invalid list file
list_file <-"test_files/disease_lists/invalid_list.csv"
expect_error(d_list <- get_public_disease_list(list_file, default_list),
             "is incorrectly formatted")


# Cleanup after tests (NO TESTS AFTER THIS STEP) -------------------------------
unlink(internal_folder, recursive = TRUE)
unlink(public_folder, recursive = TRUE)
unlink(settings_folder, recursive = TRUE)

