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

# Test read_report_config() ----------------------------------------------------
# Test with valid config file
good_config_file <- "test_files/configs/good_config.yaml"

expect_silent(report_config <- read_report_config(good_config_file))
expect_equal(report_config$current_population, 56000)
expect_equal(report_config$avg_5yr_population, 57000)
expect_equal(report_config$rounding_decimals, 3)


has_config_defaults <- function(config) {
  expect_equal(config$current_population, 100000)
  expect_equal(config$avg_5yr_population, 100000)
  expect_equal(config$rounding_decimals, 2)
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

has_config_defaults(report_config)


# Test invalid config fields
invalid_config_file <- "test_files/configs/invalid_config.yaml"

expect_warning(report_config <- read_report_config(invalid_config_file),
               "'current_population' is missing")
expect_warning(report_config <- read_report_config(invalid_config_file),
               "'avg_5yr_population' is missing")
expect_warning(report_config <- read_report_config(invalid_config_file),
               "'rounding_decimals' is missing")

has_config_defaults(report_config)
