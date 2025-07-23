# Test validate_epitrax() ---------------------------------------------------
epitrax <- structure(
  list(data = c(1,2,3)),
  class = "epitrax"
)
expect_silent(validate_epitrax(epitrax, report.check = FALSE))
expect_error(validate_epitrax(epitrax, report.check = TRUE))

epitrax$config = list(rounding_decimals = 2, generate_csvs = TRUE)
epitrax$report_diseases = list(internal = "internal_list", public = "public_list")

expect_silent(validate_epitrax(epitrax, report.check = TRUE))


# Test epitrax_add_config() ----------------------------------------------------
config_file <- system.file("tinytest/test_files/configs/good_config.yaml",
                           package = "epitraxr")
epitrax <- structure(
  list(data = c(1,2,3)),
  class = "epitrax"
)
expect_silent(epitrax <- epitrax_add_config(epitrax, config_file))
expect_equal(epitrax$config, read_report_config(config_file))
