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
config_file <- "test_files/configs/good_config.yaml"
epitrax <- structure(
  list(data = c(1,2,3)),
  class = "epitrax"
)
expect_silent(epitrax <- epitrax_add_config(epitrax, config_file))
expect_equal(epitrax$config, read_report_config(config_file))


# Test epitrax_add_report_diseases() -------------------------------------------
i_file <- "test_files/disease_lists/internal_list.csv"
p_file <- "test_files/disease_lists/public_list.csv"

epitrax <- structure(
  list(diseases = c("Measles", "Chickenpox")),
  class = "epitrax"
)

expect_silent(epitrax <- epitrax_add_report_diseases(
  epitrax,
  disease_list_files = list(internal = i_file, public = p_file)
))

expect_equal(epitrax$report_diseases$internal, utils::read.csv(i_file))
expect_equal(epitrax$report_diseases$public, utils::read.csv(p_file))
