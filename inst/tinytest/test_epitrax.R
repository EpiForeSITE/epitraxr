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


# Test epitrax_add_config() ----------------------------------------------------
config_file <- "test_files/configs/good_config.yaml"
epitrax <- structure(
  list(data = c(1,2,3)),
  class = "epitrax"
)
expect_silent(epitrax <- epitrax_add_config(epitrax, config_file))
expect_true(inherits(epitrax, "epitrax"))
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
expect_true(inherits(epitrax, "epitrax"))
expect_equal(epitrax$report_diseases$internal, utils::read.csv(i_file))
expect_equal(epitrax$report_diseases$public, utils::read.csv(p_file))


# Test setup_epitrax() ---------------------------------------------------------
data_file <- "test_files/data/test_epitrax_data.csv"
config_file <- "test_files/configs/good_config.yaml"
disease_lists <- list(
  internal = "test_files/disease_lists/internal_list.csv",
  public = "test_files/disease_lists/public_list.csv"
)

expect_silent(epitrax <- setup_epitrax(
  epitrax_file = data_file,
  config_file = config_file,
  disease_list_files = disease_lists
))
expect_true(inherits(epitrax, "epitrax"))
expect_equal(epitrax$data, get_epitrax(data_file)$data)
expect_equal(epitrax$config, read_report_config(config_file))
expect_equal(epitrax$report_diseases$internal, utils::read.csv(i_file))
expect_equal(epitrax$report_diseases$public, utils::read.csv(p_file))


# Test internal report functions -----------------------------------------------
data_file <- "test_files/data/test_epitrax_data.csv"
config_file <- "test_files/configs/good_config.yaml"
disease_lists <- list(
  internal = "use_defaults",
  public = "use_defaults"
)

expect_warning(epitrax <- setup_epitrax(
  epitrax_file = data_file,
  config_file = config_file,
  disease_list_files = disease_lists
))

# Test epitrax_ireport_annual_counts()
epitrax <- epitrax_ireport_annual_counts(epitrax)
expect_true(inherits(epitrax, "epitrax"))
expect_true("annual_counts" %in% names(epitrax$internal_reports))
expect_true(is.data.frame(epitrax$internal_reports$annual_counts))
expect_equal(nrow(epitrax$internal_reports$annual_counts), 5)

# Test epitrax_ireport_monthly_counts_all_yrs()
epitrax <- epitrax_ireport_monthly_counts_all_yrs(epitrax)
expect_true(inherits(epitrax, "epitrax"))
expect_true("monthly_counts_2019" %in% names(epitrax$internal_reports))
expect_true(is.data.frame(epitrax$internal_reports$monthly_counts_2019))
expect_equal(nrow(epitrax$internal_reports$monthly_counts_2019), 5)

# Test epitrax_ireport_monthly_avgs()
epitrax <- epitrax_ireport_monthly_avgs(epitrax, exclude.report.year = TRUE)
expect_true(inherits(epitrax, "epitrax"))
expect_true("monthly_avgs_2019-2023" %in% names(epitrax$internal_reports))
expect_true(is.data.frame(epitrax$internal_reports$`monthly_avgs_2019-2023`))
expect_equal(nrow(epitrax$internal_reports$`monthly_avgs_2019-2023`), 5)

# Test epitrax_ireport_ytd_counts_for_month()
epitrax <- epitrax_ireport_ytd_counts_for_month(epitrax, as.rates = TRUE)
expect_true(inherits(epitrax, "epitrax"))
expect_false("ytd_counts" %in% names(epitrax$internal_reports))
expect_true("ytd_rates" %in% names(epitrax$internal_reports))
expect_true(is.data.frame(epitrax$internal_reports$ytd_rates))
expect_equal(nrow(epitrax$internal_reports$ytd_rates), 5)
