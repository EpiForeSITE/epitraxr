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


# Test report functions --------------------------------------------------------
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

# Test epitrax_preport_month_crosssections()
epitrax <- epitrax_preport_month_crosssections(epitrax, month_offsets = 0:1)
expect_true(inherits(epitrax, "epitrax"))
expect_true(all(c("public_report_Dec2024", "public_report_Nov2024") %in% names(epitrax$public_reports)))
expect_equal(length(epitrax$public_reports), 2)
expect_true(is.data.frame(epitrax$public_reports$public_report_Dec2024))
expect_equal(nrow(epitrax$public_reports$public_report_Dec2024), 5)

# Test epitrax_preport_ytd_rates()
epitrax <- epitrax_preport_ytd_rates(epitrax)
expect_true(inherits(epitrax, "epitrax"))
expect_true("public_report_YTD" %in% names(epitrax$public_reports))
expect_true(is.data.frame(epitrax$public_reports$public_report_YTD))
expect_equal(nrow(epitrax$public_reports$public_report_YTD), 5)

# Test epitrax_write_csvs()
# - Create folders for testing
fsys <- list(
  internal = file.path(tempdir(), "test_internal"),
  public = file.path(tempdir(), "test_public"),
  settings = file.path(tempdir(), "test_settings")
)
setup_filesystem(fsys, clear.reports = TRUE)

# - Test with config$generate_csvs == FALSE
epitrax$config$generate_csvs <- FALSE
epitrax <- epitrax_write_csvs(epitrax, fsys = fsys)

expect_true(inherits(epitrax, "epitrax"))
expect_equal(length(list.files(fsys$internal)), 0)
expect_equal(length(list.files(fsys$public)), 0)

# - Test with config$generate_csvs == TRUE
epitrax$config$generate_csvs <- TRUE
epitrax <- epitrax_write_csvs(epitrax, fsys = fsys)

expect_true(inherits(epitrax, "epitrax"))
expect_equal(length(list.files(fsys$internal)), 9)
expect_equal(length(list.files(fsys$public)), 3)

# - Check contents
annual_counts_fp <- file.path(fsys$internal, "annual_counts.csv")
public_report_YTD_fp <- file.path(fsys$public, "public_report_YTD.csv")

expect_true(file.exists(annual_counts_fp))
expect_true(file.exists(public_report_YTD_fp))

expect_equal(epitrax$internal_reports$annual_counts,
             utils::read.csv(annual_counts_fp, check.names = FALSE))
expect_equal(epitrax$public_reports$public_report_YTD,
             utils::read.csv(public_report_YTD_fp))

# Test epitrax_write_xlsxs()
# - Create folders for testing
fsys <- list(
  internal = file.path(tempdir(), "test_internal"),
  public = file.path(tempdir(), "test_public"),
  settings = file.path(tempdir(), "test_settings")
)
setup_filesystem(fsys, clear.reports = TRUE)

# - Check overall results
epitrax <- epitrax_write_xlsxs(epitrax, fsys = fsys)

expect_true(inherits(epitrax, "epitrax"))
expect_equal(length(list.files(fsys$internal)), 1)
expect_equal(length(list.files(fsys$public)), 1)

# - Check file contents
internal_xlsx <- file.path(fsys$internal, "internal_reports_combined.xlsx")
public_xlsx <- file.path(fsys$public, "public_reports_combined.xlsx")

expect_true(file.exists(internal_xlsx))
expect_true(file.exists(public_xlsx))

internal_xlsx_data <- readxl::read_excel(internal_xlsx,
                                         sheet = "annual_counts")
public_xlsx_data <- readxl::read_excel(public_xlsx,
                                       sheet = "public_report_Dec2024")

expect_equal(as.data.frame(internal_xlsx_data),
             epitrax$internal_reports$annual_counts)
expect_equal(as.data.frame(public_xlsx_data),
             epitrax$public_reports$public_report_Dec2024)
