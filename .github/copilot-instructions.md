# epitraxr - R Package for EpiTrax Data and Report Generation

Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

## Working Effectively

**Bootstrap and setup the development environment:**
- Install R and system dependencies:
  ```bash
  sudo apt update && sudo apt install -y r-base r-base-dev build-essential libcurl4-openssl-dev libssl-dev libxml2-dev pandoc
  ```
- Install required R package dependencies via apt (avoids network issues):
  ```bash
  sudo apt install -y r-cran-devtools r-cran-lubridate r-cran-yaml r-cran-tinytest
  ```

**Build and install the package:**
- Build and install locally: `sudo R CMD INSTALL .` -- takes 2 seconds. Set timeout to 10 seconds.
- The package installs successfully and includes sample data in `inst/sample_data/sample_epitrax_data.csv`

**Run tests:**
- Test using tinytest framework: `R -e "library(epitraxr); tinytest::test_package('epitraxr')" --no-restore` -- takes 0.5 seconds. Set timeout to 10 seconds.
- Note: Some existing tests may fail (this is a known issue in the codebase), but data and report generation tests should pass.
- Tests are located in `inst/tinytest/` and cover data validation, report generation, and filesystem operations.

**Manual validation - ALWAYS run this after making changes:**
- Validate core functionality with sample data:
  ```bash
  R -e "
  library(epitraxr)
  sample_file <- system.file('sample_data', 'sample_epitrax_data.csv', package = 'epitraxr')
  epitrax_data <- read_epitrax_data(sample_file)
  diseases <- unique(epitrax_data\$disease)
  config <- epitraxr_config(current_population = 102000, avg_5yr_population = 97000)
  monthly_counts <- create_report_monthly_counts(data = epitrax_data, y = 2024, disease_names = diseases[1:2])
  ytd_report <- create_report_ytd_counts(data = epitrax_data, disease_names = diseases[1:2], y = 2024, m = 5, config = config)
  cat('Validation successful! Monthly report has', nrow(monthly_counts), 'rows and YTD report has', nrow(ytd_report), 'rows\n')
  " --no-restore
  ```
- This validation takes 0.4 seconds. Set timeout to 5 seconds.
- Expected output should show "Validation successful!" with row counts.

## Key Development Operations

**R CMD check (for comprehensive validation):**
- `R CMD check --no-manual .` -- Note: Currently fails due to DESCRIPTION format issues, but this is a known limitation.
- The package functions correctly despite check failures.

**Pre-commit hooks:**
- Install: `pip install pre-commit && pre-commit install`
- Run: `pre-commit run --all-files` -- May encounter network connectivity issues with R environment bootstrap. This is not critical for basic development.
- The project uses `.pre-commit-config.yaml` with hooks for trailing whitespace, YAML validation, and R-specific checks.

## Repository Structure

**Core directories:**
- `R/` - Main package code (data.R, epitrax.R, filesystem.R, helpers.R, reports.R, validation.R)
- `inst/` - Package installation files including sample data and test files
- `inst/sample_data/` - Sample EpiTrax CSV data for testing and examples
- `inst/tinytest/` - Test suite using tinytest framework
- `scripts/` - Utility and example scripts showing package usage
- `man/` - Generated documentation files
- `tests/tinytest.R` - Entry point for running tests

**Key files:**
- `DESCRIPTION` - Package metadata and dependencies
- `README.Rmd` - Source for README.md (edit this, not README.md directly)
- `.pre-commit-config.yaml` - Pre-commit hook configuration

## Validation Scenarios

**Always test these scenarios after making changes:**

1. **Data loading and validation:**
   - Load sample data: `epitrax_data <- read_epitrax_data(sample_file)`
   - Validate data structure and content
   - Check that diseases list is extracted correctly

2. **Report generation workflows:**
   - Monthly counts report: `create_report_monthly_counts()`
   - Year-to-date reports: `create_report_ytd_counts()`
   - Configuration management: `epitraxr_config()`

3. **File system operations:**
   - Report directory setup: `setup_filesystem()`
   - CSV and Excel output generation
   - Configuration file reading: `read_report_config()`

## Common Development Tasks

**When modifying data processing functions:**
- Always test with the included sample data
- Validate that reports generate expected row and column counts
- Check that disease filtering works correctly

**When modifying report generation:**
- Test both monthly and YTD report generation
- Verify CSV output functionality
- Test with different configuration parameters

**When modifying file system functions:**
- Test directory creation and cleanup
- Validate configuration file parsing
- Test report writing functions

## GitHub Workflows

The project includes:
- `.github/workflows/r-check.yaml` - R CMD check across multiple platforms and R versions
- `.github/workflows/build-for-cran.yaml` - CRAN-ready package building
- `.github/workflows/pkgdown.yaml` - Documentation site generation

**Important timing notes:**
- All local development operations complete in under 10 seconds
- Package installation: 2 seconds
- Tests: 0.5 seconds  
- Manual validation: 0.4 seconds
- NEVER CANCEL these operations - they are fast and reliable

## Dependencies

**System dependencies (installed via apt):**
- r-base, r-base-dev, build-essential
- libcurl4-openssl-dev, libssl-dev, libxml2-dev, pandoc

**R package dependencies (from DESCRIPTION):**
- lubridate, stats, utils, writexl, yaml (Imports)
- readxl, tinytest, rmarkdown (Suggests)

**All dependencies available via apt packages to avoid network issues during development.**