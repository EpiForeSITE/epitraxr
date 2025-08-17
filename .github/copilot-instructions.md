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
- All tests must pass before a PR is approved. If any tests fail, they must be fixed first.
- Tests are located in `inst/tinytest/` and cover data validation, report generation, and filesystem operations.
- To test specific functionality, you can run individual test files: `R -e "library(epitraxr); tinytest::run_test_file('inst/tinytest/test_filename.R')" --no-restore`

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

## Testing Strategy

**Primary testing approach:**
- Use the comprehensive tinytest test suite: `R -e "library(epitraxr); tinytest::test_package('epitraxr')" --no-restore`
- For targeted testing after changes to specific files, run individual test files: `R -e "tinytest::run_test_file('inst/tinytest/test_[component].R')" --no-restore`
- All tests must pass before submitting a PR

## Common Development Tasks

**When adding new functions:**
- Every new function must have an accompanying roxygen block with proper documentation
- Every new function must have corresponding unit tests in the appropriate test file in `inst/tinytest/`
- Follow existing patterns for documentation and testing

**When modifying data processing functions:**
- Run tests for the specific component: `R -e "library(epitraxr); tinytest::run_test_file('inst/tinytest/test_data.R')" --no-restore`
- Ensure all existing tests continue to pass

**When modifying report generation:**
- Run tests for report functionality: `R -e "library(epitraxr); tinytest::run_test_file('inst/tinytest/test_reports.R')" --no-restore`
- Verify that report generation tests pass

**When modifying file system functions:**
- Run tests for filesystem functionality: `R -e "library(epitraxr); tinytest::run_test_file('inst/tinytest/test_filesystem.R')" --no-restore`
- Test directory creation and cleanup functionality

## GitHub Workflows

The project includes:
- `.github/workflows/r-check.yaml` - R CMD check across multiple platforms and R versions
- `.github/workflows/build-for-cran.yaml` - CRAN-ready package building
- `.github/workflows/pkgdown.yaml` - Documentation site generation

**Important timing notes:**
- All local development operations complete in under 10 seconds
- Package installation: 2 seconds
- Full test suite: 0.5 seconds  
- Individual test files: < 0.2 seconds
- NEVER CANCEL these operations - they are fast and reliable

## Dependencies

**System dependencies (installed via apt):**
- r-base, r-base-dev, build-essential
- libcurl4-openssl-dev, libssl-dev, libxml2-dev, pandoc

**R package dependencies (from DESCRIPTION):**
- lubridate, stats, utils, writexl, yaml (Imports)
- readxl, tinytest, rmarkdown (Suggests)

**All dependencies available via apt packages to avoid network issues during development.**