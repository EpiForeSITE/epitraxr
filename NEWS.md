# epitraxr (development version)

## New Features
* Initial release of the epitraxr package
* EpiTrax data processing and validation functions
* Monthly, annual, and year-to-date report generation
* Public and internal report formats
* Configurable trend analysis with thresholds
* Excel and PDF report output capabilities
* R Markdown template integration
* Comprehensive filesystem management
* Sample dataset for testing and examples

## Core Functions
* `get_epitrax()` - Create structured EpiTrax data objects
* `create_report_*()` family - Generate various report types
* `epitrax_*()` methods - Object-oriented report generation
* `write_report_*()` functions - Export to CSV, Excel, and PDF formats
* Configuration management with YAML support

## Documentation & Testing
* Full roxygen documentation for all functions
* Comprehensive unit test suite using tinytest
* Practical examples using package sample data
* CI/CD integration with GitHub Actions

## Dependencies
* **Imports**: lubridate, stats, utils, writexl, yaml
* **Suggests**: readxl, tinytest, rmarkdown
