# epitraxr 0.3.1

## Major Changes

* **Initial release** of the epitraxr package for manipulating EpiTrax data and generating reports
* Added comprehensive report generation system with both internal and public-facing reports
* Introduced **combined monthly and YTD statistics reports** via `create_public_report_combined_month_ytd()` function
* Created flexible EpiTrax object system with data validation and configuration management
* Added **grouped disease reports** with support for disease categorization via `Group_name` field
* Implemented **configurable trend analysis** with customizable threshold parameters
* Added **PDF report generation** with R Markdown templates for professional output

## New Functions

### Report Generation
* `create_public_report_month()` - Generate monthly cross-section public reports
* `create_public_report_ytd()` - Generate year-to-date public reports  
* `create_public_report_combined_month_ytd()` - Generate comprehensive reports combining monthly and YTD statistics
* `create_report_monthly_counts()` - Create monthly disease count reports
* `create_report_monthly_avgs()` - Create monthly average reports
* `create_report_monthly_medians()` - Create monthly median reports
* `create_report_ytd_counts()` - Create year-to-date count reports
* `create_report_ytd_medians()` - Create year-to-date median reports
* `create_report_annual_counts()` - Create annual count reports
* `create_report_grouped_stats()` - Create grouped statistical reports with disease categorization

### Data Processing
* `read_epitrax_data()` - Read and process EpiTrax CSV data files
* `prep_report_data()` - Prepare data for report generation
* `get_epitrax()` - Extract EpiTrax object from processed data
* `setup_epitrax()` - Set up EpiTrax object with data and configuration

### File System Management
* `setup_filesystem()` - Create and manage report directory structure
* `create_filesystem()` - Create internal, public, and settings folders
* `clear_old_reports()` - Clean up previous report files
* `get_report_disease_lists()` - Load disease list configurations

### Configuration and Validation
* `epitrax_set_config_from_file()` - Configure EpiTrax object from YAML file
* `epitrax_set_config_from_list()` - Configure EpiTrax object from R list
* `validate_config()` - Validate report configuration parameters with defaults
* `validate_data()` - Validate EpiTrax data structure and column requirements
* `validate_filesystem()` - Validate directory structure for reports

### Output Generation
* `write_report_csv()` - Export reports as CSV files
* `write_report_xlsx()` - Export reports as Excel files
* `write_report_pdf()` - Generate PDF reports using R Markdown templates
* `write_grouped_report_pdf()` - Generate PDF reports for grouped disease statistics
* `epitrax_write_csvs()` - Batch export multiple CSV reports
* `epitrax_write_xlsxs()` - Batch export multiple Excel reports
* `epitrax_write_pdf_public_reports()` - Generate PDF reports for public distribution
* `epitrax_write_pdf_grouped_stats()` - Generate grouped PDF reports with categorized diseases

### Utility Functions
* `convert_counts_to_rate()` - Convert case counts to rates per 100,000
* `get_trend()` - Calculate trend indicators for disease surveillance with configurable thresholds
* `format_week_num()` - Format MMWR week numbers
* `reshape_monthly_wide()` - Reshape data to monthly wide format
* `reshape_annual_wide()` - Reshape data to annual wide format

## Features

* **Flexible disease list management** with support for internal and public-facing disease names
* **Disease grouping and categorization** via `Group_name` field for organized reporting
* **Population-based rate calculations** with configurable population parameters
* **Configurable trend analysis** with customizable thresholds for disease surveillance 
* **Multiple output formats** including CSV, Excel, and PDF reports with professional templates
* **Comprehensive test suite** with tinytest framework covering all major functionality
* **Sample data** included for testing and demonstration purposes
* **YAML configuration management** for easy setup and deployment
* **Automated report directory structure** creation and management
* **Piped workflow support** for streamlined data processing and report generation

## Dependencies

* R packages: lubridate, stats, utils, writexl, yaml
* Suggested packages: readxl, tinytest, rmarkdown

## Documentation

* Complete function documentation with examples
* README with installation instructions and usage examples
* Comprehensive test coverage across all major functions