# epitraxr (development version)

* Allow user to choose how many years of data to include in reports (defaults is 5 + the latest year)
* Rename `prep_report_data()` to `standardize_report_diseases()`
* Add `vignette("piped-mode")`
* Update `vignette("epitraxr")` with clearer language and examples
* Fix bug in `epitrax_write_xlsxs()` where it would write Excel sheets even if report lists were empty

# epitraxr 0.3.2

* Create grouped reports
* Enable PDF output for some reports
* Add threshold parameter to `get_trend()`
* Add new report combining monthly and YTD stats
* Complete R package checklist

# epitraxr 0.2.2

* Minor fixes to verify piped script matches original script behavior

# epitraxr 0.2.1

* Add method for setting config programmatically

# epitraxr 0.2.0

* Fully support pipe operator add report generation functions for EpiTrax object

# epitraxr 0.1.3

* Add pkgdown website
* Create new EpiTrax data object

# epitraxr 0.1.0

* Initial release of epitraxr package
* Core modules: Data Processing, Report Generation, Filesystem Management, Helper Functions
* Sample data included for testing and demonstration purposes
* Comprehensive testing with tinytest framework
* Full roxygen documentation for all functions
