# Write report CSV files

`write_report_csv` writes the given data to the specified folder with
the given filename.

## Usage

``` r
write_report_csv(data, filename, folder)
```

## Arguments

- data:

  Dataframe. Report data.

- filename:

  String. Report filename.

- folder:

  Filepath. Report destination folder.

## Value

NULL.

## Examples

``` r
# Create sample data
r_data <- data.frame(
  Disease = c("Measles", "Chickenpox"),
  Counts = c(20, 43)
)

# Write to temporary directory
write_report_csv(r_data, "report.csv", tempdir())
unlink(file.path(tempdir(), "report.csv"), recursive = TRUE)
```
