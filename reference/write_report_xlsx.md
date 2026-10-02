# Write report Excel files

`write_report_xlsx` writes the given data to the specified folder with
the given filename in Excel (.xlsx) format.

## Usage

``` r
write_report_xlsx(data, filename, folder)
```

## Arguments

- data:

  List. Named list of dataframes. The name will be used as the sheet
  name.

- filename:

  String. Report filename.

- folder:

  Filepath. Report destination folder.

## Value

NULL.

## Examples

``` r
# Create sample data with multiple sheets
r_data1 <- data.frame(
  Disease = c("Measles", "Chickenpox"),
  Counts = c(20, 43)
)
r_data2 <- data.frame(
  Disease = c("Measles", "Chickenpox"),
  Rate = c(10.5, 22.7)
)
r_xl <- list(
  counts = r_data1,
  rates = r_data2
)

# Write to temporary directory
write_report_xlsx(r_xl, "report.xlsx", tempdir())
unlink(file.path(tempdir(), "report.xlsx"), recursive = TRUE)
```
