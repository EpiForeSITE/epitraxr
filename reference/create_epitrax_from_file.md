# Create an EpiTrax object from data file

`create_epitrax_from_file` reads an EpiTrax data file and creates a
structured object containing the data along with commonly used metadata
and empty report lists.

## Usage

``` r
create_epitrax_from_file(filepath = NULL, num_yrs = 5)
```

## Arguments

- filepath:

  Optional filepath. EpiTrax data file should be a CSV. If this
  parameter is NULL, the user will be prompted to choose a file
  interactively.

- num_yrs:

  Integer. Number of years of data to keep. Defaults to 5.

## Value

An object of class `epitrax` containing:

- `data`: The validated and formatted EpiTrax data

- `diseases`: Vector of unique diseases in the dataset

- `yrs`: Vector of years in the dataset

- `report_year`: Most recent year in the dataset

- `report_month`: Most recent month in report_year

- `internal_reports`: Empty list to store internal reports

- `public_reports`: Empty list to store public reports

## See also

[`read_epitrax_data()`](https://epiforesite.github.io/epitraxr/reference/read_epitrax_data.md)
which this function wraps and
[`setup_epitrax()`](https://epiforesite.github.io/epitraxr/reference/setup_epitrax.md)
which wraps this function

## Examples

``` r
if (interactive()) {
  # Interactive file chooser:
  create_epitrax_from_file()
}

# Using sample data included with package
data_file <- system.file("sample_data/sample_epitrax_data.csv",
                         package = "epitraxr")
epitrax <- create_epitrax_from_file(data_file)

# Access components
head(epitrax$data)
#>     disease month year counts
#> 1 Influenza     8 2020      1
#> 2 Influenza    12 2019      1
#> 3 Influenza     7 2021      1
#> 4  COVID-19     7 2024      1
#> 5 Influenza     6 2020      1
#> 6 Influenza     4 2022      1
epitrax$diseases
#> [1] "Influenza"  "COVID-19"   "Measles"    "Chickenpox" "Syphilis"  
epitrax$report_year
#> [1] 2024
```
