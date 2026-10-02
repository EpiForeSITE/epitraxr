# Read in EpiTrax data

'read_epitrax_data' reads EpiTrax data from a CSV, validates, and
formats it. It also filters rows older than given number of years. The
input file must contain the columns:

- `patient_mmwr_year` (integer)

- `patient_mmwr_week` (integer)

- `patient_disease` (character)

## Usage

``` r
read_epitrax_data(filepath = NULL, num_yrs = 5)
```

## Arguments

- filepath:

  Optional filepath. Data file should be a CSV. If this parameter is
  NULL, the user will be prompted to choose a file interactively.

- num_yrs:

  Integer. Number of years of data to keep. Defaults to 5.

## Value

The validated and formatted EpiTrax data from the input file.

## Details

See the example file here:
`system.file("sample_data/sample_epitrax_data.csv", package = "epitraxr")`

## Examples

``` r
if(interactive()) {
  # Interactive file chooser:
  read_epitrax_data()
}

# Using a file path:
data <- read_epitrax_data(
  filepath = system.file("sample_data/sample_epitrax_data.csv",
                         package = "epitraxr"),
  num_yrs = 3
)
head(data)
#>      disease month year counts
#> 3  Influenza     7 2021      1
#> 4   COVID-19     7 2024      1
#> 6  Influenza     4 2022      1
#> 7   COVID-19     6 2021      1
#> 9    Measles     4 2023      1
#> 11 Influenza     4 2021      1
```
