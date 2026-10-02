# Standardize diseases for report

'standardize_report_diseases' removes rows from the data that shouldn't
appear in the report and adds rows for diseases that should be in the
report, but weren't in the input dataset. Added rows are filled with 0s.

## Usage

``` r
standardize_report_diseases(data, diseases)
```

## Arguments

- data:

  Dataframe. Current report data.

- diseases:

  Character vector. Diseases to include in the report.

## Value

Report data with rows for all diseases to report.

## Examples

``` r
df <- data.frame(disease=c("A","B","D"), counts=c(5,7,8))
standardize_report_diseases(df, c("A","C"))
#>   disease counts
#> 1       A      5
#> 2       C      0
```
