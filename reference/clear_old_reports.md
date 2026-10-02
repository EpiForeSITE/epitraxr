# Clear out old reports before generating new ones.

`clear_old_reports` deletes reports from previous runs and returns a
list of the reports that were deleted.

## Usage

``` r
clear_old_reports(internal, public)
```

## Arguments

- internal:

  Filepath. Folder for internal reports.

- public:

  Filepath. Folder for public reports.

## Value

The list of old reports that were cleared.

## Examples

``` r
ireports_folder <- file.path(tempdir(), "internal")
preports_folder <- file.path(tempdir(), "public")
dir.create(ireports_folder)
dir.create(preports_folder)

clear_old_reports(ireports_folder, preports_folder)
#> [[1]]
#> character(0)
#> 
#> [[2]]
#> character(0)
#> 
unlink(c(ireports_folder, preports_folder), recursive = TRUE)
```
