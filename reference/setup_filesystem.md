# Setup the report filesystem

`setup_filesystem` creates the necessary folder structure and optionally
clears old reports. This is a convenience function that combines
`create_filesystem` and `clear_old_reports`.

## Usage

``` r
setup_filesystem(folders, clear.reports = FALSE)
```

## Arguments

- folders:

  List. Contains paths to report folders with elements:

  - `internal`: Folder for internal reports

  - `public`: Folder for public reports

  - `settings`: Folder for settings files

- clear.reports:

  Logical. Whether to clear old reports from the internal and public
  folders. Defaults to FALSE.

## Value

The input folders list, unchanged.

## See also

[`create_filesystem()`](https://epiforesite.github.io/epitraxr/reference/create_filesystem.md),
[`clear_old_reports()`](https://epiforesite.github.io/epitraxr/reference/clear_old_reports.md)
which this function wraps.

## Examples

``` r
# Create folders in a temporary directory
folders <- list(
  internal = file.path(tempdir(), "internal"),
  public = file.path(tempdir(), "public"),
  settings = file.path(tempdir(), "settings")
)
setup_filesystem(folders)
#> $internal
#> [1] "/tmp/RtmpzpkD4L/internal"
#> 
#> $public
#> [1] "/tmp/RtmpzpkD4L/public"
#> 
#> $settings
#> [1] "/tmp/RtmpzpkD4L/settings"
#> 
unlink(unlist(folders, use.names = FALSE), recursive = TRUE)
```
