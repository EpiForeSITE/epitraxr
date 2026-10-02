# Validate filesystem structure

`validate_filesystem` checks that the filesystem structure is valid.

## Usage

``` r
validate_filesystem(fsys)
```

## Arguments

- fsys:

  List. Contains paths to report folders with elements:

  - `internal`: Folder for internal reports

  - `public`: Folder for public reports

## Value

NULL if valid, otherwise throws an error.

## Examples

``` r
fsys <- list(
  internal = "internal_reports",
  public = "public_reports"
)
validate_filesystem(fsys)
```
