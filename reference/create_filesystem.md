# Create filesystem

`create_filesystem` creates the given folders if they don't already
exist.

## Usage

``` r
create_filesystem(internal, public, settings)
```

## Arguments

- internal:

  Filepath. Folder for internal reports.

- public:

  Filepath. Folder for public reports.

- settings:

  Filepath. Folder for report settings.

## Value

NULL.

## Examples

``` r
internal_folder = file.path(tempdir(), "internal")
public_folder = file.path(tempdir(), "public")
settings_folder = file.path(tempdir(), "settings")

create_filesystem(
  internal = internal_folder,
  public = public_folder,
  settings = settings_folder
)

unlink(c(internal_folder, public_folder, settings_folder), recursive = TRUE)
```
