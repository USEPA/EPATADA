# Get the excel downloads path for criteria files

Get the excel downloads path for criteria files

## Usage

``` r
get_downloads_path(filename = NULL)
```

## Arguments

- filename:

  the name of the .xlsx file to locate. Default is NULL and will return
  the location of the Download's folder path of your OS.

## Value

the download's folder path for a user's operating system and file name,
if provided, within the path.

## Examples

``` r
if (FALSE) { # \dontrun{
myfilepath <- get_downloads_path()
} # }
```
