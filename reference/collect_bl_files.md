# Collect basic level files

Collect basic level files

## Usage

``` r
collect_bl_files(input, type)
```

## Arguments

- input:

  directory to scan

- type:

  datatype of the basic level file ("audio" or "video")

## Value

dataframe with "file" column containing filepaths

## Examples

``` r
x <- collect_bl_files("../some_directory", "video")
x$files
#> character(0)
```
