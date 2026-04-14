# Concatenate all the basic level files

Concatenate all the basic level files

## Usage

``` r
concat_all_bl(x, output = NULL)
```

## Arguments

- x:

  list of by-month basic level tibbles

- output:

  if specified, path to the output csv file

## Value

a tibble with all the basic level data

## Examples

``` r
if (FALSE) { # \dontrun{
x <- concat_month_bl("dir/with/bl/files", "output/folder", "video")
concat_all_bl(x, "all_video.csv")
} # }
```
