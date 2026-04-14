# Concatenate basic level files by month

Concatenate basic level files by month

## Usage

``` r
concat_month_bl(input, output = NULL, type)
```

## Arguments

- input:

  directory to scan for basic level files

- output:

  directory to write concatenated by-month basic level files to

- type:

  basic level datatype ("audio" or "video")

## Value

a list of tibbles, each tibble a month's aggregated basic level
