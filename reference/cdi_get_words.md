# Select all word item columns for a cdi spreadsheet This function was used to generate the current seedlings cdi spreadsheet as retrieved by `get_cdi_spreadsheet()`.

Select all word item columns for a cdi spreadsheet This function was
used to generate the current seedlings cdi spreadsheet as retrieved by
[`get_cdi_spreadsheet()`](http://bergelsonlab.com/blabr/reference/get_cdi_spreadsheet.md).

## Usage

``` r
cdi_get_words(data, cdi_type = "wg")
```

## Arguments

- data:

  a dataframe of the original cdi csv

- cdi_type:

  Either wg or ws

## Value

New dataframe containing only vocabulary item column
