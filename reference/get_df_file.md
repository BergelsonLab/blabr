# Downloads and optionally loads a csv/feather file from a specified version of a dataset

Downloads and optionally loads a csv/feather file from a specified
version of a dataset

## Usage

``` r
get_df_file(
  repo,
  filename,
  version = NULL,
  read = TRUE,
  col_types = NULL,
  version_already_handled = FALSE
)
```

## Arguments

- repo:

  dataset name: 'all_basiclevel', 'reliability', etc.

- filename:

  name of a csv/feather file

- version:

  version tag to checkout

- read:

  If TRUE (default), loads the file. Otherwise, returns the path.

- col_types:

  Passed to
  [`readr::read_csv`](https://readr.tidyverse.org/reference/read_delim.html)
  when filename ends with ".csv".

## Value

tibble for feather files, data.frame for csv files

## Examples

``` r
if (FALSE) { # \dontrun{
get_df_file('all_basiclevel', 'all_basiclevel_NA.csv', version = '0.6.4')
} # }
```
