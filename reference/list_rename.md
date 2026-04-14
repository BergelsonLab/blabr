# `dplyr::rename` for lists

[`dplyr::rename`](https://dplyr.tidyverse.org/reference/rename.html) for
lists

## Usage

``` r
list_rename(.x, ..., .strict = TRUE)
```

## Arguments

- .x:

  A list.

- ...:

  Any number of `new_name = old_name` pairs.

- .strict:

  If `TRUE`, the function will throw an error if any of the new names do
  not exist in the list.
