# Split a message report into a list of hierarchical dataframes

Split a message report into a list of hierarchical dataframes

## Usage

``` r
split_message_report(report, drop_empty_columns = TRUE)
```

## Arguments

- report:

  Message report dataframe as created by `read_message_report`.

- drop_empty_columns:

  Logical. If TRUE, columns that are all NA will be dropped.
