# Split a fixation report into a list of hierarchical dataframes

Split a fixation report into a list of hierarchical dataframes

## Usage

``` r
split_fixation_report(
  report,
  drop_empty_columns = TRUE,
  drop_saccades = TRUE,
  drop_prev_and_next = TRUE
)
```

## Arguments

- report:

  Fixation report dataframe as created by `read_fixation_report`.

- drop_empty_columns:

  Logical. If TRUE, columns that are all NA will be dropped.

- drop_saccades, drop_prev_and_next:

  Logical. If TRUE, columns related to saccades and previous/next
  fixations/saccades will be dropped.
