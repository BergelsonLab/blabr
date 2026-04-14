# Merge tables from split fixation and message reports

The main point of merging is to have one list and no repeating data. For
example, we don't have two "trials" tables in memory where the one
coming from the message report has more trials - we want them merged.

## Usage

``` r
merge_split_reports(fix_rep_data, mes_rep_data)
```

## Arguments

- fix_rep_data:

  List of tables as returned by `split_fixation_report`.

- mes_rep_data:

  List of tables as returned by `split_message_report`.
