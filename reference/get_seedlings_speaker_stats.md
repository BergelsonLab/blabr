# Calculates the number of Seedlings annotations in each interval (by speaker)

Calculates the number of Seedlings annotations in each interval (by
speaker)

## Usage

``` r
get_seedlings_speaker_stats(intervals, annotations)
```

## Arguments

- intervals:

  A tibble with columns depending on `time_type`:

  - For `wall`: `interval_start`, `interval_end` (POSIXct),
    `interval_start_wav` (ms)

  - For `wav`: `interval_start_wav`, `interval_end_wav` (both in ms
    since wav start) Can contain other columns.

- annotations:

  A tibble loaded from a csv annotations file from the Seedlings project
  or similar

## Value

A tibble with `interval_start` and `interval_end` columns for each
interval in `intervals` and two new columns: `speaker` and
`n_annotations`.
