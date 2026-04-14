# Sample intervals periodically, e.g. every hour

Sample intervals periodically, e.g. every hour

## Usage

``` r
sample_intervals_periodically(intervals, duration, period)
```

## Arguments

- intervals:

  A tibble with columns depending on `time_type`:

  - For `wall`: `interval_start`, `interval_end` (POSIXct),
    `interval_start_wav` (ms)

  - For `wav`: `interval_start_wav`, `interval_end_wav` (both in ms
    since wav start) Can contain other columns.

- duration:

  Interval duration supported by
  [`lubridate::period`](https://lubridate.tidyverse.org/reference/period.html),
  e.g., '2 mins'.

- period:

  Period at the end of which we should sample intervals, e.g., '1 hour'

## Value

A subsample of intervals.
