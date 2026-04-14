# Sample intervals randomly

Sample intervals randomly

## Usage

``` r
sample_intervals_randomly(
  intervals,
  size,
  duration,
  allow_fewer = FALSE,
  seed = NULL
)
```

## Arguments

- intervals:

  A tibble with columns depending on `time_type`:

  - For `wall`: `interval_start`, `interval_end` (POSIXct),
    `interval_start_wav` (ms)

  - For `wav`: `interval_start_wav`, `interval_end_wav` (both in ms
    since wav start) Can contain other columns.

- size:

  required sample size

- duration:

  Interval duration supported by
  [`lubridate::period`](https://lubridate.tidyverse.org/reference/period.html),
  e.g., '2 mins'.

- allow_fewer:

  Is it OK if there are fewer than size intervals?

- seed:

  Seed for the random number generator.

## Value

A subsample of intervals.
