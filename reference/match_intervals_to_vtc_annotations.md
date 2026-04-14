# Match intervals to VTC annotations

Does a brute-force interval join to match intervals to all annotations
that overlap with them.

## Usage

``` r
match_intervals_to_vtc_annotations(intervals, all_rttm)
```

## Arguments

- intervals:

  A tibble with columns depending on `time_type`:

  - For `wall`: `interval_start`, `interval_end` (POSIXct),
    `interval_start_wav` (ms)

  - For `wav`: `interval_start_wav`, `interval_end_wav` (both in ms
    since wav start) Can contain other columns.

- all_rttm:

  An `all.rttm` file from the VTC output loaded with `read_rttm`.
