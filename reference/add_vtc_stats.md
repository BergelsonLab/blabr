# Add ctc calculated based on the vtc annotations

Add ctc calculated based on the vtc annotations

## Usage

``` r
add_vtc_stats(intervals, all_rttm)
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

## Value

Same as `intervals` but with additional `vtc_ctc` column.

## Note

Ported from childrpoject's high-volubility sampler code.
