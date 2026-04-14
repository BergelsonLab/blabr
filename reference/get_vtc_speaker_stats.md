# Calculate per-speaker statistics based on the VTC output. This function does not keep all of the original columns, check `add_vtc_speaker_stats` for that.

Calculate per-speaker statistics based on the VTC output. This function
does not keep all of the original columns, check `add_vtc_speaker_stats`
for that.

## Usage

``` r
get_vtc_speaker_stats(intervals, all_rttm)
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

A tibble with `interval_start` and `interval_end` columns for each
interval in `intervals` and three new columns: `voice_type` and
`duration` and `count` of VTC annotations. The `duration` is in seconds.
