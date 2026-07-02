# Calculate per-speaker statistics based on the VTC output and add it to the original tibble. Do the same calculation as `get_vtc_speaker_stats` but keep all the original columns.

Calculate per-speaker statistics based on the VTC output and add it to
the original tibble. Do the same calculation as `get_vtc_speaker_stats`
but keep all the original columns.

## Usage

``` r
add_vtc_speaker_stats(intervals, all_rttm)
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

Same as `intervals` but with three new columns: `voice_type` and
`duration` and `count` of VTC annotations. The `duration` is in seconds.
