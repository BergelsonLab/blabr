# Add LENA stats to each interval in a dataframe

Add LENA stats to each interval in a dataframe

## Usage

``` r
add_lena_stats(its_xml, intervals, time_type = c("wall", "wav"))
```

## Arguments

- its_xml:

  XML object created by
  [`rlena::read_its_file`](https://rdrr.io/pkg/rlena/man/read_its_file.html).

- intervals:

  A tibble with columns depending on `time_type`:

  - For `wall`: `interval_start`, `interval_end` (POSIXct),
    `interval_start_wav` (ms)

  - For `wav`: `interval_start_wav`, `interval_end_wav` (both in ms
    since wav start) Can contain other columns.

- time_type:

  Either `wall` or `wav`. If `wall` (default), expects columns
  `interval_start`, `interval_end` (POSIXct timestamps), and
  `interval_start_wav` (milliseconds since wav start). Will calculate
  `interval_end_wav` dynamically and drop it from output. If `wav`,
  expects `interval_start_wav` and `interval_end_wav` (both in
  milliseconds since wav start).

## Value

Same as `intervals` but with three new columns: `cvc`, `ctc`, and `awc`.
Missing values are replaced with 0 for intervals without segments.

## Note

ITS segments overlapping with intervals are allocated proportionally
based on the overlap duration. If a segment overlaps multiple intervals,
it contributes proportionally to each based on overlap length.

The difference between this function and `calculate_lena_like_stats` is
that the latter calculates exactly the same stats but does it for every
five minute period of wall time (e.g., 13:00-13:05). It should be
rewritten to first create these intervals and then use `add_lena_stats`
to avoid code repetition (see issue \#).
