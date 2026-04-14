# Calculate per-speaker statistics based on the .its file

Only analyzes conversation blocks (blkType == 'Conversation'). Uses
point-in-time matching where segment start time must fall within
interval boundaries (unlike add_lena_stats which uses overlap-based
proportional allocation).

## Usage

``` r
get_lena_speaker_stats(its_xml, intervals, time_type = c("wall", "wav"))
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

a tibble with the following columns:

- interval columns: same as in the intervals input tibble,

- spkr: speaker identifier from LENA

- adult_word_count: non-zero for FA\* and MA\* speakers only

- utterance_count: for CH\* - the sum of childUttCnt, for everyone
  else - the number of conversation segments

- segment_duration: total duration of segments for this speaker in this
  interval
