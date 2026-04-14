# Post-processing for the full basic level dataframe

Post-processing includes adding a subject and month column, whether it's
an audio or video, and reformatting timestamps.

## Usage

``` r
process_concat_bl(x)
```

## Arguments

- x:

  full (audio or video) basic level dataframe

## Value

processed basic level dataframe

## Details

The function will figure out whether it's an audio or video dataframe.
It must be one or the other though. It will not handle a joined
audio+video frame.
