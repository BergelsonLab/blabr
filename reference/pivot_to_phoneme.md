# Wrangling the babar dataframe into long form, one row per phoneme with sonority value. Whether a sound is considered a phoneme is determined by the minimum count parameter.

Wrangling the babar dataframe into long form, one row per phoneme with
sonority value. Whether a sound is considered a phoneme is determined by
the minimum count parameter.

## Usage

``` r
pivot_to_phoneme(df, minimum_count = 50)
```

## Arguments

- df:

  babar dataframe

- minimum_count:

  the minimum number of occurences of a phoneme per recording to be
  counted as a phoneme. Default is 50.

## Value

A dataframe with one row per phoneme per recording
