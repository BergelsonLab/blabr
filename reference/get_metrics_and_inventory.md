# Collecting phonetic and consonant inventories as space separated strings, and canonical related metrics.

The metrics calculated are:

- Canonical utterances (per utterance and per syllable): n_canonical/all

- Canonical babbling ratio: n_canonical/n_canonical + n_non_canonical

- Total number of syllables

- Total number of canonical syllables

## Usage

``` r
get_metrics_and_inventory(df, minimum_count = 50)
```

## Arguments

- df:

  babar dataframe

- minimum_count:

  the minimum number of occurences of a phoneme per recording to be
  counted as a phoneme. Default is 50.

## Value

A dataframe with these new metrics as new columns, one row per recording
