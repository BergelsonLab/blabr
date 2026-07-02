# Calculate canonical related metrics for each recording, including:

- Canonical utterances (per utterance and per syllable): n_canonical/all

- Canonical babbling ratio: n_canonical/n_canonical + n_non_canonical

- Total number of syllables

- Total number of canonical syllables

Calculate canonical related metrics for each recording, including:

- Canonical utterances (per utterance and per syllable): n_canonical/all

- Canonical babbling ratio: n_canonical/n_canonical + n_non_canonical

- Total number of syllables

- Total number of canonical syllables

## Usage

``` r
get_canonical_metrics(df)
```

## Arguments

- df:

  babar dataframe

## Value

A dataframe with these new metrics as new columns
