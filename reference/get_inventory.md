# Collecting the phonetic inventory of each recording. The inventory is a space separated string.

Collecting the phonetic inventory of each recording. The inventory is a
space separated string.

## Usage

``` r
get_inventory(df, minimum_count = 50)
```

## Arguments

- df:

  babar dataframe

- minimum_count:

  the minimum number of occurences of a phoneme per recording to be
  counted as a phoneme. Default is 50.

## Value

A dataframe with these new metrics as new columns
