# Collecting the consonant inventory of each recording.

Collecting the consonant inventory of each recording.

## Usage

``` r
get_consonant_inventory(df, minimum_count = 50)
```

## Arguments

- df:

  babar dataframe

- minimum_count:

  the minimum number of occurences of a phoneme per recording to be
  counted as a phoneme. Default is 50.

## Value

A dataframe with these new metrics as new columns

## Details

Glides are not included in the inventory. The inventory is a space
separated count parameter.
