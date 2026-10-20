# Adding percentile scores to existing SEEDLingS CDI spreadsheet

Adding percentile scores to existing SEEDLingS CDI spreadsheet

## Usage

``` r
add_norms_to_cdi(cdi_df)
```

## Arguments

- cdi_df:

  SEEDLingS CDI spreadsheet as retrieved by
  [`get_seedlings_cdi()`](http://bergelsonlab.com/blabr/reference/get_seedlings_cdi.md)

## Details

Iteratively go through every summary metrics (if any) in SEEDLingS CDI
summary table and calculate the percentile score, by sex and for both
sexes, adding two new columns for each metric. E.g., for `CDIcomp`, the
two new columns added are `CDIcomp-percentile-both` and
`CDIcomp-percentile-sex`. Return a new dataframe with all these new
columns. The new columns will be next to their original score column
