# Load CDI norms for all metrics and all genders for WG form

Load CDI norms for all metrics and all genders for WG form

## Usage

``` r
load_cdi_norms_wg()
```

## Value

A list of three dataframes ("boys", "girls", "both"). Each dataframe is
a combined norm conversion table for every WG metrics for that gender.
Each row is one percentile (1-99) for each age (8-18), each column is a
different metric (e.g. comp, prod, earlygest,...), and each cell is the
score for that metric at said percentile
