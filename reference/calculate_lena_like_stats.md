# Calculate stats similar to those in LENA's 5min.csv files

Calculate stats similar to those in LENA's 5min.csv files

## Usage

``` r
calculate_lena_like_stats(its_xml, duration)
```

## Arguments

- its_xml:

  XML object created by
  [`rlena::read_its_file`](https://rdrr.io/pkg/rlena/man/read_its_file.html).

- duration:

  Interval duration supported by
  [`lubridate::period`](https://lubridate.tidyverse.org/reference/period.html),
  e.g., '2 mins'.

## Value

a tibble with at least these four columns: interval_start, interval_end,
AWC.Actual, CTC.Actual, CWC.Actual
