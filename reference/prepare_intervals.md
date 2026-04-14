# Prepare intervals to potentially be annotated later

Various metrics can then be calculated for each intervals and the "best"
can then be annotated.

## Usage

``` r
prepare_intervals(its_xml, duration)
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

A tibble with
