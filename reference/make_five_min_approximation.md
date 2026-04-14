# Approximate LENA's 5min.csv output

Approximate LENA's 5min.csv output

## Usage

``` r
make_five_min_approximation(its_xml)
```

## Arguments

- its_xml:

  XML object created by
  [`rlena::read_its_file`](https://rdrr.io/pkg/rlena/man/read_its_file.html).

## Value

a tibble with at least these four columns: interval_start, interval_end,
AWC.Actual, CTC.Actual, CWC.Actual

## Examples

``` r
# Download the example ITS file (code from rlena's README)
url <- paste0("https://cdn.rawgit.com/HomeBankCode/lena-its-tools/",
              "master/Example/e20160420_165405_010572.its")
tmp <- tempfile()
download.file(url, tmp)
its <- rlena::read_its_file(tmp)
make_five_min_approximation(its_xml = its)
#> # A tibble: 77 × 7
#>    interval_start      interval_end        interval_start_wav recording_id   cvc
#>    <dttm>              <dttm>                           <dbl>        <dbl> <dbl>
#>  1 2016-04-02 09:20:30 2016-04-02 09:25:00                  0            1    17
#>  2 2016-04-02 09:25:00 2016-04-02 09:30:00             270000            1    17
#>  3 2016-04-02 09:30:00 2016-04-02 09:35:00             570000            1    21
#>  4 2016-04-02 09:35:00 2016-04-02 09:40:00             870000            1    28
#>  5 2016-04-02 09:40:00 2016-04-02 09:45:00            1170000            1    31
#>  6 2016-04-02 09:45:00 2016-04-02 09:50:00            1470000            1    26
#>  7 2016-04-02 09:50:00 2016-04-02 09:55:00            1770000            1    20
#>  8 2016-04-02 09:55:00 2016-04-02 10:00:00            2070000            1    17
#>  9 2016-04-02 10:00:00 2016-04-02 10:05:00            2370000            1    29
#> 10 2016-04-02 10:05:00 2016-04-02 10:10:00            2670000            1    31
#> # ℹ 67 more rows
#> # ℹ 2 more variables: awc <dbl>, ctc <int>
```
