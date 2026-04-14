# Read EyeLink fixation report file

Read EyeLink fixation report file

## Usage

``` r
read_fixation_report(
  report_path,
  guess_max = 1e+05,
  remove_unfinished = TRUE,
  remove_practice = TRUE
)
```

## Arguments

- report_path:

  Report file path.

- guess_max:

  Passed to
  [`readr::read_tsv`](https://readr.tidyverse.org/reference/read_delim.html).
  Default is 100000. Decreasing this number can speed up reading, just
  make sure there were no problems during reading by looking out for
  warning from `readr` or by passing the output dataframe to
  [`readr::problems`](https://readr.tidyverse.org/reference/problems.html).

- remove_unfinished:

  Removes lines where the `order` column is NA. Does not work in the
  current version because no value are interpreted as NA.

- remove_practice:

  Only keeps rows where the `practice` column is "n".

## Value

A dataframe with the report data.
