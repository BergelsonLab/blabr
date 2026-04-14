# Write the seedlings audio annotation tibble back to the csv file

Write the seedlings audio annotation tibble back to the csv file

## Usage

``` r
write_seedlings_audio_annotations(annotations, file_path)
```

## Arguments

- annotations:

  A tibble such as read by read_seedlings_audio_annotations.

- file_path:

  File path to write to.

## Value

Input `annotations` with any extra columns removed and the "pho" column
having NA filled with empty strings - the tibble that is fed to
[`readr::write_csv`](https://readr.tidyverse.org/reference/write_delim.html).
