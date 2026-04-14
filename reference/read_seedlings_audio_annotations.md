# Read audio sparse code csv from the Seedlings annotations.

Read audio sparse code csv from the Seedlings annotations.

## Usage

``` r
read_seedlings_audio_annotations(file_path, parse_timestamp_audio = T)
```

## Arguments

- file_path:

  Path to the sparse code csv file.

- parse_timestamp_audio:

  Should the timestamp column be split into the "onset" and "offset"
  integer columns?

## Value

A tibble with all the columns in the file at `file_path` read as
character vector. If `parse_timestamp_audio` is true, there will be two
additional integer columns: `onset` and `offset`.
