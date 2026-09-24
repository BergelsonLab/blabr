# Parse a raw local subject ID and return a cleaned, standardized version of the ID

Parse a raw local subject ID and return a cleaned, standardized version
of the ID

## Usage

``` r
parse_raw_id(raw_id)
```

## Arguments

- raw_id:

  A string (or vector of strings when used with `dplyr`) representing a
  raw local subject ID, which may contain study name and subject number
  in various formats

## Details

Will look for a continuous string of letters and underscore (the study
name) followed by a continuous string of digits (the subject number),
and will return a cleaned version of the ID in the format
"studyname_subjectnumber", with the study name in uppercase, all
underscore removed, and the subject number as an integer (no leading
zeros). If the input does not match this pattern, it will return the
original input.
