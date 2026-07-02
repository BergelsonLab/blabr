# Read in a babar output csv file, or a folder of babar output csv files, and compile into one dataframe

Read in a babar output csv file, or a folder of babar output csv files,
and compile into one dataframe

## Usage

``` r
read_babar(filepath, batch)
```

## Arguments

- filepath:

  path to either a babar csv file or a folder containing multiple babar
  csv files

- batch:

  TRUE if reading a folder, FALSE if reading only one file

## Value

A combined dataframe of all babar csv file read, with new columns for
duration and recording_id without .eaf extension
