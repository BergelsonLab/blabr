# Get the reliability spreadsheets

Get the reliability spreadsheets

## Usage

``` r
get_reliability(av, month, version = NULL)
```

## Arguments

- av:

  either "audio" or "video"

- month:

  the month as a string, e.g. "06"

- version:

  version tag to checkout

## Value

dataframe with data for the requested modality and month

## Examples

``` r
audio_06_rel <- get_reliability("audio", "06")
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "reliability" repository at the following location: /home/runner/BLAB_DATA/reliability. Please clone it.
```
