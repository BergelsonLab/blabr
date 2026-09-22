# The wrangled eyetracking spreadsheet for CLF eyetracking components of all studies (CLF, PBS, Prefunc, ProsPr, WFR, biWFR).

The wrangled eyetracking spreadsheet for CLF eyetracking components of
all studies (CLF, PBS, Prefunc, ProsPr, WFR, biWFR).

## Usage

``` r
get_clf_eyetracking(version = NULL)
```

## Arguments

- version:

  version tag to checkout

## Value

a dataframe containing the fixation as time series, excluding invalid
trials

## Examples

``` r
eyetracking <- get_clf_eyetracking(version = '0.0.1')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "clf_eyetracking" repository at the following location: /home/runner/BLAB_DATA/clf_eyetracking. Please clone it.
```
