# Get the all_basiclevel data from the all_basiclevel repo

Get the all_basiclevel data from the all_basiclevel repo

## Usage

``` r
get_all_basiclevel(version = NULL, drop_basic_level_na = TRUE)
```

## Arguments

- version:

  version tag to checkout

- drop_basic_level_na:

  whether to use the "\*\_NA" version that has all the nouns, including
  those whose "basic_level" is set to "NA" (drop_basic_level_na = FALSE)
  or the standard version that does not include them.

## Value

a dataframe containing the all_basiclevel data

## Examples

``` r

# get version with a specific version tag
all_bl <- get_all_basiclevel(version='0.3.2')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "all_basiclevel" repository at the following location: /home/runner/BLAB_DATA/all_basiclevel. Please clone it.
```
