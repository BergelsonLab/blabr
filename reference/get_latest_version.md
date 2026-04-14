# Find latest version available for downloading?

Find latest version available for downloading?

## Usage

``` r
get_latest_version(repo, tags_already_updated = FALSE)
```

## Arguments

- repo:

  dataset name: 'all_basiclevel', 'reliability', etc.

- tags_already_updated:

  boolean, have the repository tags been updated recently? Avoids
  unnecessary fetching of the tags when multiple functions that use tags
  are called in succession.

## Value

the version string

## Examples

``` r
get_latest_version('all_basiclevel')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "all_basiclevel" repository at the following location: /home/runner/BLAB_DATA/all_basiclevel. Please clone it.
```
