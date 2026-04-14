# List available versions

List available versions

## Usage

``` r
get_versions(repo, tags_already_updated = FALSE)
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
get_versions('seedlings-nouns')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "seedlings-nouns" repository at the following location: /home/runner/BLAB_DATA/seedlings-nouns. Please clone it.
```
