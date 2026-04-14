# Finds the latest version tag

Finds the latest version tag

## Usage

``` r
get_latest_tag(repo, tags_already_updated = FALSE)
```

## Arguments

- repo:

  dataset name: 'all_basiclevel', 'reliability', etc.

- tags_already_updated:

  boolean, have the repository tags been updated recently? Avoids
  unnecessary fetching of the tags when multiple functions that use tags
  are called in succession.

## Value

The latest version tag as a string

## Examples

``` r
if (FALSE) { # \dontrun{
get_latest_tag('all_basiclevel')
} # }
```
