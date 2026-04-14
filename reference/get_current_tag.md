# Find tag label of the currently checked out commit.

Find tag label of the currently checked out commit.

## Usage

``` r
get_current_tag(repo, tags_already_updated = FALSE)
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
all_bl <- get_all_basiclevel(version = '0.0.7')
current_tag <- get_current_tag('all_basiclevel')
stopifnot(current_tag == '0.0.7')
} # }
```
