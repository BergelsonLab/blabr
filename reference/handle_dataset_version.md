# Handles the version.

If the version isn't specified, finds the newest version and warns that
not specifying the version might not be a good idea. If it is specified,
notifies if there is a newer version available.

## Usage

``` r
handle_dataset_version(
  repo,
  version = NULL,
  tags_already_updated = FALSE,
  check_for_updates = TRUE
)
```

## Arguments

- version:

  version tag to checkout

## Value

version string
