# Get the CDI spreadsheet for SEEDLingS babies

Get the CDI spreadsheet for SEEDLingS babies

## Usage

``` r
get_cdi_spreadsheet(version = NULL, type = "feather")
```

## Arguments

- version:

  version tag to checkout

- type:

  "feather" or "csv". defaults to "feather"

## Value

a tibble containing the SEEDLingS CDI spreadsheet

## Examples

``` r
cdi <- get_cdi_spreadsheet(version='0.0.7')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "cdi_spreadsheet" repository at the following location: /home/runner/BLAB_DATA/cdi_spreadsheet. Please clone it.
```
