# The the motor questionaire spreadsheet for the SEEDLingS babies

The the motor questionaire spreadsheet for the SEEDLingS babies

## Usage

``` r
get_motor_spreadsheet(version = NULL, type = "feather")
```

## Arguments

- version:

  version tag to checkout

- type:

  "feather" or "csv". defaults to "feather"

## Value

a tibble contaiing the SEEDLingS Motor Questionaire spreadsheet

## Examples

``` r
motor <- get_motor_spreadsheet(version = '0.0.2')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "motor_spreadsheet" repository at the following location: /home/runner/BLAB_DATA/motor_spreadsheet. Please clone it.
```
