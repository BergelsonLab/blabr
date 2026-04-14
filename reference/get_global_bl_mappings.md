# Get the global basic level spreadsheets

They are used to map every token in all_basiclevel_na to its global
basic level, see `map_global_basic_level` and
`update_global_basic_levels`

## Usage

``` r
get_global_bl_mappings(version = NULL)
```

## Arguments

- version:

  version tag to checkout

## Value

list of object_dict and

## Examples

``` r
global_bl_mapping <- get_global_bl_mappings(version = '0.3.2')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "all_basiclevel" repository at the following location: /home/runner/BLAB_DATA/all_basiclevel. Please clone it.
```
