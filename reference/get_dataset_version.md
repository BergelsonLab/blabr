# Get the version tag of the last downloaded version of a dataset

For a given dataset, returns the version tag of the version that was
last downloaded - the one that is on your computer right now.

## Usage

``` r
get_dataset_version(dataset)
```

## Arguments

- dataset:

  dataset name: 'all_basiclevel', 'reliability', etc.

## Value

list with two keys: version and date

## Details

The main assumed usage is switching from using
[`get_all_basiclevel()`](http://bergelsonlab.com/blabr/reference/get_all_basiclevel.md)
without the version argument.

Use interactively only and put the actual version string literal in your
code.

## Examples

``` r
all_bl_version <- get_dataset_version('all_basiclevel')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "all_basiclevel" repository at the following location: /home/runner/BLAB_DATA/all_basiclevel. Please clone it.
print(all_bl_version$version)
#> Error: object 'all_bl_version' not found
print(all_bl_version$date)
#> Error: object 'all_bl_version' not found
```
