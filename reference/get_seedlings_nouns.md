# Load data from the SEEDLingS - Nouns dataset

For the functions to work, clone
[seedlings-nouns](https://github.com/BergelsonLab/seedlings-nouns) to
`~/BLAB_DATA/seedlings-nouns/` first.

## Usage

``` r
get_seedlings_nouns(version = NULL)

get_seedlings_nouns_extra(version = NULL, table)

get_seedlings_nouns_codebook(
  version = NULL,
  table = c("seedlings-nouns", "regions", "recordings", "sub-recordings")
)
```

## Arguments

- version:

  version tag to checkout

- table:

  For `get_seedlings_nouns_extra`, the extra table to load. One of:
  "regions", "recordings", "sub-recordings". For
  `get_seedlings_nouns_codebook`, the table can also be
  "seedlings-nouns" which is also the default for that function. See
  ["README.md"](https://github.com/BergelsonLab/seedlings-nouns/blob/main/README.md)
  for details.

## Value

- For `get_seedlings_nouns`, a tibble with one annotated noun per row.

- For `get_seedlings_nouns_extra`, a tibble with one row per region,
  recording, or sub-recording depending on which table was requested.

- For `get_seedlings_nouns_codebook`, a tibble with ono row per column
  of the requested table.

## Details

- `get_seedlings_nouns()` loads the main "seedlings-nouns" table with
  the annotated nouns.

- `get_seedlings_nouns_extra()` function allows for loading additional
  tables: "regions", "recordings", and "sub-recordings".

- `get_seedlings_nouns_codebook()` function loads codebooks for any of
  the four tables mentioned above.

To get the same data every time you run the script, always supply the
version argument. To get the latest version number, run
`get_latest_version('seedlings-nouns')` and then set the version
parameter to the output number, e.g.,
`get_seedlings_nouns(version = 'v1.0.0')`.

Alternatively, don't set the version parameter, run the function, look
for the version number in the issued warning, and then set `version` to
that number. You don't need to run the function again after that.

If you are a Bergelson Lab member and you need to use a version that
isn't public, clone seedlings-nouns_private(https://github.com/Bergel
sonLab/seedlings-nouns_private) to
`~/BLAB_DATA/seedlings-nouns_private/`. The function will look in the
private repository only if you supply a corresponding private version -
one starting with "0." or ending with "-dev". To get the latest private
version, use `get_latest_version('seedlings-nouns_private')`. Otherwise,
look in the releases section on GitHub. There are no version
descriptions though so you basically select the version by date.

## Examples

``` r
version <- 'v1.0.0'
seedlings_nouns <- get_seedlings_nouns(version)
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "seedlings-nouns" repository at the following location: /home/runner/BLAB_DATA/seedlings-nouns. Please clone it.
seedlings_nouns_codebook <- get_seedlings_nouns_codebook(version)
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "seedlings-nouns" repository at the following location: /home/runner/BLAB_DATA/seedlings-nouns. Please clone it.
seedlings_regions <- get_seedlings_nouns_extra(version, 'regions')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "seedlings-nouns" repository at the following location: /home/runner/BLAB_DATA/seedlings-nouns. Please clone it.
seedlings_regions_codebook <- get_seedlings_nouns_codebook(version,
                                                           'recordings')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "seedlings-nouns" repository at the following location: /home/runner/BLAB_DATA/seedlings-nouns. Please clone it.
```
