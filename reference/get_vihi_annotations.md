# Load VIHI annotation data

Clone BLAB-private
[vihi_annotations](https://github.com/bergelsonlab/vihi_annotations.git)
repo to `~/BLAB_DATA` once before using this function.

## Usage

``` r
get_vihi_annotations(
  version = NULL,
  subset = c("random", "everything", "VI+TD-VI"),
  table = c("annotations", "intervals", "merged", "all"),
  include_all_tier_types = FALSE,
  allow_annotation_errors = FALSE,
  include_pi = FALSE
)
```

## Arguments

- version:

  version tag to checkout

- subset:

  Which pre-defined subset of the data should be loaded?

  - 'random' (the default) loads the annotations from the 15 randomly
    sampled intervals from all recordings in the corpus.

  - 'VI+TD-VI' loads the annotations from the random, the top-5
    high-volubility intervals, and the initial 90 minutes interval from
    VI recordings and their TD matches.

  - 'everything' loads all annotations from all tiers. Exercise caution
    with this option: the data will include incomplete and unchecked
    annotations.

- table:

  Which table to return - `annotations` (the default) or `intervals`. If
  `merged`, returns the `annotations` table with the interval
  information merged in. Intervals without annotations won't be
  included. If `all`, returns a named list of both tables.#'

- include_all_tier_types:

  Should all tier types be included in the output? If `FALSE` (the
  default), only tiers that are relevant to the subset are returned. For
  the 'random' and 'VI+TD-VI' subsets, the relevant tier types are:
  transcription, vcm, lex, mwu, xds. For the 'everything' subset, this
  parameter is ignored as all tier types are returned.

- allow_annotation_errors:

  In case errors are found in the annotations, should the function throw
  an error (`FALSE`, the default) or add `error_n` columns to the
  `annotations` table? Use only as a way to inspect the errors, not as a
  way to ignore them.

- include_pi:

  Should annotations marked as PI be included in the output? If `FALSE`
  (the default), they are filtered out.

## Value

A table or a list of tables depending on the `table` parameter.

## Details

The speaker TIER is identified by the `participant` column. Other tiers
are in columns.

Notes:

- Annotation are checked for errors for the standard ACLEW tiers only.
  Interval-level checks aren't currently checked at all.

- Annotations marked as PI are included. Filter them out if you don't
  want them.

- Every recording has been given an interval 0 with sampling type
  "initial", which corresponds to the first 90 minutes but EXCLUDING any
  overlap with the random or hi-vol intervals. If any of the sampled
  intervals (random or hi-vol) is in the first 90 minutes, their
  annotations will NOT be included in interval 0. However, any
  annotations within the first 90 minutes will be given the value `TRUE`
  in the `is_first_90_minutes` column regardless of whether it is in
  interval 0 or not.

- The transcribed utterance can be empty (”). Normally, that means that
  a code interval has been segmented but not annotated. But there might
  be other stray utterance segments like that.

- (relevant for non-speaker TIERs only) Currently, there is no way to
  tell whether an annotation is missing because it was not segmented or
  because it was segmented but not yet annotated: both are represented
  as NA. This will change in the future: missing segment will still be
  NA, but missing annotation will be ”.

## Examples

``` r
vitd_annotations <- get_vihi_annotations(version='0.0.0.9006-dev.5',
                                         subset='VI+TD-VI')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "vihi_annotations" repository at the following location: /home/runner/BLAB_DATA/vihi_annotations. Please clone it.

vitd <- get_vihi_annotations(version='0.0.0.9006-dev.5', subset='VI+TD-VI',
                             table='all')
#> Error in run_git_command(repo, "fetch --tags --prune --prune-tags"): Expected to find the "vihi_annotations" repository at the following location: /home/runner/BLAB_DATA/vihi_annotations. Please clone it.
vitd$annotations %>% head()
#> Error: object 'vitd' not found
vitd$intervals %>% head()
#> Error: object 'vitd' not found
```
