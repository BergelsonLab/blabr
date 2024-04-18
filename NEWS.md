# blabr 0.21.0

## Changed

- `get_vihi_annotations` now
  - filters out PI by default,
  - returns only randomly sample intervals by default,
  - can return annotations from random and top-5 high-volubitlity intevals for VI and their TD matches with `subset = 'VI+TD-VI'`,
  - checks ACLEW tiers for consistency,
  - see `?get_vihi_annotations` for more details.
  
## Fixed

- Lots in tests. They are still faling though.

# blabr 0.20.1

## Added

- Account for changes in the vihi_annotations dataset: 
  - Three new tiers: fun, pro, rep.
  - Columns with the transcription and its annotation id are now called `transcription` and `transcription_id`.

# blabr 0.20.0

## Added

- Introduced `get_seedlings_nouns_extra` and `get_seedlings_nouns_codebook` functions.
  `get_seedlings_nouns` only loads the main table now.
- `get_seedlings_nouns` and friends now produce messages informing user about the existence of codebooks, relation to other tables, etc.
- Dataset versions removed from GitHub but persisting locally will no longer be loaded.
- Add `get_blab_share_path`, remove `get_pn_opus_path`.

## Fixed

- The help page of `get_seedlings_nouns` has been updated.
- Check that the requested dataset versions are present in the GitHub repo.
  Supplying non-exitent versions to `get_*` functions (`get_seedlings_nouns`, `get_vihi_annotaitons`, etc.) used to lead to loading of the version that was currently in `BLAB_DATA`.
- Enforce column specification in `get_*` functions: throw an error if it doesn't match the data.
  Previously, if I messed up and didn't add new columns to the code at all or didn't use them for specific dataset versions, there was no indication of that.
- Fix column misspecification of seedlings-nouns tables revealed by enforcing column specifications.
- Add missing column specs to get_vihi_annotations.
- Avoid repeating dataset version handling unnecessariliy resulting in doubling of reminder to supply a version to the function call or update the requested version.
- Throw an error if a git command throws one.

# blabr 0.19.1

## Fixed

- There were unqualified calls in `get_vihi_annotations`. I don't yet know why
  this wasn't flagged by `devtools::check()` or didn't cause a test error.

# blabr 0.19.0

## Added:

- Function `get_vihi_annotations` similar to other `get_*` functions.

# blabr 0.18.1

## Fixed:

- Correctly identify public versions of datasets.

# blabr 0.18.0

Fix:
- No more installation errors due to missing the `tidyverse` meta-package.
- Now, neither the packages from `tidyverse`, nor any other packages are attached after running `library(blabr)`.
  This forces the user to insert explicit `library(<pkg>)` calls to their own code leading to fewer unintended consequences, e.g., `filter` referring to `dplyr::filter` instead of the standard `stats::filter` even when user didn't run `library(dplyr)`.

# blabr 0.17.0

Features:
- Switched to the public version in `get_seedlings_nouns`.
  The development versions can still be requested.
- Now, `get_seedlings_nouns` can get other tables and codebooks from the SEEDLingS - Nouns dataset with the `table` and `get_codeobook` parameters.

Fixes:
- `CONTRIBUTING.md` - `devtools::test()` should be run before `devtools::check()`.
- Multiple tests don't fail anymore.
  Except for `test-seedlings.R`, this one is skipped for now.

# blabr 0.16.3

Fixes: 
- Take into account that `global_bl` already exists when adding an updated global_bl column to "all_basiclevel_NA.csv".
- `col_factor` was called without qualifying with `readr::`.

# blabr 0.16.2

Fix: correctly add `global_bl` column specification when reading "all_basiclevel_NA.csv".

# blabr 0.16.1

Account for adding `global_bl` column directly to `all_basiclevel`.

# blabr 0.16.0

Account for csvs in the `seedlings-nouns_private` having moved to the "public/" subfolder.

# blabr 0.15.0

`get_all_basiclevel` uses "all_basiclevel_na.csv" only as it will be the only file in the `all_basiclevel` repo from now on.
If you need to stick to an older version of blabr and `get_all_basiclevel` stopped working for you, add `type = 'csv'` to the call.

# blabr 0.14.2

Account for global basic level dictionaries having moved to the all_basiclevel repo.

# blabr 0.14.1

Fix: make `get_*` functions work on Windows.

# blabr 0.14.0

Add function  `get_seedlings_nouns` that loads the `seedlings-nouns` dataset from the lab-private repo.

# blabr 0.13.2

Switch to Makrdown for docs.

# blabr 0.13.1

Bugfix: update `big_aggregate` to reflect the switch from "TVS" to "TVN" as "speaker" value.

# blabr 0.13.0

- Improved handling of segments spanning two intervals in `add_lena_stats` and
  `make_five_min_approximation`: each such segment contributes utterance counts
  to these intervals in proportion to the overlap.
  
  With this change, `make_five_min_approximation` produces `awc` and `cvc` on
  the test file that differ from the corresponding lena5min csv file by at most
  1.

# blabr 0.12.0

- New functions: `prepare_intervals` and `add_lena_stats` that previously used
  to be a single function `make_five_min_approximation`. The latter still exists
  but calls the former two now.
  
  There are some changes to the behavior of `make_five_min_approximation`:
  - No more zero-duration intervals.
  - Segments overlapping with two intervals now count fully towards both
    (previously they would count only towards the first one).
  - Intervals returned for any time point the recording was on (previously, 
    only intervals with segments starting in them were returned).

# blabr 0.11.0

- Multiple fixes to the global basic level logic:
  - ambiguous words in `object_dict` can't have rows with `NA` in
    `disambiguate`,
  - more specific instructions for manual updates,
  - copy `object_dict` when some annotations need disambiguating even if the
    dictionary itself does not need to be updated.

# blabr 0.10.0

- Add `get_pn_opus_path` function.

# blabr 0.9.0

- Only objects marked for export in the roxygen comments will be exported. If a
  function from `blabr` no longer works, use `blabr:::<function_name>` and tell
  the lab technician.

# blabr 0.8.1

- Updated docs for `get_vtc_speaker_stats`.

# blabr 0.8.0

- Added `add_vtc_stats` function that calculates VTC-bases ctc. Ported from
  childproject.

# blabr 0.7.0

- Added `add_lena_stats` function that calculates ctc, cvc, and awc for a set of
  intervals.

# blabr 0.6.0

- Added `make_new_global_basic_level` function that loads `all_basiclevel`,
  and adds a `global_bl` column to it which contains global basic levels.
  Clone `global_basic_level` to `~/BLAB_DATA` before using.

# blabr 0.5.2

- bugfix: `get_seedlings_speaker_stats` now uses the `speaker` field from the
  sparse code csvs, instead of the LENA-identified `tier` field.

# blabr 0.5.1

- bugfix: `get_seedlings_speaker_stats` called multiple functions without
  specifying the `library::` part.

# blabr 0.5.0

- Added `read_rttm`/`write_rttm` functions to read/write `.rttm` files that
  Voice Type Classifier (VTC) creates.
- Added functions `get_seedlings_speaker_stats`, `get_vtc_speaker_stats` that
  add stats to a set of time intervals based on Seedlings annotations and VTC
  outputs respectively.
- Renamed `get_speaker_stats` to `get_lena_speaker_stats` to make the stats
  source explicit.

# blabr 0.4.6

LENA functions:

* `calculate_lena_like_stats` now outputs an additional column
  `interval_start_wav` that contains the interval start as the number of
  milliseconds from the start of the wav file,
* `sample_intervals_*` functions keep only the necessary columns from the input
  `intervals_tibble`: `interval_start`, `interval_end`, and - in the case of
  `sample_intervals_with_highest` - the column whose values was maximized.

# blabr 0.4.5

* Removed dependency on `fuzzyjoin` and BioConductor package `IRanges` - less
  problems installing blabr.
* VIHI LENA intervals for annotation: prevent utterances counting towards two
  neighbouring intervals.

# blabr 0.4.4

* fix: sample_intervals_periodically works now

# blabr 0.4.3

* fix: correctly tell R to install IRanges from BioConductor

# blabr 0.4.2

* LENA: calculate stats, sample intervals in several ways

  - get LENA-like AWC, CTC, CVC stats for given time intervals,
  - get speaker-level stats: adult word count, total segment duration, child
    utterance count
  - sample intervals: randomly, periodically, and optimizing for a given metric

# blabr 0.4.1

* bugfix: `lag` is now prefixed with `dplyr::` in `make_five_min_approximation`,
  so that `stats::lag` is not used.

# blabr 0.4.0

* add `make_five_min_approximation` function that processes an .its file and outputs a
  tibble with columns `duration`, `AWC.Actual`, `CTC.Actual`, `CWC.Actual` that are
  similar to the ones in the LENA's 5min.csv files, except for a different handling of
  speech segments that cross a 5-min interval border: LENA splits the values between the
  two intervals, while we consider them to belong to the first one.

# blabr 0.3.0

* `get_*` functions produce similar results for ".csv" and ".feather" now. The
  attributes are not exactly the same and the orders of factor levels are
  different but now the outputs are both tibbles and have the same column types.
  The similarity is checked with `all.equal(..., check.attributes = FALSE))`

# blabr 0.2.1

* Suppress git message when checking out a tag.

# blabr 0.2.0

* `get_*` functions do not have `branch` and `commit` parameters anymore,
  instead they have a new `version` parameter that currently refers to a tag
  label in the corresponding dataset repository. Using `get_all_*` functions
  without supplying the version argument is discouraged, an appropriate warning
  is in place.
  
  Motivation for the change:
  - explicitly setting dataset version gives one a chance at reproducible
    analysis,
  - using versions instead of commit hashes lets us later choose a different
    non-git storage option. Or, even if we do go with git, the old and new
    hashes will not clash and/or confuse the users.
* Added a `NEWS.md` file to track changes to the package.
