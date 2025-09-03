# blabr 0.25.2

# blabr 0.25.1

## Fixed

- Make `blabr` look for git in the current `PATH`.
  Previously, the path to `git` was only looked up once during the package installation leading to changes to `PATH` having no effect at all.

# blabr 0.25.0

## Changed

- `assign_time_windows`:
  - The window boundaries are now specified with two arguments: `t_starts`, `t_ends` instead of `t_start`, `short_window_time`, `med_window_time`, and `long_window_time`. `DEFAULT_WINDOWS_UPPER_BOUNDS` can be used as `t_ends`.
  - The windows columns are now called "window_<t_start>_<t_end>ms" and "which_window_<t_start>_<t_end>ms" instead of, e.g., "shortwin" and "whichwin_short". 
  - The window boundaries have to be specifed explicitly, no default values will be used.
  - The `which_window_<t_start>_<t_end>ms` columns now have the following values: "pre", "neither", and "<t_start>_<t_end>ms" (the latter used to be "short"/"med"/"long")
- `tag_low_data_trials`:
  - `min_fraction` must be specified explicitly, the default value of 1/3 was removed but will be suggested in the error message.
  - You can now either specify the `window_column` to be used or `t_start` and `t_end` removing the confusing option where an existing window column was used but its end and `t_start` were used in the calculation of the maximum number of bins in the window.
  - When you specify a window column, the boundaries will be inferred from the column name (see `assign_time_windows`).

# blabr 0.24.1

## Fixed

- Update `get_reliability` for the `0.0.4` version of the dataset.

# blabr 0.24.0

## Changed

- Streamlined update_global_basic_levels.
  It now loads all_basiclevel_NA.csv from a local file, not from BLAB_DATA. This way, there is no need to create a tagged commit in the all_basiclevel repo with the updated all_basiclevel_NA.csv that doesn't yet have global basic levels. You still need to commit and tag global basic level mapping dictionaries in the same repo though.

# blabr 0.23.0

## Changed

- Update `get_seedlings_nouns` and friends to work with the v2.0.0-dev.3 version of the dataset:
  - Update column names and types.
  - Teach to treat v2.0.0-dev.3 as a development version.
- `get_*` functions that read data from `~/BLAB_DATA` will no longer make the corresponding repos headless when the version commit is already checked out as a branch. 


# blabr 0.22.1

## Fixed

- A bug in `verify_no_overlapping_fixations` which is internally used by `fixations_to_timeseries`.

# blabr 0.22.0

## Added

- `read_message_report` function.
- `split_fixation_repor`, `split_message_report`, and `merge_split_reports` functions
  that together transform the data from a fixation and a message report into a
  list of these tables: `experiment`, `recordings`, `trials`,
  `fixations`, `messages`. Use them as the first step in eyetracking processing.

## Changed

- Most of the eyetracking functions have been renamed and updated.
  One notable difference is that they expect input dataframe to contain columns `recording_id` and `trial_index` and do not allow to choose a column as a parameter.
  Instead, use `dplyr::rename` if necessary or, better yet, keep these two column names and don't rename them.
  Functions `split_fixation_report` and `split_message_report` will create these columns for you by renaming `RECORDING_SESSION_LABEL` and `TRIAL_INDEX` from the original report tables.
- `binifyFixations` is now `fixations_to_timeseries`. What's chcanged:
  - It is now a lot faster. As an example, the processing time went from 1+ min to 1-3 s on the "ht_seedlings" dataset.
  - `keepCols` parameter was dropped, use `dplyr::select` instead.
  - `gaze`, `binSize`, and `maxTime` parameters are now `fixations`, `t_step`, and `t_max`.
    Otherwise, they didn't change.
  - It expects the fixation boundaries to be in the `t_start` and `t_end` columns.
    If you used `split_fixation_report`, they will be named like that already, otherwise use `dplyr::rename`.
  - The function will throw an error if the fixations overlap which has happened to some datasets in the past.
  - The output no longer has the `timeBin` column (it was redundant and easily confusable with `time`), `Nonset` is `t_onset` - time since the target onset rounded up to the nearest multiple of `t_step`.
- `fixations_report` is now `read_fixation_report`, parameter `val_guess_max` is just `guess_max` now.
- `get_windows` is now `assign_time_windows`. 
  - It now takes the following parameters: `fixation_timeseries`, `t_step` (previously `bin_size`, `t_start`, `short_window_time`, `med_window_time`, `long_window_time`.
  - The input dataframe must contain the `target_onset` column.
  - `nb_1` that used to take the number of time steps (bins) since the target onset until the common window start was replaced with `t_start` which takes the time in ms between those two events.
  - There will be more changes to this function in the future mainly aimed at making it harder to use one set of window boundaries in this function and then a different one in `tag_low_data_trials` (previously `FindLowData`).
- `FindLowData` is now `tag_low_data_trials`.
  - It now takes the following parameters: `fixation_timeseries`,  `window_column`,  `t_start`, `t_end`, `t_step`, `min_fraction` (defaults to 1/3).
  - `min_fraction` makes the minimum amount of data more explicit.
  - The `nb_2` parameter was replaced with `t_start` which has the same meaning as in `assign_time_windows`.
    If you are converting from older code, `nb_2` used to be in ms, unlike `nb_1` in `get_windows` which was in bins so there is nothing to convert.
  - There is no check that `t_start` and `t_end` correpond to those used in `assign_time_windows` so be careful to make sure they match.
  - The output column is now called `is_low_data_trial` and is always TRUE or FALSE (it used to be NA for some trials).
  
## Fixed

- Example for `get_vihi_annotations` uses data version that doesn't lead to errors.
- `whichwinmed` and `whichwinshort` columns in the output of `assign_time_windows` (previously, `get_windows`) are now calculated correctly.
  They used to be identical to `whichwinlong` because of a bug.

## Removed

- Many eyetracking functions were hard-deprecated in favor of their renamed and updated versions.
  Some of them are described above under "Changed".
  See `help(blabr::defunct)` for the full list.
  You can also run any of them to get a replacement suggestion.
- Removed `RemoveLowData` and `RemoveFrozenTrials`. Use `tag_low_data_trials` and `FindFrozenTrials` followed by `dplyr::filter()` instead.

# blabr 0.21.0

## Changed

- `get_vihi_annotations` now
  - filters out PI by default,
  - returns only randomly sample intervals by default,
  - can return annotations from random and top-5 high-volubitlity intevals for VI and their TD matches with `subset = 'VI+TD-VI'`,
  - checks ACLEW tiers for consistency,
  - see `?get_vihi_annotations` for more details.
  
## Fixed

- Lots in tests. They are still failing though.

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
  Supplying non-existent versions to `get_*` functions (`get_seedlings_nouns`, `get_vihi_annotaitons`, etc.) used to lead to loading of the version that was currently in `BLAB_DATA`.
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
