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
