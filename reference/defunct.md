# Defunct functions

**\[defunct\]**

These functions were removed from blabr. If there's a known replacement,
calling the function will tell you about it.

## Usage

``` r
# Deprecated in 0.22.0 -------------------------------------

load_tsv(fixrep_path, val_guess_max = 1e+05)

fixations_report(
  fixrep_path,
  val_guess_max = 1e+05,
  remove_unfinished = TRUE,
  remove_practice = TRUE
)

binifyFixations(
  gaze,
  binSize = 20,
  keepCols = c("Subject", "TrialNumber", "Target", "T"),
  maxTime = NULL
)

get_windows(
  fix_mes_age,
  bin_size = 20,
  nb_1 = 18,
  short_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$short,
  med_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$med,
  long_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$long
)

FindLowData(gazeData, subsetWin, window_size = NULL, nb_2 = 0, binSize = 20)

RemoveLowData(
  gazeData,
  subsetWin,
  window_size = NULL,
  nb_2 = 0,
  binSize = 20,
  propt = "propt",
  timeBin = "timeBin",
  Trial = "Trial",
  SubjectNumber = "SubjectNumber"
)

RemoveFrozenTrials(gazeData, Trial, SubjectNumber, gaze)
```
