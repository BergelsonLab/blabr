# Tag low-data trials based on fixation timeseries

Identifies and marks trials with insufficient data based on the
proportion of time points within a specified time window that contain
valid data. A trial is considered "low-data" if less than `min_fraction`
of the time window contains valid data.

## Usage

``` r
tag_low_data_trials(
  fixation_timeseries,
  window_column = NULL,
  t_start = NULL,
  t_end = NULL,
  t_step = 20,
  min_fraction
)
```

## Arguments

- fixation_timeseries:

  A dataframe containing fixation timeseries data. It must minimally
  contain the following columns:

  - `recording_id`: Identifier for the recording session.

  - `trial_index`: Index or identifier for the trial.

  - `is_good_timepoint`: Logical vector indicating valid time points
    (`TRUE` or `FALSE`).

  - If `window_column` is provided: a column with that name indicating
    time bins within the window of interest. Values should be `"Y"` or
    `"N"`.

- window_column:

  (Optional) A string specifying the name of the column that indicates
  (using the factor label `"Y"`) the time bins that belong to the window
  being tested for insufficient data. Either `window_column` or both
  `t_start` and `t_end` must be supplied.

- t_start:

  (Optional) Numeric value specifying the lower bound of the window of
  interest in milliseconds from the target onset. Must be provided along
  with `t_end` if `window_column` is not supplied.

- t_end:

  (Optional) Numeric value specifying the upper bound of the window of
  interest in milliseconds from the target onset. Must be provided along
  with `t_start` if `window_column` is not supplied.

- t_step:

  The time step in milliseconds. Must match the one used in
  `fixations_to_timeseries`.

- min_fraction:

  Numeric value between `0` and `1` indicating the minimum fraction of
  the window that must contain valid data for the trial to be considered
  "high-data". For example, `min_fraction = 1/3` requires at least
  one-third of the window to have valid data. **This parameter must be
  specified.**

## Value

The input dataframe with an additional logical column
`is_trial_low_data`, indicating whether each trial is considered
low-data (`TRUE`) or not (`FALSE`).

## Details

Time points counted as having data meet both of the following criteria:

- `is_good_timepoint` is `TRUE`. This column is typically created with a
  condition like `mutate(is_good_timepoint = some_condition)`. The
  definition can vary between studies.

- The time bin is within the window of interest, indicated by the
  `window_column` or specified by `t_start` and `t_end`.

**Usage Requirements:**

- **Either** `window_column` **or both** `t_start` **and** `t_end`
  **must be supplied.**

  - If `window_column` is provided, the function uses it to identify
    time bins within the window of interest.

  - If `t_start` and `t_end` are provided:

    - The function checks if a column named `window_{t_start}_{t_end}ms`
      exists in `fixation_timeseries`.

      - If it exists, the function stops and suggests using
        `window_column = "window_{t_start}_{t_end}ms"` instead.

      - If it does not exist, the function calls
        [`assign_time_windows()`](http://bergelsonlab.com/blabr/reference/assign_time_windows.md)
        to create the required window column and proceeds.

    - The temporary `which_window_{t_start}_{t_end}ms` column created by
      [`assign_time_windows()`](http://bergelsonlab.com/blabr/reference/assign_time_windows.md)
      is dropped after use.

The function calculates the minimum number of time points with valid
data required for a trial to be considered "high-data", based on the
`min_fraction` and the duration of the time window (`t_end - t_start`).
It then tags each trial by adding a new column `is_trial_low_data`,
which is `TRUE` for low-data trials and `FALSE` otherwise.
