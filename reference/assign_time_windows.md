# Assigns binned fixations to specified time windows

Assigns binned fixations to specified time windows

## Usage

``` r
assign_time_windows(fixation_timeseries, t_step = 20, t_starts, t_ends)
```

## Arguments

- fixation_timeseries:

  A fixation timeseries dataframe that must contain at least the
  following columns:

  - `target_onset` (numeric): Time of target onset in milliseconds.

  - `time` (numeric): Time in milliseconds from the start of the trial.

- t_step:

  The time step in milliseconds. Must match the one used in
  `fixations_to_timeseries`.

- t_starts:

  A numeric vector of start times (in milliseconds) for the time
  windows, relative to the target onset. Each value must be a multiple
  of `t_step`. If of length 1, it will be recycled to match the length
  of `t_ends`.

- t_ends:

  A numeric vector of end times (in milliseconds) for the time windows,
  relative to the target onset. Each value must be a multiple of
  `t_step`. If of length 1, it will be recycled to match the length of
  `t_starts`.

## Value

The input dataframe with the following columns added:

- `prewin` (factor): Whether a time bin comes before the target onset.
  Values are "Y" and "N".

- For each window defined by `t_starts[i]` and `t_ends[i]`, the
  following columns are added:

  - `window_{t_starts[i]}_{t_ends[i]}ms` (factor): Whether a time bin is
    in the window from `t_starts[i]` to `t_ends[i]` milliseconds after
    target onset. Values are "Y" and "N".

  - `which_window_{t_starts[i]}_{t_ends[i]}ms` (factor): Indicates
    whether the time bin is in the window (labeled as
    "t_startsi_t_endsims"), comes before the target onset ("pre"), or
    neither ("neither").

- `t_onset` (numeric): Time (in milliseconds) from the target onset
  rounded up to the nearest multiple of `t_step`.

## Details

The function assigns each time bin in `fixation_timeseries` to specified
time windows defined by `t_starts` and `t_ends` relative to the target
onset. For each window, it creates two new columns:

- `window_{start}_{end}ms`: Indicates whether the time bin falls within
  the window from `start` to `end` milliseconds after target onset.
  Values are "Y" or "N".

- `which_window_{start}_{end}ms`: Indicates whether the time bin is in
  the window (labeled as "start_endms"), comes before the target onset
  ("pre"), or neither ("neither").

Time bins located exactly at `t_starts[i]` or `t_ends[i]` are included
in the corresponding window (i.e., the intervals are inclusive of the
endpoints).

The function also adds a `prewin` column indicating whether the time bin
comes before the target onset.

The `t_onset` column is computed as the time from the target onset,
rounded up to the nearest multiple of `t_step`.

If you want to use the default BLab time windows, set `t_starts` to
`360` and `t_ends` to `DEFAULT_WINDOWS_UPPER_BOUNDS`.
