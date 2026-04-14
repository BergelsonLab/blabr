# Convert a dataframe of fixation intervals to an evenly spaced timeseries

- The fixations are first dropped/trimmed if `t_max` is set.

- The fixation interval boundaries are then rounded up to the nearest
  multiples of `t_step`.

- Each fixation row is then duplicated as many time as there are time
  points (also multiples of `t_step`) falling within the rounded up
  intervals. The timepoins are stored in a new column `time`.

- If a timepoint falls within multiple fixations, only the row with the
  later fixation is kept.

- If a timepoint doesn't fall within any fixations in a given trial, it
  won't appear in the output at all.

## Usage

``` r
fixations_to_timeseries(fixations, t_step = 20, t_max = NULL)
```

## Arguments

- fixations:

  A dataframe that must minimally contain the following columns:

  - `recording_id` and `trial_index` that uniquely identify trials.

  - `t_start`, `t_end` - the start and end times of the fixations in ms.
    Use `read_*_report`, `split_*_report`, and `merge_split_reports` to
    get such a dataframe.

- t_step:

  The time step in ms.

- t_max:

  Optional. The maximum time in ms. Fixations that start after this time
  will be removed and the end time of fixations that end after this time
  will be set to this time.

## Value

A dataframe with similar columns as the input dataframe but with many
more rows. Here are the differences in columns:0

- A new column `time` was added that contains the time points that are
  multiples of `t_step` and are within the fixation interval after it
  was rounded up to the nearest multiple of `t_step`.

- The `t_start` and `t_end` columns were renamed to `fixation_start` and
  `fixation_end`.

- Some fixations may have been dropped if they started after `t_max` or
  if they were all in one time bin and it was shared with another
  fixation.

## Examples

``` r
fixations <- data.frame(
  recording_id = seq(1, 3),
  trial_index = seq(1, 3),
  t_start = c(105, 202, 256),
  t_end = c(155, 241, 560),
  x = c(0, -50, 50),
  y = rep(0, 3))
t_step <- 20
t_max <- 400
fixations_to_timeseries(fixations, t_step, t_max)
#>    recording_id trial_index fixation_start fixation_end   x y time
#> 1             1           1            105          155   0 0  120
#> 2             1           1            105          155   0 0  140
#> 3             1           1            105          155   0 0  160
#> 4             2           2            202          241 -50 0  220
#> 5             2           2            202          241 -50 0  240
#> 7             3           3            256          400  50 0  260
#> 8             3           3            256          400  50 0  280
#> 9             3           3            256          400  50 0  300
#> 10            3           3            256          400  50 0  320
#> 11            3           3            256          400  50 0  340
#> 12            3           3            256          400  50 0  360
#> 13            3           3            256          400  50 0  380
#> 14            3           3            256          400  50 0  400
```
