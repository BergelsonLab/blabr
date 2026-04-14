# Join the audio and video dataframes into one big dataframe

Join the audio and video dataframes into one big dataframe

## Usage

``` r
join_full_audio_video(
  audiostats,
  videostats,
  output_name = NULL,
  keep_na = FALSE,
  keep_comments = FALSE
)
```

## Arguments

- audiostats:

  the full audio basic level dataframe

- videostats:

  the full video basic level dataframe

- output:

  if specified, the output name for csv and feather files (without
  extension)

## Value

the joined dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
audiomonths <- concat_month_bl("../basiclevel_dataprep/all_bl/", "output/", "audio")
audiostats <- concat_all_bl(audiomonths, "all_audio.csv")
videomonths <- concat_month_bl("../basiclevel_dataprep/all_bl/", "output/", "video")
videostats <- concat_all_bl(videomonths, "all_video.csv")
joined_data <- join_full_audio_video(audiostats, videostats)
} # }
```
