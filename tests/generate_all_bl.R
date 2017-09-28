library(blabr)

################################################
# you should set the following paths yourself: #
################################################

# folder with all the individual basiclevel csv's
all_bl_dir <- "../all_bl/09-26-17"

# folder to output the by-month csv's to
per_month_output_dir <- "../basiclevel_bymonth"

# all_audio.csv output
all_audio_out <- "../output/all_audio.csv"

# all_video.csv output
all_video_out <- "../output/all_video.csv"

# all_basiclevel output name without extension (it will add the .csv and .feather)
all_bl_out <- "../output/all_basiclevel"



############################
# where the magic happens: #
############################

# audio by month
audiomonths <- concat_month_bl(all_bl_dir, per_month_output_dir, "audio")
# all audio
audiostats <- concat_all_bl(audiomonths, all_audio_out)

# video by month
videomonths <- concat_month_bl(all_bl_dir, per_month_output_dir, "video")
# all video
videostats <- concat_all_bl(videomonths, all_video_out)

# all basiclevel
all_bl <- join_full_audio_video(audiostats, videostats, all_bl_out)

