
all_basiclevel <- function(all_bl_dir) {
  audiomonths <- concat_month_bl(all_bl_dir, type="audio")
  audiostats <- concat_all_bl(audiomonths)

  videomonths <- concat_month_bl(all_bl_dir, type="video")
  videostats <- concat_all_bl(videomonths)

  all_bl <- join_full_audio_video(audiostats, videostats)
  return(all_bl)
}
