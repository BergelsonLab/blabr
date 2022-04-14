# The "basic_level" column in the dataset collapses version of the same word:
# "doggie", "dogs", and "dog" get mapped to "dog". This is done at the recording
# level. Here, we create a *global* basic level version of that done at the
# corpus level.


add_global_basic_level <- function(all_basiclevel_na) {
  # Check that there are rows with NA in the basic_level column
  assertthat::assert_that(sum(is.na(all_basiclevel_na$basic_level)) > 0)

  data_dir <- file.path(blab_data, 'global_basic_level', 'data')
  dict <- readr::read_csv(
    file.path(data_dir, "global_bl_dictionary.csv"),
    col_types = cols(
      object = col_character(),
      disambiguate = col_character(),
      global_bl = col_character()
    ))
  disamb_rows <- readr::read_csv(
    file.path(data_dir, "disambiguated_rows.csv"),
    col_select = c(annotid, object, disambiguate),
    col_types = cols(
      object = col_character(),
      annotid = col_character(),
      disambiguate = col_character()
    ))

  all_bl_NA2 <- dplyr::left_join(
    all_basiclevel_na,
    disamb_rows,
    by = c(
      "object",
      "annotid"
    )
  )

  new_global_bls <- readr::read_csv(
    file.path(data_dir, "new_entries_for_all_bl_disamb_feb2021.csv"),
    col_types = cols(
      object = col_character(),
      basic_level = col_character(),
      global_bl = col_character()
    ))

  dict_full <- full_join(dict, new_global_bls) %>%
    dplyr::select(-basic_level) # %>%
  # write_csv("data/global_bl_dictionary.csv")


  all_bl_NA3 <- all_bl_NA2 %>%
    dplyr::mutate(disambiguate = dplyr::case_when(
      object == "balls" & basic_level == "ball" & is.na(disambiguate) ~ "toy",
      object == "Momo" & basic_level == "Momo" & is.na(disambiguate) ~ "dog",
      object == "glasses" & basic_level == "glasses" & is.na(disambiguate) ~ "eye",
      TRUE ~ disambiguate)) %>%
    dplyr::left_join(dict_full, by = c("object", "disambiguate"))

  duplicate_lines <- all_bl_NA3 %>%
    dplyr::count(annotid) %>%
    dplyr::filter(n>1)

  to_fix <- all_bl_NA3 %>%
    dplyr::filter(annotid %in% duplicate_lines$annotid) %>%
    dplyr::left_join(
      all_bl_NA3,
      by = c(
        "ordinal",
        "onset",
        "offset",
        "object",
        "utterance_type",
        "object_present",
        "speaker",
        "basic_level",
        "annotid",
        "id",
        "subj",
        "month",
        "SubjectNumber",
        "audio_video",
        "tier",
        "pho",
        "disambiguate",
        "global_bl"
      )
    ) %>%
    dplyr::select(object, basic_level, global_bl) %>%
    dplyr::distinct()

  # write_feather(all_bl_NA3, "data/all_bl_global.feather")
  return(list(all_bl_NA3, dict_full, to_fix))
}
