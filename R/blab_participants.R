#' Parse a raw local subject ID and return a cleaned, standardized version of the ID
#' Will look for a continuous string of letters and underscore (the study name) 
#' followed by a continuous string of digits (the subject number), 
#' and will return a cleaned version of the ID in the format "studyname_subjectnumber",
#' with the study name in uppercase, all underscore removed, and the subject number as an integer (no leading zeros). 
#' If the input does not match this pattern, it will return the original input.
#' 
#' @param raw_id A string (or vector of strings when used with `dplyr`) representing a raw local subject ID, 
#' which may contain study name and subject number in various formats
#' 
parse_raw_id <- function(raw_id) {
  parsed_id <- str_match(stringr::str_trim(raw_id), "^([A-Za-z_]+)\\s*(\\d+)")
  study_id <- parsed_id[, 2]
  subject_id <- parsed_id[, 3]
  cleaned_id <- ifelse(
    is.na(study_id) | is.na(subject_id), 
    raw_id, 
    paste(
      toupper(stringr::str_remove_all(study_id, "_")),
      as.numeric(stringr::str_trim(subject_id)), 
      sep = "_")
  )
  ifelse(is.na(raw_id), NA_character_, cleaned_id)
}


#' Standardize and clean up local subject IDs for blab participants
#' 
#' @param df A data frame containing participant information, loaded in from the blab-wide participant spreadsheet
#'
wrangle_blab_participants <- function(df) {
  RN_df <- df %>% dplyr::select(blab_id, RN, CHS_global_id, n_projects)
  studies_df <- df %>% dplyr::select(-RN, -CHS_global_id, -n_projects)
  
  cleaned_df <- studies_df %>%
    dplyr::mutate(
      across(
        -c(blab_id),
        parse_raw_id
      )
    ) %>% 
    right_join(RN_df, by = "blab_id") %>% 
    select(blab_id, RN, CHS_global_id, n_projects, everything())
  
  return(cleaned_df)
}


#' Get clean blab-wide participants data from participants tracking sheet and standardize the local ids
#' 
#' @param 
#' @param 
#' 
#' @export
get_blab_participants <- function() {
  participants_path <- file.path(get_blab_share_path(), "experimental_projects/participants.xlsx")
  message("Reading all blab_wide participants data...")
  message("Certain participants have been excluded due to not finishing a study or being rescheduled and assigned a new ID, which might results in gaps in the local subject ID. Check the exclusion list for which participants are excluded, and then check each project's tracking sheet for more details.") 
  
  participants <- readxl::read_xlsx(participants_path, sheet = "Participants") %>%
    # Any prelim cleaning, selecting columns, etc.
    wrangle_blab_participants()
  return(participants)
}

#' Get the list of excluded participants from the blab_wide tracking sheet
#' 
#' @param 
#' @param 
#' 
#' @export
get_blab_excluded_participants <- function() {
  participants_path <- file.path(get_blab_share_path(), "experimental_projects/participants.xlsx")
  
  exclusion <- readxl::read_xlsx(participants_path, sheet = "Excluded") 
  return(exclusion)
}
