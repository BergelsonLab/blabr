#' 
#' 
#' @return 
#' @export
#' 
wrangle_word_exposure <- function(df, with_metadata=FALSE, with_notes=FALSE, check_value=TRUE) {
  clean_col_df <- df %>% 
    dplyr::select(ResponseId, starts_with("Q")) %>% 
    janitor::row_to_names(1) %>%
    .[-c(1,2), ] 
  
  metadata_df <- df %>% 
    dplyr::select(ResponseId, !starts_with("Q")) %>% 
    janitor::row_to_names(1) %>%
    .[-c(1,2), ] %>% 
    janitor::clean_names()
  
  wrangled_df <- clean_col_df %>% 
    dplyr::select(`Response ID`, `Subject number`, `Date`, `Experimenter Initials`, tidyselect::matches("(?:\\bRating\\b|\\bNotes\\b)")) %>%
    dplyr::rename_with(~ str_replace(., "Vocabulary exposure - ", "")) %>% 
    dplyr::rename_with(~ str_replace(., " - Notes", "_notes")) %>% 
    dplyr::rename_with(~ str_replace(., " - Rating", "")) %>% 
    janitor::clean_names()
  
  if (!with_notes) {
    wrangled_df <- wrangled_df %>% 
      dplyr::select(-tidyselect::ends_with("_notes"))
  }
  if (with_metadata) {
    wrangled_df <- wrangled_df %>% 
      left_join(metadata_df, by = "response_id")
  }
  
  return(wrangled_df)
}

#' 
#' 
#' @return 
#' @export
#' 
get_word_exposure_wordlist <- function(df) {
  clean_df <- wrangle_word_exposure(df) 
  
  word_list <- wrangled_df %>% 
    dplyr::select(-response_id, -subject_number, -date, -experimenter_initials, -tidyselect::ends_with("_notes"), ) %>% 
    colnames()
  return(word_list)
}