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
  
  no_notes <- wrangled_df %>% 
    dplyr::select(-tidyselect::ends_with("_notes")) 
  
  if (check_value) {
    item_cols <- get_word_exposure_wordlist(no_notes)
    convert_value_df <- check_for_non_numeric(no_notes, item_cols)
    if (nrow(convert_value_df) != 0) {
      warning("Found ", nrow(convert_value_df) , " rows with non-numeric values; returning unchanged results. Check the results of participants ", convert_value_df$subject_number, call. = FALSE)
    } else {
      wrangled_df <- wrangled_df %>% 
        dplyr::mutate(across(item_cols, as.numeric)) 
    }
  }
  
  if (!with_notes) {
    wrangled_df <- no_notes
  }
  if (with_metadata) {
    wrangled_df <- wrangled_df %>% 
      left_join(metadata_df, by = "response_id")
  }
  
  return(wrangled_df)
}

check_for_non_numeric <- function(no_notes_df, item_cols) {
  
  check_value_by_item <- function(score) {
    coerced <- suppressWarnings(as.numeric(score))
    !is.na(score) & is.na(coerced)
  }
  
  new_cols <- paste0(item_cols, "_check")
  
  convert_value_df <- purrr::reduce(
    item_cols, 
    \(d, col) dplyr::mutate(
      d, 
      "{col}_check" := check_value_by_item(.data[[col]]), 
      .after = tidyselect::all_of(col)), 
    .init = no_notes_df
  ) %>% 
    dplyr::filter(
      dplyr::if_any(
        tidyselect::all_of(new_cols),
        \(x) x # check for values that are TRUE
      )
    )
  
  return(convert_value_df)
}



#' 
#' 
#' @return 
#' 
get_word_exposure_wordlist <- function(wrangled_df) {
  word_list <- wrangled_df %>% 
    dplyr::select(-response_id, -subject_number, -date, -experimenter_initials, -tidyselect::ends_with("_notes")) %>% 
    colnames()
  return(word_list)
}