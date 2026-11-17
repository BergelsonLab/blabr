#' 
#' 
#' @return 
#' @export
#' 
wrangle_word_exposure <- function(df) {
  wrangled_df <- df %>% 
    dplyr::select(starts_with("Q")) %>% 
    janitor::row_to_names(1) %>%
    .[-c(1,2), ] %>% 
    dplyr::select(`Subject number`, contains("Rating")) %>% 
    dplyr::rename(subject = `Subject number`) %>%
    dplyr::rename_with(~ str_replace(., "Vocabulary exposure - ", "")) %>% 
    dplyr::rename_with(~ str_replace(., " - Rating", "")) %>% 
    dplyr::mutate(across(-subject, as.numeric)) %>% 
    janitor::clean_names()
  
  return(wrangled_df)
}