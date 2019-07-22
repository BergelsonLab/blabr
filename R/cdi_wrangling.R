library(tidyverse)

CDI_scores <- read_csv("test_data/test_cdi_data.csv")

rename_cdi_cols <- function(data, cdi_type = "wg") {

  data <- if (cdi_type == "wg" | cdi_type == "WG") {
    dplyr::select(data, subject_id, completed, item_34:item_429)
  } else {
    dplyr::select(data, subject_id, completed, item_1:item_680)
  }

  ifelse(cdi_type == "WG" | cdi_type == "wg", dict <- read_csv("CDI_dict/English_WG_dictionary.csv"),
         ifelse(cdi_type == "WS" |cdi_type == "ws", dict <- read_csv("CDI_dict/English_WS_dictionary.csv"),
                stop("This function does not support that CDI type. Did you mean WS or WG?")))

  dict <- dict %>%
    filter(item_type == "word")

  new_names <- c("subject_ID", "completed", as.character(dict$gloss)) %>%
    str_replace_all(" ", "_")

  names(data) <- as.character(new_names)

  return(data)
}

attempt <- rename_cdi_cols(CDI_scores, cdi_type = "ws")

get_vocab_score <- function(data, cdi_type) {

}


