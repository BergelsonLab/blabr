library(tidyverse)

cdi_get_words <- function(data, cdi_type = "wg") {

  data <- if (cdi_type == "wg" | cdi_type == "WG") {
    dplyr::select(data, subject_id, completed, `baa baa`:some)
  } else {
    dplyr::select(data, subject_id, completed, `baa baa`:then)
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

CDI_scores <- read_csv("test_data/test_cdi_ws.csv")


attempt <- cdi_get_words(CDI_scores, cdi_type = "WS")
data <- attempt

get_vocab_score <- function(data, cdi_type, remove_incomplete = T) {


  if (remove_incomplete == T){
    data <- data %>%
      filter(completed == T)}

  data <- data %>%
    gather(key = word, value = value, 3:ncol(data)) %>%
    mutate(value = as.factor(value)) %>%
    rename(SubjectNumber = subject_ID) %>%
    group_by(SubjectNumber, value) %>%
    tally()

  if (cdi_type == "wg" | cdi_type == "WG") {
    data <- data %>%
      mutate(value = fct_recode(value,
                                "understands"="understands",
                                "produces" = "produces",
                                "neither" = "NA"))
  } else {
    data <- data %>%
      mutate(value = fct_recode(value,
                                "understands"="understands",
                                "neither" = "NA"))
  }

  data <- data %>%
    spread(value, n, fill=0)

  if (cdi_type == "wg" | cdi_type == "WG") {
    data <- data %>%
      mutate(CDIcomp = understands+produces,
             CDIprod = produces) %>%
      select(SubjectNumber, CDIcomp, CDIprod)
  } else {
    data <- data %>%
      select(SubjectNumber, produces) %>%
      rename(CDIprod = produces)
  }
  return(data)

}

attempt2 <- get_vocab_score(attempt, cdi_type = "WS", remove_incomplete = F)
