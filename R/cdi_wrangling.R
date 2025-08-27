cdi_get_words <- function(data, cdi_type = "wg") {

  data <- if (cdi_type == "wg" | cdi_type == "WG") {
    dplyr::select(data, subject_id, completed, `baa baa`:some)
  } else {
    dplyr::select(data, subject_id, completed, `baa baa`:then)
  }

  ifelse(cdi_type == "WG" | cdi_type == "wg", dict <- system.file("extdata", "English_WG_dictionary.csv", package = "blabr"),
         ifelse(cdi_type == "WS" |cdi_type == "ws", dict <- system.file("extdata", "English_WG_dictionary.csv", package = "blabr"),
                stop("This function does not support that CDI type. Did you mean WS or WG?")))

  dict <- dict %>%
    filter(item_type == "word")

  new_names <- c("subject_ID", "completed", as.character(dict$gloss)) %>%
    str_replace_all(" ", "_")

  names(data) <- as.character(new_names)

  return(data)
}

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
      dplyr::mutate(
        value = forcats::fct_recode(
          value,
          "understands" = "understands",
          "produces" = "produces",
          "neither" = "NA"
        )
      )
  } else {
    data <- data %>%
      dplyr::mutate(value = forcats::fct_recode(value,
                                                "understands" = "understands",
                                                "neither" = "NA"))
  }

  data <- data %>%
    tidyr::spread(value, n, fill=0)

  if (cdi_type == "wg" | cdi_type == "WG") {
    data <- data %>%
      dplyr::mutate(CDIcomp = understands+produces,
             CDIprod = produces) %>%
      dplyr::select(SubjectNumber, CDIcomp, CDIprod)
  } else {
    data <- data %>%
      dplyr::select(SubjectNumber, produces) %>%
      dplyr::rename(CDIprod = produces)
  }
  return(data)

}
