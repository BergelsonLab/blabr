#' Clean up raw CDI output (item + summary) for any project administered through 
#' WebCDI.
#' 
#' @param filepath Dataframe of the csv file downloaded from WebCDI 
#' (you should use the csv with both item level and sumary data)
#' @param form Which kind of cdi form is this (`WG` or `WS`). 
#' Will only include columns from each form.
#' @param table Which subset of the output to include? 
#' All table include id-related columns: "study_name", "subject_id", "repeat_num".
#' - "summary" (default): each row is one cdi form, including aggregated score 
#' for each form (e.g. total words produced, total number of first gestures, etc.).
#' - "wordlevel": each row is an item on one cdi form, with an "item" column 
#' for the name of the item and a "response" column.
#' - "raw": this is the rawest form of the data as downloaded from webcdi, with 
#' id-related columns added, each row is one cdi form, including columns for 
#' each item and columns for aggregated score, 
#' as well as metadata columns (e.g. date created, webcdi link, admin id, etc.). 
#' Is not affected by `withDemographic` and `justWord` parameters.
#' @param withDemographic Should the data include answers to demographic questions 
#' (e.g. sibling count, birth order, caregive info, etc.)? These columns are not 
#' treated as item, so if you select `wordlevel_long` table, the demographic 
#' answers will be included in every row. If you select `raw` table, demographic 
#' columns will always be included. `FALSE` is default. 
#' @param justWord Should the data include only vocabulary checklist item? If 
#' `TRUE` (default), returns only `Words Produced` and `Words Understood` related 
#' columns for `summary` table and vocabulary items for `wordlevel` table. If 
#' `FALSE`, will include gestures-related items for `WG` form and sentence-related 
#' items for `WS` form. If you select `raw` table, all items will be included 
#' regardless of this variable. 
#' @param rename For `WS` form, do we standardize the name of the 
#' word-related summary item to be the same as `WG` form. If `TRUE` (default), the 
#' three summary columns in WS named `Total Produced`, `Total Produced Percentile-sex`, 
#' and `Total Produced Percentile-both` will  changed to `Words Produced`, 
#' `Words Produced Percentile-sex` and `Words Produced Percentile-both`. Otherwise, 
#' they will remain unchanged
#' 
#' @return A dataframe of the wrangled CDI output according to the parameters
#' @export
#' 
wrangle_web_cdi <- function(cdi_df,
                            form = c("WG", "WS"),
                            table = c("summary", "wordlevel", "raw"),
                            new_cols = NULL,
                            withDemographic = FALSE,
                            justWord = TRUE,
                            rename = TRUE) {
  form <- match.arg(form)
  table <- match.arg(table)
  
  wg_key <- readr::read_csv(system.file("extdata", "English_WG_dictionary.csv", package = "blabr"))
  ws_key <- readr::read_csv(system.file("extdata", "English_WS_dictionary.csv", package = "blabr"))
  wg_cols <- wg_key$item
  ws_cols <- ws_key$item
  
  item_cols <- union(wg_cols, ws_cols)
  summary_cols <- union(wg_summary_cols, ws_summary_cols) # these are defined in in_cdi.R
  
  all_cols <- colnames(cdi_df)
  id_cols <- c("study_name", "subject_id", "repeat_num")
  if (!is.null(new_cols)) {
    id_cols <- c(id_cols, new_cols)
  }
  admin_cols <- c("opt_out", "local_lab_id", "administration_id", "link", "completed", "completedBackgroundInfo", "due_date", "last_modified", "created_date", "completed_date", "event_id")
  demographic_cols <- all_cols[!all_cols %in% c(id_cols, admin_cols, summary_cols, item_cols)]
  
  if (withDemographic) {
    cols_to_keep <- c(id_cols, demographic_cols)
  } else {
    cols_to_keep <- id_cols
  }
  
  if (justWord) {
    wg_cols <- wg_key %>% 
      dplyr::filter(item_type == "word") %>% 
      dplyr::pull(item)
    ws_cols <- ws_key %>% 
      dplyr::filter(item_type == "word") %>% 
      dplyr::pull(item)
    wg_summary_cols <- wg_summary_cols[stringr::str_detect(wg_summary_cols, "(Produced)|(Understood)")]
    ws_summary_cols <- ws_summary_cols[stringr::str_detect(ws_summary_cols, "(Produced)|(Understood)")]
  }
  
  # decide for each form
  if (form == "WG") {
    item_cols <- wg_cols
    summary_cols <- wg_summary_cols
  } else if (form == "WS") {
    if (rename) {
      cdi_df <- cdi_df %>% 
        dplyr::rename(
          `Words Produced` = `Total Produced`, 
          `Words Produced Percentile-sex` = `Total Produced Percentile-sex`, 
          `Words Produced Percentile-both`= `Total Produced Percentile-both`
        ) # so we have the same name for form WG and WS
    }
    item_cols <- ws_cols
    summary_cols <- ws_summary_cols
  } 
  
  if (table != "wordlevel") {
    if (table == "summary") {
      cols_to_keep <- c(cols_to_keep, summary_cols)
    } else {
      cols_to_keep <- all_cols
    }
    cdi_df <- cdi_df %>% 
      dplyr::select(all_of(cols_to_keep))
    
  } else {
    cdi_df <- cdi_df %>% 
      dplyr::select(c(cols_to_keep, item_cols)) %>% 
      tidyr::pivot_longer(cols = all_of(item_cols), names_to = "item", values_to = "response")
  }
  
  return(cdi_df)
}

#' Load VIHI cdi output from BLAB_SHARE (server will need to be mounted)
#' @inheritParams wrangle_web_cdi
#' @param population Which group of participants to include (`VI`, `HI`, or 
#' `VIHI` (which will include both))?
#' @param form Which kind of cdi forms to include (`WG`, `WS`, or `both`)? 
#' The data will only include items that are featured on said form.
#' 
#' @export
#' 
get_vihi_cdi <- function(population = c("VIHI", "VI", "HI", "TD"),
                         form = c("all", "WG", "WS"),
                         table = c("summary", "wordlevel", "raw"),
                         withDemographic = FALSE,
                         justWord = TRUE) {
  population <- match.arg(population)
  form <- match.arg(form)
  table <- match.arg(table)
  
  cdi_path <- file.path(get_blab_share_path(), "VIHI/Surveys/CDI/VIHI/VIHI_CDI_wordlevel.csv")
  cdi_full <- readr::read_csv(cdi_path) %>% 
    dplyr::mutate(Population = stringr::str_sub(VIHI_ID, 1, 2)) 
  
  vihi_id_key <- cdi_full %>%
    dplyr::select(VIHI_ID, Population, Form, exact_age, subject_id, repeat_num, study_name)

  cdi_wg <- cdi_full %>%
    dplyr::filter(Form == "WG") %>%
    dplyr::select(-VIHI_ID, -Population, -Form, -exact_age)

  cdi_ws <- cdi_full %>%
    dplyr::filter(Form == "WS") %>%
    dplyr::select(-VIHI_ID, -Population, -Form, -exact_age)

  cdi_wg_wrangled <- wrangle_web_cdi(cdi_wg,
                                     form = "WG",
                                     table = table,
                                     withDemographic = withDemographic,
                                     justWord = justWord,
                                     rename = FALSE) %>%
    dplyr::mutate(Form = "WG") %>%
    dplyr::left_join(vihi_id_key)


  cdi_ws_wrangled <- wrangle_web_cdi(cdi_ws,
                                     form = "WS",
                                     table = table,
                                     withDemographic = withDemographic,
                                     justWord = justWord,
                                     rename = FALSE) %>%
    dplyr::mutate(Form = "WS") %>%
    dplyr::left_join(vihi_id_key)

  if (form == "all") {
    final_cdi <- dplyr::bind_rows(cdi_wg_wrangled, cdi_ws_wrangled)
  } else if (form == "WS") {
    final_cdi <- cdi_ws_wrangled
  } else {
    final_cdi <- cdi_wg_wrangled
  }

  if (population != "VIHI") {
    final_cdi <- final_cdi %>%
      dplyr::filter(Population == population)
  }

  final_cdi <- final_cdi %>%
    dplyr::select(-study_name) %>%
    dplyr::select(VIHI_ID, Population, Form, exact_age, everything())

  return(final_cdi)
}

#' Load CDI output from all RO1 studies and SemPhonD
#' 
#' @inheritParams wrangle_web_cdi
#' @param version version tag to checkout
#' 
#' @export
#'
get_r01_cdi <- function(table = c("summary", "wordlevel", "raw"),
                        withDemographic = FALSE,
                        justWord = TRUE,
                        version=NULL) {
  table <- match.arg(table)
  new_cols = c("unique_cdi_id")
  
  ro1_cdi <- get_df_file('r01_cdi_spreadsheet', "all_cdi.csv",
              version = version)
  
  cdi_wg <- ro1_cdi %>%
    dplyr::filter(form == "WG") %>%
    dplyr::select(-form) %>% 
    wrangle_web_cdi(form = "WG",
                    new_cols = new_cols,
                    table = table,
                    withDemographic = withDemographic,
                    justWord = justWord,
                    rename = FALSE) %>%
    dplyr::mutate(form = "WG")
  
  cdi_ws <- ro1_cdi %>%
    dplyr::filter(form == "WS") %>%
    dplyr::select(-form) %>% 
    wrangle_web_cdi(form = "WS",
                    new_cols = new_cols,
                    table = table,
                    withDemographic = withDemographic,
                    justWord = justWord,
                    rename = FALSE) %>%
    dplyr::mutate(form = "WS")
  
  final_cdi <- dplyr::bind_rows(cdi_wg, cdi_ws) %>% 
    dplyr::select(study_name, subject_id, repeat_num, form, unique_cdi_id, dplyr::everything())
  
}

#' Select all word item columns for a cdi spreadsheet
#'
#' @param data a dataframe of the original cdi csv
#' @param cdi_type Either wg or ws
#'
#' @return New dataframe containing only vocabulary item column
#' 
#' @export
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

#' Calculate the vocabulary checklist score of a cdi spreadsheet
#'
#' @param data a dataframe of the original cdi csv
#' @param cdi_type Either wg or ws
#' @param remove_incomplete whether to remove any incomplete cdi forms
#'
#' @return New dataframe with vocab score
#' @export
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
