# Meant to work with SEEDLingS CDI spreadsheet, but potentially useful for other CDI that lacks norms.
#' The norm conversion table was downloaded from [wordbank](https://github.com/langcog/wordbank-shiny/tree/main/apps/scoring/norms/percentiles/English%20Percentiles)

#' TODO: 
#'  Add documentations to everything
#'  Add comments to explain the methods for wrangling norms and the method for iteratively adding norms values 
#'  for each col
#'  maybe change load_cdi for wg and ws respectively
#'  maybe add option for giving col map for potential use in the future

#'
#'
#'@export
get_norm_path <- function(form, gender, metric) {
  if (form == "WG") {
    if (metric == "comp") {
      norm_path <- file.path(form, paste0(form, "comp_", gender, "_nohighcomp_1.csv"))
    } else {
      norm_path <- file.path(form, paste0(form, metric, "_", gender, "_1.csv"))
    }
  } else {
    norm_path <- file.path(form, paste0(form, metric, "_", gender, "_36_1.csv"))
  }
  
  norm_path <- system.file("extdata", norm_path, package = "blabr")
  return(norm_path)
}

#'
#'
#'@export
wrangle_norm <- function(form, gender, metric) {
  norm_path <- get_norm_path(form, gender, metric)
  df_raw <- readr::read_csv(norm_path, skip = 1, show_col_types = FALSE)
  norm_df <- df_raw %>% 
    dplyr::rename(percentile = age) %>%
    tidyr::pivot_longer(cols = -percentile, names_to = "age", values_to = metric, names_transform = as.integer) %>% 
    dplyr::mutate(form = form, gender = gender) %>%
    dplyr::select(form, gender, everything())
  
  return(norm_df)
}

#'
#'
#'@export  
load_cdi_norms <- function() {
  all_norm_path <- system.file("extdata", "cdi_norms_wg.rds", package = "blabr")
  if (file.exists(all_norm_path)) {
    message(paste0("Loading cached CDI norms from ", all_norm_path))
    all_norms <- readRDS(all_norm_path)
    return(all_norms)
  }
  
  metrics_wg <- c("phrases", "comp", "prod", "earlygest", "lategest", "totgest")
  metrics_ws <- c("prod", "wordforms", "wordend2", "M3L", "complex")
  
  genders <- c("girls", "boys", "both")
  
  all_norms <- list()
  
  for (gender in genders) {
    norm_by_gender <- data.frame()
    for (metric in metrics_wg) {
      wrangled = wrangle_norm("WG", gender, metric)
      if (nrow(norm_by_gender) == 0) {
        norm_by_gender <- wrangled
      } else {
        norm_by_gender <- dplyr::full_join(norm_by_gender, wrangled, by = c("form", "gender", "percentile", "age"))
      }
    }
    all_norms[[gender]] <- norm_by_gender
  }
  
  return(all_norms)
}

calculate_percentile <- function(df, norms, col, both) {
  if (both) {
    new_col_name <- paste(col, "percentile", sep = "-")
  } else {
    new_col_name <- paste(col, "percentile", "sex", sep = "-")
  }
  
  norms <- norms %>% 
    dplyr::select(age, score = all_of(col), percentile) %>% 
    dplyr::group_by(age, score) %>% 
    dplyr::slice_max(percentile, n = 1) %>%
    dplyr::arrange(age, score)
  
  df %>% 
    dplyr::mutate(.x = .data[[col]]) %>% 
    dplyr::left_join(
      norms, 
      by = dplyr::join_by(month == age, closest(.x >= score))
    ) %>% 
    dplyr::select(-.x, -score) %>% 
    dplyr::rename(!!new_col_name := percentile) # because new_col_name is a string, we have to "dequote" with `!!` and then assigned with ":="
}

#'
#'
#'@export
add_norms_to_cdi <- function(cdi_df) {
  all_norms <- load_cdi_norms()
  col_to_filename_key <- data.frame(
    metric = c("phrases", "comp", "prod", "earlygest", "lategest", "totgest"),
    col_name = c("phrases", "CDIcomp", "CDIprod", "early_gestures", "later_gestures", "total_gestures")
  ) # Because the names in the norm table is different from seedlings colnames
  name_mapping <- setNames(col_to_filename_key$metric, col_to_filename_key$col_name)
  
  norm_both <- all_norms[["both"]] %>% 
    dplyr::rename(any_of(name_mapping))
  
  cdi_with_percentile <- data.frame()
  
  for (gender in c("boys", "girls")) {
    if (gender == "girls") {
      cdi_df_by_gender <- cdi_df %>% 
        dplyr::filter(Child_gender == "Female") 
    } else {
      cdi_df_by_gender <- cdi_df %>% 
        dplyr::filter(Child_gender == "Male") 
    }
    norm_by_gender <- all_norms[[gender]] %>% 
      dplyr::rename(any_of(name_mapping))
    
    cdi_df_with_percentile_by_gender <- purrr::reduce(
      col_to_filename_key$col_name, 
      \(d, col) calculate_percentile(d, norm_by_gender, col, FALSE), 
      .init = cdi_df_by_gender
    )
    
    cdi_df_with_percentile_both <- purrr::reduce(
      col_to_filename_key$col_name, 
      \(d, col) calculate_percentile(d, norm_both, col, TRUE), 
      .init = cdi_df_with_percentile_by_gender
    )
    cdi_with_percentile <- dplyr::bind_rows(cdi_with_percentile, cdi_df_with_percentile_both)
  }
  
  return(cdi_with_percentile %>% dplyr::arrange(SubjectNumber))
}