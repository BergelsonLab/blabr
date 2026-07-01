babar_cols_type <- readr::cols_only(
  filename=readr::col_character(),
  onset=readr::col_double(),
  offset=readr::col_double(),
  speaker=readr::col_factor(levels = c("KCHI", "FEM", "MAL", "OCH")),
  phonemes=readr::col_character(),
  syllables=readr::col_character(),
  n_syllables=readr::col_integer(),
  n_phonemes=readr::col_integer(),
  cv=readr::col_character(),
  n_canonical_syllables=readr::col_integer(),
  is_canonical=readr::col_logical()
)

sonority_dict <- readr::read_tsv(system.file("extdata", "sonority.tsv", package = "blabr"))

all_vowels <- sonority_dict %>%
  dplyr::filter(type == "vowel") %>%
  dplyr::pull(phoneme) %>%
  unique()

all_glides <- sonority_dict %>%
  dplyr::filter(category == "glide") %>%
  dplyr::pull(phoneme) %>%
  unique()

#'
#'
#'@param
#'
#'@return
#'@export
read_babar <- function(filepath, batch) {
  if (batch) {
    message(paste0("Reading all csv files in the following folder: ", filepath))
    babar_files <- list.files(
      path = filepath,
      pattern = "\\.csv$",
      full.names = TRUE
    )
    df <- babar_files %>%
      lapply(readr::read_csv, col_types = babar_cols_type) %>%
      dplyr::bind_rows()
  } else {
    df <- filepath %>%
      readr::read_csv(col_types = babar_cols_type)
  }

  df <- df %>%
    dplyr::mutate(
      recording_id = stringr::str_replace(filename, ".wav", ""),
      duration = offset - onset
    )
  return(df)
}

get_vowels_and_glides <- function() {
  return(list(vowels=all_vowels, glides=all_glides, all=sonority_dict))
}

pivot_to_phoneme <- function(df, minimum_count=50) {
  df_by_phoneme <- df %>%
    tidyr::drop_na(phonemes) %>%
    tidyr::separate_rows(phonemes, sep=" ") %>% # one row for every phoneme
    dplyr::left_join(sonority_dict, by = c("phonemes" = "phoneme")) %>%
    dplyr::group_by(recording_id, phonemes) %>%
    dplyr::mutate(phoneme_count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(phoneme_count >= minimum_count)

  return(df_by_phoneme)
}

#'
#'
#'@param
#'
#'@return
#'@export
get_consonant_inventory <- function(df, minimum_count=50) {
  df_by_phoneme <- pivot_to_phoneme(df, minimum_count=minimum_count)

  df_with_consonants <- df_by_phoneme %>%
    dplyr::filter(! category %in% c("vowel", "glide")) %>%
    dplyr::distinct(recording_id, phonemes) %>%
    dplyr::summarize(
      consonant_inventory = paste(phonemes, collapse = " "),
      n_consonants = n(),
      .by = recording_id
    )

  return(df_with_consonants)
}

#'
#'
#'@param
#'
#'@return
#'@export
get_inventory <- function(df, minimum_count=50) {
  df_by_phoneme <- pivot_to_phoneme(df, minimum_count=minimum_count)

  df_with_inventory <- df_by_phoneme %>%
    dplyr::distinct(recording_id, phonemes) %>%
    dplyr::summarize(
      inventory = paste(phonemes, collapse = " "),
      n_phonemes = n(),
      .by = recording_id
    )

  return(df_with_inventory)
}

#'
#'
#'@param
#'
#'@return
#'@export
get_canonical_metrics <- function(df) {
  df_with_metrics <- df %>%
    dplyr::mutate(is_canonical_with_na = ifelse(is.na(syllables), NA, is_canonical), 
                  n_syllables = ifelse(is.na(syllables), NA, n_syllables)
                  ) %>% 
    dplyr::group_by(recording_id) %>% 
    dplyr::summarise(
      n_utterances = n(),
      canonical_prop_utt = sum(is_canonical_with_na, na.rm = TRUE)/n(), 
      canonical_babbling_ratio = mean(is_canonical_with_na, na.rm = TRUE),
      n_syllables = sum(n_syllables, na.rm = TRUE),
      n_canonical_syllables = sum(n_canonical_syllables),
      canonical_prop_syl = sum(n_canonical_syllables) / n_syllables
    )
}

get_metrics_and_inventory <- function(df, minimum_count=50) {
  df_with_inventory <- get_inventory(df, minimum_count=minimum_count)
  df_with_consonant_inventory <- get_consonant_inventory(df, minimum_count=minimum_count)
  df_with_metrics <- get_canonical_metrics(df)

  final_df <- df_with_inventory %>%
    dplyr::left_join(
      df_with_consonant_inventory,
      by = "recording_id"
    ) %>%
    dplyr::left_join(
      df_with_metrics,
      by = "recording_id"
    )

  return(final_df)
}
