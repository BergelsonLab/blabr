babar_cols_type <- readr::cols_only(
  filename=readr::col_character(),
  onset=readr::col_double(),
  offset=readr::col_double(),
  # speaker=readr::col_factor(levels = c("KCHI", "FEM", "MAL", "OCH")),
  speaker=readr::col_character(),
  phonemes=readr::col_character(),
  syllables=readr::col_character(),
  n_syllables=readr::col_integer(),
  n_phonemes=readr::col_integer(),
  cv=readr::col_character()
)

sonority_dict <- readr::read_tsv(system.file("extdata", "sonority.tsv", package = "blabr"))

all_vowels <- sonority_dict %>%
  filter(type == "vowel") %>%
  pull(phoneme) %>%
  unique()

all_glides <- sonority_dict %>%
  filter(category == "glide") %>%
  pull(phoneme) %>%
  unique()

#'
#'
#'@param
#'
#'@return
#'@export
read_babar <- function(filepath, batch) {
  message("Reading babar file batch:")
  # add canonical_prop_utt, consonant_inventory,
  # phonetic_inventory, phoneme_entropy, syllable_entropy,
  # type_token_ratio, canonical_prop_syl
  if (batch) {
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

pivot_to_phoneme <- function(df) {
  df_by_phoneme <- df %>%
    tidyr::drop_na(phonemes) %>%
    tidyr::separate_rows(phonemes, sep=" ") %>% # one row for every phoneme
    dplyr::left_join(sonority_dict, by = c("phonemes" = "phoneme")) %>%
    dplyr::group_by(recording_id, phonemes) %>%
    dplyr::mutate(phoneme_count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(phoneme_count >= 20)

  return(df_by_phoneme)
}

#'
#'
#'@param
#'
#'@return
#'@export
get_consonant_inventory <- function(df) {
  df_by_phoneme <- pivot_to_phoneme(df)

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
get_inventory <- function(df) {
  df_by_phoneme <- pivot_to_phoneme(df)

  df_with_inventory <- df_by_phoneme %>%
    dplyr::distinct(recording_id, phonemes) %>%
    dplyr::summarize(
      inventory = paste(phonemes, collapse = " "),
      n_phonemes = n(),
      .by = recording_id
    )

  return(df_with_inventory)
}


