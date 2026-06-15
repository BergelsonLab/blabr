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

sonority_dict <- readr::read_tsv("./sonority.tsv")

#'
#'
#'@param
#'
#'@return
#'@export
read_babar <- function(filepath) {
  message("Reading babar file:")
  # add canonical_prop_utt, consonant_inventory, 
  # phonetic_inventory, phoneme_entropy, syllable_entropy, 
  # type_token_ratio, canonical_prop_syl
  df <- filepath %>% 
    readr::read_csv(col_types = babar_cols_type) %>% 
    dplyr::mutate(
      recording_id = stringr::str_replace(filename, ".wav", ""),
      duration = offset - onset
    )  
  return(df)
}



