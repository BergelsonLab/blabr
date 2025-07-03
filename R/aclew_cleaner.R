clean_aclew_input <- function (df) {
cleaned_input_utts <- df %>%
  mutate(VIHI_ID = substr(VIHI_ID, 1, 10)) %>% #Clip off the ".eaf" so that it'll match with the demographics file
  get_match_number() %>% #Get triplet and CI/HA/CA from demographics file
  filter(!is.na(Role)) %>% #If there's no role, the speaker is not a CI/HA/CA kid. They're probably a TD VI match inadvertently included here
  filter(speaker != "EE1", speaker != "CHI") %>% # remove CHI utts and electronic speech
  mutate(
    utterance_clean = str_replace_all(utterance, "&=\\w+", ""), # Remove substrings starting with &=
    utterance_clean = str_replace_all(utterance_clean, "@c", ""), #Remove the "@c" at the ends of made-up words
    utterance_clean = str_replace_all(utterance_clean, "<.*>\\s?\\[:\\s?(.*)\\]", "\\1"), #Replace "<cuz> [: because]" with "because"
    utterance_clean = str_replace_all(utterance_clean, "<(.*)>\\s?\\[=!\\s?.*\\]", "\\1"), #Replace "<lala> [=! sings]" with "lala"
    utterance_clean = str_replace_all(utterance_clean, "\\[-\\s[a-z]{3}\\]", ""), # Remove language tags like "[- spa]" or "[- ger]"
    utterance_clean = str_replace_all(utterance_clean, "[[:punct:]&&[^']]", ""), # Remove any punctuation, except for apostrophes
    utterance_clean = str_replace_all(utterance_clean, "xxx", ""), # Remove "xxx"
    utterance_clean = str_replace_all(utterance_clean, "\\s+$", "") # Remove trailing whitespace
  ) %>%
  filter(utterance_clean != "")

return(cleaned_input_utts)
}

