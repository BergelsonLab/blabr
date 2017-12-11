library(entropy)

basic_level_tested_words <- c("baby","babe","baby+doll", "ball","bally",
                              "blanket","blankey","blanky", "book","books",
                              "bottle","baba","ba", "car", "car+car","diaper",
                              "diape","diapey","diapers","diatee","didey","diadey",
                              "foot","footsy","footy","feet","feetsie","footsie",
                              "feetsy","feety", "hair","hairs", "hand", "juice",
                              "juices","juice+box","juice+boxes","juicey",
                              "milk","milkies","milky","milk+water","milk+jug",
                              "milks", "mouth", "nose","nosey", "spoon","spoony",
                              "stroller")


add_chi_noun_onset <- function(x) {
  x%>%
    filter(speaker == "CHI")%>%
    group_by(subj)%>%
    summarise(noun_chi_onset = min(as.numeric(as.character(month))))%>%
    right_join(x)
}

malformed_speaker_codes <- function(x) {
  x %>% filter(nchar(as.character(speaker))!=3)
}


count_experimentwords <- function(x) {
  x %>%
    filter(basic_level %in% basic_level_tested_words) %>%
    group_by(subj, month, audio_video) %>%
    summarise(num_exp_tokens = n(),
              num_exp_types = n_distinct(basic_level))
}

count_mot_fat <- function(x) {
  x %>%
    filter(speaker %in% c("MOT","FAT"))%>%
    group_by(subj, month, audio_video, speaker)%>%
    tally()%>%
    spread(speaker, n)
}

count_utterance <- function(x) {
  x %>%
    filter(utterance_type %in%c("d","i","q","r","s","n"))%>%
    group_by(subj, month, audio_video, utterance_type)%>%
    tally()%>%
    spread(utterance_type, n)
}

count_object_present <- function(x) {
  x %>%
    filter(object_present %in% c("n","y"))%>%
    group_by(subj, month, audio_video, object_present)%>%
    tally()%>%
    spread(object_present, n)%>%
    mutate(prop_op = y/(n+y))%>%
    rename(y_op = y,
           n_op = n)
}

object_present_exp <- function(x) {
  x %>%
    filter(basic_level %in% basic_level_tested_words &
           object_present %in% c("n","y"))%>%
    group_by(subj, month, audio_video, object_present)%>%
    tally()%>%
    spread(object_present, n)%>%
    mutate(prop_op_exp = y/(n+y))%>%
    rename(y_op_exp = y,
           n_op_exp = n)
}

count_device_and_toy <- function(x) {
  x %>%
    filter(speaker %in% c("TOY","TVN","TVM","TVS","TVB"))%>%
    group_by(subj, month, audio_video, speaker)%>%
    tally()%>%
    spread(speaker, n)
}

count_chi <- function(x) {
  x %>%
    filter(speaker %in% c("CHI"))%>%
    group_by(subj, month, audio_video, speaker)%>%
    tally()%>%
    spread(speaker, n)
}


count_chi_types <- function(x) {
  x %>%
    filter(speaker %in% c("CHI"))%>%
    group_by(subj, month, audio_video)%>%
    dplyr::select(subj, month, basic_level)%>%
    distinct(basic_level)%>%
    tally()%>%
    rename(CHItypes = n)
}

chi_noun_onset <- function(x) {
  x %>%
    dplyr::select(subj, noun_chi_onset)%>%
    distinct()
}


big_aggregate <- function(x, output=NULL) {
  fat_mot_count <- count_mot_fat(x)
  num_experimentwords <- count_experimentwords(x)
  six_to_seventeen_home_utt_count <- count_utterance(x)
  six_to_seventeen_home_device_count <- count_device_and_toy(x)
  six_to_seventeen_home_op <- count_object_present(x)
  six_to_seventeen_home_op_exp <- object_present_exp(x)
  six_to_seventeen_home_chi_count <- count_chi(x)
  six_to_seventeen_home_chi_type_count <- count_chi_types(x)
  six_to_seventeen_home_noun_chi_onset <- chi_noun_onset(x)

  big_df <- x %>%
              group_by(subj, month, SubjectNumber, audio_video)%>%
              summarise(numspeakers = n_distinct(speaker),
                        numtokens = n(),
                        numtypes = n_distinct(basic_level))%>%
              left_join(fat_mot_count)%>%
              left_join(num_experimentwords)%>%
              left_join(six_to_seventeen_home_utt_count)%>%
              left_join(six_to_seventeen_home_device_count)%>%
              left_join(six_to_seventeen_home_op)%>%
              left_join(six_to_seventeen_home_op_exp)%>%
              left_join(six_to_seventeen_home_chi_count)%>%
              left_join(six_to_seventeen_home_chi_type_count)%>%
              mutate_each(funs(replace(., which(is.na(.)), 0)))%>%
              group_by(subj, month, SubjectNumber, audio_video)%>%
              mutate(prop_mom = MOT/numtokens,
                     prop_dad = FAT/numtokens,
                     prop_parent = prop_mom+prop_dad,
                     prop_tech = (TVN+TVS+TVM+TOY+TVB)/numtokens,
                     tech = (TVN+TVS+TVM+TOY+TVB),
                     propd = d/numtokens,
                     propi = i/numtokens,
                     propn = n/numtokens,
                     propq = q/numtokens,
                     propr = r/numtokens,
                     props = s/numtokens,
                     type_token_ratio = numtypes/numtokens,
                     exp_type_ratio = num_exp_types/numtypes,
                     exp_token_ratio = num_exp_tokens/numtokens,
                     ent_subj_av = entropy(c(d/numtokens,
                                             q/numtokens,
                                             s/numtokens,
                                             r/numtokens,
                                             n/numtokens,
                                             i/numtokens),unit = "log2"),
                     sum_prop_ut = round(sum(c(d/numtokens,
                                               q/numtokens,
                                               s/numtokens,
                                               r/numtokens,
                                               n/numtokens,
                                               i/numtokens)),2))%>%
              dplyr::select(-TVM, -TVS, -TVN, -TVB)%>%
              left_join(six_to_seventeen_home_noun_chi_onset)%>%
              mutate(posttalk =  ifelse(as.numeric(as.character(month))<noun_chi_onset|
                                          is.na(noun_chi_onset),F,T))

  if (!is.null(output)) {
    write.csv(big_df, output, row.names=FALSE)
  }

  return(big_df)
}




