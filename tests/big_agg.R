library(blabr)
library(entropy)

big_aggregate <- function(x, output=NULL) {
  fat_mot_count <- count_mot_fat(x)
  num_experimentwords <- count_experimentwords(x)
  six_to_seventeen_home_utt_count <- count_utterance(x)
  six_to_seventeen_home_device_count <- count_device_and_toy(x)
  six_to_seventeen_home_op <- count_object_present(x)
  six_to_seventeen_home_op_exp <- object_present_exp(x)
  six_to_seventeen_home_chi_count <- count_chi(x)
  six_to_seventeen_home_chi_type_count <- count_chi_types(x)
  six_to_seventeen_home_noun_chi_onset <- add_chi_noun_onset(x) %>% chi_noun_onset()

  big_df <- x %>%
    dplyr::group_by(subj, month, SubjectNumber, audio_video)%>%
    summarise(numspeakers = n_distinct(speaker),
              numtokens = n(),
              numtypes = n_distinct(basic_level))%>%
    dplyr::left_join(fat_mot_count)%>%
    dplyr::left_join(num_experimentwords)%>%
    dplyr::left_join(six_to_seventeen_home_utt_count)%>%
    dplyr::left_join(six_to_seventeen_home_device_count)%>%
    dplyr::left_join(six_to_seventeen_home_op)%>%
    dplyr::left_join(six_to_seventeen_home_op_exp)%>%
    dplyr::left_join(six_to_seventeen_home_chi_count)%>%
    dplyr::left_join(six_to_seventeen_home_chi_type_count)%>%

    ############################################################### This is where things break
    dplyr::mutate_each(funs(replace(., which(is.na(.)), 0)))%>%
    ###############################################################
    # dplyr::mutate_at(funs(replace(., which(is.na(.)), 0)))%>% # potential fix?

    dplyr::group_by(subj, month, SubjectNumber, audio_video)%>%
    dplyr::mutate(prop_mom = MOT/numtokens,
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
    dplyr::left_join(six_to_seventeen_home_noun_chi_onset)%>%
    dplyr::mutate(posttalk =  ifelse(as.numeric(as.character(month))<noun_chi_onset|
                                       is.na(noun_chi_onset),F,T))

  if (!is.null(output)) {
    write.csv(big_df, output, row.names=FALSE)
  }

  return(big_df)
}



###########################################################
#  all_bl should be able to pass through big_aggregate()  #
###########################################################

all_bl <- get_all_basiclevel()
big_agg <- big_aggregate(all_bl)



