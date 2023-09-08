add_chi_noun_onset <- function(x) {
  x %>%
    dplyr::filter(speaker == "CHI")%>%
    dplyr::group_by(subj)%>%
    dplyr::summarise(noun_chi_onset = min(as.numeric(as.character(month)))) %>%
    dplyr::right_join(x)
}

malformed_speaker_codes <- function(x) {
  x %>% dplyr::filter(nchar(as.character(speaker))!=3)
}

count_mot_fat <- function(x) {
  print(colnames(x))
  x %>%
    dplyr::filter(speaker %in% c("MOT","FAT"))%>%
    dplyr::group_by(subj, month, audio_video, speaker)%>%
    dplyr::tally()%>%
    tidyr::spread(speaker, n)
}

count_utterance <- function(x) {
  x %>%
    dplyr::filter(utterance_type %in%c("d","i","q","r","s","n"))%>%
    dplyr::group_by(subj, month, audio_video, utterance_type)%>%
    dplyr::tally()%>%
    tidyr::spread(utterance_type, n)
}

count_object_present <- function(x) {
  x %>%
    dplyr::filter(object_present %in% c("n","y"))%>%
    dplyr::group_by(subj, month, audio_video, object_present)%>%
    dplyr::tally()%>%
    tidyr::spread(object_present, n)%>%
    dplyr::mutate(prop_op = y/(n+y))%>%
    dplyr::rename(y_op = y,
           n_op = n)
}

count_device_and_toy <- function(x) {
  x %>%
    dplyr::filter(speaker %in% c("TOY","TVS","TVM","TVS","TVB"))%>%
    dplyr::group_by(subj, month, audio_video, speaker)%>%
    dplyr::tally()%>%
    tidyr::spread(speaker, n)
}

count_chi <- function(x) {
  x %>%
    dplyr::filter(speaker %in% c("CHI"))%>%
    dplyr::group_by(subj, month, audio_video, speaker)%>%
    dplyr::tally()%>%
    tidyr::spread(speaker, n)
}


count_chi_types <- function(x) {
  x %>%
    dplyr::filter(speaker %in% c("CHI"))%>%
    dplyr::group_by(subj, month, audio_video)%>%
    dplyr::select(subj, month, basic_level)%>%
    dplyr::distinct(basic_level)%>%
    dplyr::tally()%>%
    dplyr::rename(CHItypes = n)
}

chi_noun_onset <- function(x) {
  x %>%
    dplyr::select(subj, noun_chi_onset)%>%
    dplyr::distinct()
}

#' Add aggregations columns into an all_basiclevel tibble
#'
#' @param x an all_basiclevel dataframe
#' @param exclude columns to exclude from the final output
#' @param output an output path to save the result to
#'
#' @return a tibble containing the all_basicalevel data
#' @export
#'
#' @examples
#' \dontrun{
#' # get all the aggregations
#' all_bl <- get_all_basiclevel()
#' big_agg_allbl <- big_aggregate(all_bl)
#'
#' # return everything except the 'type_token_ratio' and 'prop_dad' columns
#'
#' bigagg_allbl_reduced <- big_aggregate(all_bl, exclude=c('type_token_ratio', 'prop_dad'))
#' }

big_aggregate <- function(x, exclude = NULL, output = NULL, exclude_chi = FALSE) {
  fat_mot_count <- count_mot_fat(x)
  six_to_seventeen_home_device_count <- count_device_and_toy(x)
  six_to_seventeen_home_chi_count <- count_chi(x)
  six_to_seventeen_home_chi_type_count <- count_chi_types(x)
  six_to_seventeen_home_noun_chi_onset <- add_chi_noun_onset(x) %>% chi_noun_onset()

  if (exclude_chi == TRUE) {
    x <- x %>% dplyr::filter(speaker != "CHI")
  }

  six_to_seventeen_home_utt_count <- count_utterance(x)
  six_to_seventeen_home_op <- count_object_present(x)

  big_df <- x %>%
              dplyr::group_by(subj, month, SubjectNumber, audio_video)%>%
              summarise(numspeakers = n_distinct(speaker),
                        numtokens = n(),
                        numtypes = n_distinct(basic_level))%>%
              dplyr::left_join(fat_mot_count)%>%
              dplyr::left_join(six_to_seventeen_home_utt_count)%>%
              dplyr::left_join(six_to_seventeen_home_device_count)%>%
              dplyr::left_join(six_to_seventeen_home_op)%>%
              dplyr::left_join(six_to_seventeen_home_chi_count)%>%
              dplyr::left_join(six_to_seventeen_home_chi_type_count)%>%

              dplyr::mutate_each(funs(replace(., which(is.na(.)), 0)))%>%

              # dplyr::mutate_at(funs(replace(., which(is.na(.)), 0)))%>%

              dplyr::group_by(subj, month, SubjectNumber, audio_video)%>%
              dplyr::mutate(prop_mom = MOT/numtokens,
                     prop_dad = FAT/numtokens,
                     prop_parent = prop_mom+prop_dad,
                     tech_tokens = (TVS+TOY),
                     prop_tech = tech_tokens/numtokens,
                     propd = d/numtokens,
                     propi = i/numtokens,
                     propn = n/numtokens,
                     propq = q/numtokens,
                     propr = r/numtokens,
                     props = s/numtokens,
                     type_token_ratio = numtypes/numtokens,
                     ent_subj_av = entropy::entropy(c(d/numtokens,
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
              # dplyr::select(-TVS)%>%
              dplyr::left_join(six_to_seventeen_home_noun_chi_onset)%>%
              dplyr::mutate(posttalk =  ifelse(as.numeric(as.character(month))<noun_chi_onset|
                                          is.na(noun_chi_onset),F,T))

  if (!is.null(output)) {
    write.csv(big_df, output, row.names=FALSE)
  }

  if (!is.null(exclude)) {
    big_df = big_df[,!(names(big_df) %in% exclude)]
  }

  return(big_df)
}




