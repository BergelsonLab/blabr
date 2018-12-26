load_tsv <- function(fixrep_path, val_guess_max = 100000){
  read_tsv(fixrep_path, na=character(), guess_max = val_guess_max)
}

object2string <- function(obj){
  # Gets string from name of the object, ex object2string(blop) returns "blop"
  deparse(substitue(obj))
}

string2object <- function(string_name, val){
  assign(string_name, val)
  # how to return it?
  # eval(parse(text = string_name))
}

characters_to_factors <- function(df){
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],
                                         as.factor)
  df
}


#################################################################################

fixations_report <- function(fixrep_path, val_guess_max = 100000, remove_unfinished=TRUE, remove_practice=TRUE){
  # load tsv file
  fix_report <- load_tsv(fixrep_path, val_guess_max)
  # remove incomplete studies
  if (remove_unfinished){
    fix_report <- fix_report %>%
      filter(!is.na(order))
  }
  # remove practice rows
  if(remove_practice){
    fix_report <- fix_report %>%
      filter(practice=="n")
  }

  return(fix_report) # necessary? or just `fix_report`?
}

#################################################################################

binifyFixations <- function(gaze, binSize=20, keepCols=c("Subject","TrialNumber","Target","T"), maxTime=NULL){
  #convert a list of fixations to bins
  #binSize determines the size of each bin in ms
  #keepCols determines which columns from the original data frame will show up in the output
  #	will no longer need fixation start and duration, nor fixation location coordinates
  #
  #maxTime can be used to cut down trial length
  #

  #need to know when fixations end
  if ("CURRENT_FIX_END" %in% names(gaze)) {
    gaze$FixEnd <- gaze$CURRENT_FIX_END
  } else {
    #compute end of fixation from start and duration
    gaze$FixEnd <- gaze$CURRENT_FIX_START + gaze$CURRENT_FIX_DURATION
  }
  #if maxTime is defined, do some trimming
  if (!is.null(maxTime)) {
    #drop all fixations that start after the maxTime
    gaze<-subset(gaze,CURRENT_FIX_START < maxTime)
    #trim fixation end times to be less than maxTime
    gaze$FixEnd[gaze$FixEnd>maxTime]<-maxTime
  }

  #make a fixation ID variable that is just the fixation number in the overall data frame
  gaze$FixationID <- 1:nrow(gaze)

  #  data <- ddply(idata.frame(gaze), .(FixationID), expandFixList, binSize=binSize) #this was edited on 1/21/15 to stop using ddply

  data <- gaze %>%
    group_by(FixationID) %>%
    do(expandFixList(., binSize=binSize)) %>%
    ungroup()

  #there is a border case in which two redundant bins can be generated
  #clean them up by keeping the second one
  data<-subset(data,timeBin[2:length(timeBin)]!=timeBin[1:(length(timeBin)-1)])

  #combine data
  #dataFull <- merge(data,gaze[,c(keepCols,"FixationID")],by="FixationID")
  dataFull <- left_join(data,gaze[,c(keepCols,"FixationID")],by="FixationID") #modified 5/12/16 to use join

  #add a variable with actual time instead of time bin
  dataFull$Time <- dataFull$timeBin*binSize

  return(dataFull)
}

#################################################################################
# find key press issues and create doc to correct them
keypress_issues <- function(data, study = "eye_tracking_study", practice_trials = c("p1", "p2", "p3", "p4"), output_dir = "../data/", out_csv = FALSE){ # or study = NULL and take data[:-4]
  keypress_issues <- data %>%
    filter(RT == -1 & !Trial %in% practice_trials) %>%
    group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget)%>%
    distinct(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, RT, AudioTarget)
  if (out_csv){
    write_csv(keypress_issues, paste(output_dir, "keypress_issues_", study, "_", str_replace_all(Sys.time(), ' ', '_'), ".csv", sep=''))
  }
  keypress_issues #return?
}

#################################################################################
# retrieve corrected kp OR retrieve correct late target onset // separate functions?
keypress_retrieved <- function(filename, drop_list = c("video_pop_time", "video_targetonset", "notes")){
  retrieved_kp <- read_excel(filename) %>%
    dplyr::select(-one_of(drop_list))
  retrieved_kp #return?
}

#################################################################################

get_mesrep <- function(mesrep_all, fixed_kp, final_columns = c("RECORDING_SESSION_LABEL", "CURRENT_MSG_TIME", "TRIAL_INDEX", "AudioTarget", "Trial")){

  mesrep_temp <- mesrep_all %>% #subset of mesrep_all
    dplyr::select(one_of(final_columns, "CURRENT_MSG_TEXT", "RT")) %>%
    mutate(RT = as.numeric(as.character(RT)),
           Trial=as.numeric(Trial))

  good_kp_mesrep <- mesrep_temp %>% # message reports corresponding to good key presses
    filter(CURRENT_MSG_TEXT == "EL_BUTTON_CRIT_WORD") %>%
    dplyr::select(one_of(final_columns))

  fixed_kp_mesrep <- mesrep_temp %>% # message reports corresponding to fixed key presses
    filter(CURRENT_MSG_TEXT=="PLAY_POP" & RT=="-1") %>%
    left_join(fixed_kp %>% filter(outcome=="FIX")) %>%
    dplyr::rename(PLAY_POP=CURRENT_MSG_TIME) %>%
    mutate(CURRENT_MSG_TIME = PLAY_POP+ms_diff) %>%
    dplyr::select(one_of(final_columns))

  mesrep <- fixed_kp_mesrep %>% # merge both reports
    bind_rows(good_kp_mesrep)

  mesrep # return?
}

#################################################################################

get_late_target_onset <- function(data, max_time = 6000, study = "eye_tracking_study", output_dir = "../data/", out_csv = FALSE){
  late_target_onset <- data %>%
    filter(CURRENT_MSG_TIME>max_time) %>%
    group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget) %>%
    distinct(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget)
  if(out_csv){
    write_csv(late_target_onset, paste(output_dir, "late_target_onset_", study, "_", str_replace_all(Sys.time(), ' ', '_'), ".csv", sep=''))
  }
  late_target_onset
}

#################################################################################
# retrieve corrected kp OR retrieve correct late target onset // separate functions? ## IDENTICAL TO KP FUNCTION
late_target_retrieved <- function(filename, drop_list = c("video_pop_time", "video_targetonset", "notes")){
  retrieved_late_target <- read_excel(filename) %>%
    dplyr::select(-one_of(drop_list))
  retrieved_late_target #return?
}

#################################################################################



get_windows <- function(fix_mes_age, bin_size = 20, nb_1 = 18, short_window_time = 2000, med_window_time = 3500, long_window_time = 5000){
  # TODO what is that 18 number, where is it coming from?
  short_window_lim = short_window_time/bin_size
  med_window_lim = med_window_time/bin_size
  long_window_lim = long_window_time/bin_size
  exclude <- fix_mes_age %>%
    mutate(Nonset = (timeBin-floor(TargetOnset/bin_size))*bin_size,
         lowest = (TargetOnset/bin_size)+nb_1, # TODO nb_1 used here only
         short_max = (TargetOnset/bin_size)+short_window_lim,
         med_max = (TargetOnset/bin_size)+med_window_lim,
         long_max = (TargetOnset/bin_size)+long_window_lim,
         prewin = factor(ifelse(Time <= TargetOnset, "Y", "N")),
         longwin = factor(ifelse((timeBin >= lowest &
                                    timeBin <= long_max),"Y", "N")),# this is a 367-5s window bc 5000/20 = 250
         whichwin_long = factor(ifelse(prewin == "Y","pre",
                                       ifelse(longwin == "Y", "long", "neither"))),
         medwin = factor(ifelse((timeBin >= lowest &
                                   timeBin <= med_max),"Y","N")),# this is a 367-3500ms window bc 3500/20 = 175
         whichwin_med = factor(ifelse(prewin == "Y","pre",
                                      ifelse(longwin == "Y", "med", "neither"))),
         shortwin = factor(ifelse((timeBin >= lowest &
                                     timeBin <= ((TargetOnset/bin_size)+short_window_lim)),"Y","N")),# this is a 367-2s window bc 2000/20 = 100
         whichwin_short = factor(ifelse(prewin=="Y","pre",
                                        ifelse(longwin=="Y","short","neither")))) %>%
    select(-lowest, -short_max, -med_max, -long_max)

  exclude
}

#################################################################################

#FindLowData----
FindLowData <- function(gazeData,
                          subsetWin,
                          # maxBins = NULL,
                          # maxMissing = NULL,
                          window_size = NULL,
                          nb_2 = 0,
                          binSize = 20,
                          propt = "propt",
                          timeBin = "timeBin",
                          Trial = "Trial",
                          SubjectNumber = "SubjectNumber") {
  # this function is for making sure there's at least X amount of data in a trial; there are two potential sources of missing data: 1) off screen 2) elsewhere, not in an interest area
  # gazeData is the dataset,
  # subsetWin is the column name that contains "Y" indicating that's the part in which we are making sure there's enough data,
  # maxBins is how many bins there could have been in the trial,
  # minLength is how much data is the minimum to keep the trial, (not arg)
  # maxMissing= in real time, how many ms of data need to be there

  #binSize is what size of bins the fixations were turned into, this will usually be 20ms,
  #propt is proportion of target looking,

  #timeBin is the (20 ms) bin the trial that each line is

  if (is.null(window_size)){
    if (subsetWin=="longwin"){
      window_size <- 5000
    } else if (subsetWin=="medwin"){
      window_size <- 3500
    } else if (subsetWin=="shortwin"){
      window_size <- 2000
    }
  }

  maxBins <- as.integer((window_size - nb_2)/binSize)
  maxMissing <- as.integer((window_size - nb_2) - ((window_size - nb_2)/3))


  gazeData2 <- gazeData %>%
    filter(gazeData[,subsetWin] == "Y")

  print(dim(gazeData2))
  #1) offscreen: those timebins don't exist with my version of binifyFixations so how many timebins
  # are there in relation to the maximum given the trial length?

  number_timebins <- gazeData2 %>%
    group_by(Trial, SubjectNumber) %>%
    tally() %>%
    mutate(bins = n) %>%
    dplyr::select(-n)%>%
    mutate(missing_bins = maxBins - bins)

  #2)elsewhere: let's see how many NAs we have for propt, our proportion of target looking

  elsewhere_bins <- gazeData2 %>%
    group_by(Trial, SubjectNumber) %>%
    tally(is.na(propt)) %>%
    mutate(elsewhere_bins = n) %>%
    dplyr::select(-n)

  # This is all the low data from either source

  lowdata_bins <- left_join(number_timebins, elsewhere_bins) %>%
    mutate(lowdata = missing_bins + elsewhere_bins) %>%
    mutate(missing_TF = lowdata >floor(maxMissing/binSize)) %>%
    dplyr::select(Trial, SubjectNumber, missing_TF)

  gazeData <- left_join(gazeData, lowdata_bins)

  message("new column missing_TF has been added. When T, the row has low data.")
  return(gazeData)
}

#################################################################################

#RemoveLowData ----
RemoveLowData <- function(gazeData,
                        subsetWin,
                        # maxBins = NULL,
                        # maxMissing = NULL,
                        window_size = NULL,
                        nb_2 = 0,
                        binSize = 20,
                        propt = "propt",
                        timeBin = "timeBin",
                        Trial = "Trial",
                        SubjectNumber = "SubjectNumber") {
  # this function is for making sure there's at least X amount of data in a trial; there are two potential sources of missing data: 1) off screen 2) elsewhere, not in an interest area
  #gazeData is the dataset, subsetWin is the column name that contains "Y" indicating that's the part in which we are making sure there's enough data,
  #maxBins is how many bins there could have been in the trial,
  #minLength is how much data is the minimum to keep the trial, (not arg)
  #maxMissing= in real time, how many ms of data need to be there

  #binSize is what size of bins the fixations were turned into, this will usually be 20ms,
  #propt is proportion of target looking,

  #timeBin is the (20 ms) bin the trial that each line is

  if (is.null(window_size)){
    if (subsetWin=="longwin"){
      window_size <- 5000
    } else if (subsetWin=="medwin"){
      window_size <- 3500
    } else if (subsetWin=="shortwin"){
      window_size <- 2000
    }
  }

  maxBins <- as.integer((window_size - nb_2)/binSize)
  maxMissing <- as.integer((window_size - nb_2) - ((window_size - nb_2)/3))

  gazeData2 <- gazeData %>%
    filter(gazeData[,subsetWin] == "Y")

  print(dim(gazeData2))
  #1) offscreen: those timebins don't exist with my version of binifyFixations so how many timebins
  # are there in relation to the maximum given the trial length?

  number_timebins <- gazeData2 %>%
    group_by(Trial, SubjectNumber) %>%
    tally() %>%
    mutate(bins = n) %>%
    dplyr::select(-n)%>%
    mutate(missing_bins = maxBins - bins)

  #2)elsewhere: let's see how many NAs we have for propt, our proportion of target looking

  elsewhere_bins <- gazeData2 %>%
    group_by(Trial, SubjectNumber) %>%
    tally(is.na(propt)) %>%
    mutate(elsewhere_bins = n) %>%
    dplyr::select(-n)

  # This is all the low data from either source

  lowdata_bins <- left_join(number_timebins, elsewhere_bins) %>%
    mutate(lowdata = missing_bins + elsewhere_bins) %>%
    mutate(missing_TF = lowdata >floor(maxMissing/binSize)) %>%
    dplyr::select(Trial, SubjectNumber, missing_TF)

  gazeData <- left_join(gazeData, lowdata_bins) %>%
    filter(missing_TF == F) %>%
    dplyr::select(-missing_TF)

  message("Low data rows have been removed. To identify them in a new column without removing them, use blabr::FindLowData.")
  return(gazeData)
}

#################################################################################

get_pairs <- function(data, study = "eye_tracking", output_dir = '../data/', out_csv = FALSE){
  res <- data %>%
    group_by(SubjectNumber) %>%
    distinct(Pair)
  if (out_csv){
    name <- paste(output_dir, "pairs_", study, "_", str_replace_all(Sys.time(), ' ', '_'), ".csv", sep='')
    print(name)
    write_csv(res, name)
  }
  res
}

#################################################################################

#Outlier----
outlier <- function(cross_item_mean_proptcorrTT, num_sd=3) {
  (cross_item_mean_proptcorrTT >
     (mean(cross_item_mean_proptcorrTT) +
        num_sd*(sd(cross_item_mean_proptcorrTT))) |
     cross_item_mean_proptcorrTT <
     (mean(cross_item_mean_proptcorrTT) -
        num_sd*(sd(cross_item_mean_proptcorrTT))))
}

#################################################################################

#expandFixList----
expandFixList <- function(d, binSize=20){
  timeBin<-(ceiling(d$CURRENT_FIX_START/binSize):ceiling(d$FixEnd/binSize))
  data.frame(timeBin=timeBin,FixationID=d$FixationID)
}

#################################################################################

#FindFrozenTrials----
FindFrozenTrials <- function(gazeData,
                             Trial,
                             SubjectNumber,
                             gaze) {

 gazeData <-  gazeData %>%
    group_by(SubjectNumber, Trial) %>%
    mutate(frozen = ifelse(length(levels(fct_explicit_na(gaze, na_level = "NA"))) == 1, T, F))

 message("Column added identifying trials where gaze stayed in one interest area for whole trial (frozen = T).")
 return(gazeData)
}

#################################################################################

#RemoveFrozenTrials-----
RemoveFrozenTrials <- function(gazeData,
                             Trial,
                             SubjectNumber,
                             gaze) {

  gazeData <-  gazeData %>%
    group_by(SubjectNumber, Trial) %>%
    mutate(frozen = ifelse(length(levels(fct_explicit_na(gaze, na_level = "NA"))) == 1, T, F)) %>%
    filter(frozen == F) %>%
    dplyr::select(-frozen)

  message("frozen trials have been removed. To identify them in a new column without removing them, use blabr::FindFrozenTrials.")
  return(gazeData)
}

# expandFixList <- function(d, binSize=20){
#   timeBin<-(ceiling(d$CURRENT_FIX_START/binSize):ceiling(d$FixEnd/binSize))
#   data.frame(timeBin=timeBin,FixationID=d$FixationID)
# }
