load_tsv <- function(fixrep_path, val_guess_max = 100000){
  read_tsv(fixrep_path, na=character(), guess_max = val_guess_max)
}

fixations_report <- function(fixrep_path, val_guess_max = 100000, remove_unfinished=TRUE, remove_practice=TRUE){
  fix_report <- load_tsv(fixrep_path, val_guess_max)
  if (remove_unfinished){
    fix_report <- fix_report %>%
      filter(!is.na(order))
  }
  if(remove_practice){
    fix_report <- fix_report %>%
      filter(practice=="n")
  }

  return(fix_report)
}

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
    do(expandFixList(., binSize=binSize))

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
#FindLowData----
FindLowData <- function(gazeData,
                          subsetWin,
                          maxBins = NULL,
                          maxMissing = NULL,
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

#RemoveLowData ----
RemoveLowData <- function(gazeData,
                        subsetWin,
                        maxBins = NULL,
                        maxMissing = NULL,
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

#Outlier----
outlier <- function(cross_item_mean_proptcorrTT, num_sd=3) {
  (cross_item_mean_proptcorrTT >
     (mean(cross_item_mean_proptcorrTT) +
        num_sd*(sd(cross_item_mean_proptcorrTT))) |
     cross_item_mean_proptcorrTT <
     (mean(cross_item_mean_proptcorrTT) -
        num_sd*(sd(cross_item_mean_proptcorrTT))))
}

#expandFixList----
expandFixList <- function(d, binSize=20){
  timeBin<-(ceiling(d$CURRENT_FIX_START/binSize):ceiling(d$FixEnd/binSize))
  data.frame(timeBin=timeBin,FixationID=d$FixationID)
}

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
