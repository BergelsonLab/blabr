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

removeLowData <- function(gazeData=NULL,
                          subsetData=NULL,
                          maxBins=NULL,
                          maxMissing=NULL,
                          binSize=20,
                          propt="propt",
                          timeBin="timeBin",
                          TrialNumber="TrialNumReal",
                          SubjectNumber="SubjectNumber"){
  # this function is for making sure there's at least X amount of data in a trial; there are two potential sources of missing data: 1) off screen 2) elsewhere, not in an interest area
  #gazeData is the dataset, subsetData is the column name that contains "Y" indicating that's the part in which we are making sure there's enough data,
  #maxBins is how many bins there could have been in the trial,
  #minLength is how much data is the minimum to keep the trial, (not arg)
  #maxMissing= in real time, how many ms of data need to be there

  #binSize is what size of bins the fixations were turned into, this will usually be 20ms,
  #propt is proportion of target looking,

  #timeBin is the (20 ms) bin the trial that each line is

  gazeData2 <- gazeData[gazeData[,subsetData]=="Y",] #can't use regular subset notation, though could use subset(df, eval(as.name(col)) == "Y")
  gazeData2 <- as.data.frame(gazeData2)

  #1) offscreen: those timebins don't exist with my version of binifyFixations so how many timebins are there in relation to the maximum given the trial length?
  number_timebins <- tapply(gazeData2 $timeBin, list(gazeData2 $TrialNumber, gazeData2 $SubjectNumber),length)
  missing_bins <- maxBins-number_timebins

  #2)elsewhere: let's see how many NAs we have for propt, our proportion of target looking

  gazeData2 $missing<-NA
  gazeData2 $missing[is.na(gazeData2 $propt)==T]<-1
  gazeData2 $missing[is.na(gazeData2 $propt)==F]<-0

  elsewhere_bins <- tapply(gazeData2 $missing, list(gazeData2 $TrialNumber, gazeData2 $SubjectNumber), sum)

  missing_bins+elsewhere_bins->lowdata_bins # lowdata_bins is the number of missing bins for every trial for every subject, if what's missing is more than minLength ms, this will be True
  missing_TF <- ((lowdata_bins)>floor(maxMissing/20))
  missing_TF <- as.data.frame(missing_TF)

  TrialNumber <- rownames(missing_TF)
  missing_TF <- cbind(TrialNumber, missing_TF)
  #okay now we have a df where "False" means it's fine, "NA" means no data for that trial bc the subject ended before the study was over, and "True" means too much missing data

  missingTF <- gather(missing_TF, TrialNumber)
  names(missingTF) <- c("TrialNumber", "SubjectNumber", "lowdata")


  #and merge it back onto our dataframe
  gazeData <- merge(gazeData, missingTF, by=c("SubjectNumber","TrialNumber"))

  #subset to the part without lowdata, i'm commenting this out for now

  return(gazeData)
}

outlier <- function(cross_item_mean_proptcorrTT) {
  (cross_item_mean_proptcorrTT >
     (mean(cross_item_mean_proptcorrTT) +
        3*(sd(cross_item_mean_proptcorrTT))) |
     cross_item_mean_proptcorrTT <
     (mean(cross_item_mean_proptcorrTT) -
        3*(sd(cross_item_mean_proptcorrTT))))
}

