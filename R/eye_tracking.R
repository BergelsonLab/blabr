load_tsv <- function(fixrep_path, val_guess_max = 100000){
  readr::read_tsv(fixrep_path, na=character(), guess_max = val_guess_max)
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

#' Convert all character columns to factors
#'
#' @param df Dataframe whose columns need conversion.
#'
#' @return Dataframe with all character columns converted to factors.
#' @export
characters_to_factors <- function(df){
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],
                                         as.factor)
  df
}

DEFAULT_WINDOWS_UPPER_BOUNDS <- list(short = 2000,
                                     med = 3500,
                                     long = 5000)


#################################################################################

#' (no docs yet) Read EyeLink fixation report file
#'
#' @param fixrep_path
#' @param val_guess_max 
#' @param remove_unfinished 
#' @param remove_practice 
#'
#' @return
#' @export
#'
#' @examples
fixations_report <- function(fixrep_path, val_guess_max = 100000, remove_unfinished=TRUE, remove_practice=TRUE){
  # load tsv file
  fix_report <- load_tsv(fixrep_path, val_guess_max)
  # remove incomplete studies
  if (remove_unfinished){
    fix_report <- fix_report %>%
      dplyr::filter(!is.na(order))
  }
  # remove practice rows
  if(remove_practice){
    fix_report <- fix_report %>%
      dplyr::filter(practice=="n")
  }

  return(fix_report) # necessary? or just `fix_report`?
}

#################################################################################

#' (no docs yet) Bin fixations
#'
#' @param gaze 
#' @param binSize 
#' @param keepCols 
#' @param maxTime 
#'
#' @return
#' @export
#'
#' @examples
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
    ungroup() %>%
    as.data.frame()#added by EB 8/7/20 bc the following line's subset breaks on tbls (!?)

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

#' (no docs yet) Find key press issues and create a doc to check and potentially correct them
#'
#' @param data 
#' @param study 
#' @param practice_trials 
#' @param output_dir 
#' @param out_csv 
#'
#' @return
#' @export
#'
#' @examples
keypress_issues <- function(data, study = "eye_tracking_study", practice_trials = c("p1", "p2", "p3", "p4"), output_dir = "../data/", out_csv = FALSE){ # or study = NULL and take data[:-4]
  keypress_issues <- data %>%
    dplyr::filter(RT == -1 & !Trial %in% practice_trials) %>%
    group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget)%>%
    distinct(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, RT, AudioTarget)
  if (out_csv){
    write_csv(keypress_issues, paste(output_dir, "keypress_issues_", study, "_", str_replace_all(Sys.time(), ' ', '_'), ".csv", sep=''))
  }
  keypress_issues #return?
}

#################################################################################
# retrieve corrected kp OR retrieve correct late target onset // separate functions?
# TODO: zh: Update/delete the comment. I think it is out of date and the functions have already been separated

#' (no docs yet) Load file with manually corrected key presses or late target onsets
#'
#' @param filename 
#' @param drop_list 
#'
#' @return
#' @export
#'
#' @examples
keypress_retrieved <- function(filename, drop_list = c("video_pop_time", "video_targetonset", "notes")){
  retrieved_kp <- read_excel(filename) %>%
    dplyr::select(-one_of(drop_list))
  retrieved_kp #return?
}

#################################################################################

#' (no docs yet) Create message report with corrected key presses
#'
#' @param mesrep_all 
#' @param fixed_kp 
#' @param final_columns 
#'
#' @return
#' @export
#'
#' @examples
get_mesrep <- function(mesrep_all, fixed_kp, final_columns = c("RECORDING_SESSION_LABEL", "CURRENT_MSG_TIME", "TRIAL_INDEX", "AudioTarget", "Trial")){

  mesrep_temp <- mesrep_all %>% #subset of mesrep_all
    dplyr::select(one_of(final_columns, "CURRENT_MSG_TEXT", "RT")) %>%
    mutate(RT = as.numeric(as.character(RT)),
           Trial=as.numeric(Trial))

  good_kp_mesrep <- mesrep_temp %>% # message reports corresponding to good key presses
    dplyr::filter(CURRENT_MSG_TEXT == "EL_BUTTON_CRIT_WORD") %>%
    dplyr::select(one_of(final_columns))

  fixed_kp_mesrep <- mesrep_temp %>% # message reports corresponding to fixed key presses
    dplyr::filter(CURRENT_MSG_TEXT=="PLAY_POP" & RT=="-1") %>%
    left_join(fixed_kp %>% dplyr::filter(outcome=="FIX")) %>%
    dplyr::rename(PLAY_POP=CURRENT_MSG_TIME) %>%
    mutate(CURRENT_MSG_TIME = PLAY_POP+ms_diff) %>%
    dplyr::select(one_of(final_columns))

  mesrep <- fixed_kp_mesrep %>% # merge both reports
    bind_rows(good_kp_mesrep)

  mesrep # return?
}

#################################################################################

#' (no docs yet) Find late target onsets and create a doc to check and potentially correct them
#'
#' @param data 
#' @param max_time 
#' @param study 
#' @param output_dir 
#' @param out_csv 
#'
#' @return
#' @export
#'
#' @examples
get_late_target_onset <- function(data, max_time = 6000, study = "eye_tracking_study", output_dir = "../data/", out_csv = FALSE){
  late_target_onset <- data %>%
    dplyr::filter(CURRENT_MSG_TIME>max_time) %>%
    group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget) %>%
    distinct(RECORDING_SESSION_LABEL, TRIAL_INDEX, Trial, AudioTarget)
  if(out_csv){
    write_csv(late_target_onset, paste(output_dir, "late_target_onset_", study, "_", str_replace_all(Sys.time(), ' ', '_'), ".csv", sep=''))
  }
  late_target_onset
}

#################################################################################
# retrieve corrected kp OR retrieve correct late target onset // separate functions? ## IDENTICAL TO KP FUNCTION
# TODO: zh: Update/delete the comment. I think it is out of date and the functions have already been separated

#' (no docs yet) Load file with manually corrected late target onsets
#'
#' @param filename 
#' @param drop_list 
#'
#' @return
#' @export
#'
#' @examples
late_target_retrieved <- function(filename, drop_list = c("video_pop_time", "video_targetonset", "notes")){
  retrieved_late_target <- read_excel(filename) %>%
    dplyr::select(-one_of(drop_list))
  retrieved_late_target #return?
}

#################################################################################


#' Assigns binned fixations to a short, medium, and long time windows
#'
#' @param fix_mes_age A fixations dataframe that is required to minimally contain these columns:
#'  - `TargetOnset` (numeric): Time of target onset in milliseconds.
#'  - `Time` (numeric): Time in ms (end of the bin).
#' @param bin_size Width of bins in milliseconds. Defaults to 20 ms.
#' @param nb_1 Number of the first bin to be considered as part of the windows. Defaults to 18.
#' @param short_window_time,med_window_time,long_window_time End timepoints of the short, medium, and long windows in milliseconds from the target word onset. Default to `r DEFAULT_WINDOWS_UPPER_BOUNDS$short`, `r DEFAULT_WINDOWS_UPPER_BOUNDS$med`, and `r DEFAULT_WINDOWS_UPPER_BOUNDS$long`, respectively.
#'
#' @details
#' Parameter `nb_1` defaults to 18, because it is the closest bin to 367 ms, which is the magic window onset from Fernald et al. (2008).
#'
#' A note on the window and bin boundaries. Let's use `nb_1 = 18` and the long window as an example. Both bins located exactly 360 ms and 5000 ms after the target onset will be counted as belonging to the long window (`longwin == "Y"`). In the case of the lower bound, this creates a slight inconsistency with the `Nonset` column: in most cases, the bin with `Nonset` equal to 360 ms will not belong to any windows, but in ~1/20 of the cases where the bin is exactly at 360 ms from the target onset, it will belong to all windows.
#'
#' @return The input dataframe with the following columns added:
#' - `prewin` (factor): Whether a time bin comes before the target onset. Level labels are "Y" and "N".
#' - `shortwin`, `medwin`, `longwin` (factor): Whether a time bin is in the short, medium, or long window, respectively. Level labels are "Y" and "N".
#' - `whichwin_short`, `whichwin_med`, `whichwin_long` (factor): Whether a time bin
#'   - is in the short, medium, or long window (level label "short", "medium", or "long", respectively),
#'   - comes before the target onset (level "pre"), or
#'   - neither (level "neither").
#' - `Nonset` (numeric): Time (in ms) from the target onset rounded up to the nearest bin.
#'
#' @export
get_windows <- function(
  fix_mes_age,
  bin_size = 20,
  nb_1 = 18,
  short_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$short,
  med_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$med,
  long_window_time = DEFAULT_WINDOWS_UPPER_BOUNDS$long) {

    original_columns <- colnames(fix_mes_age)
    windows_start_ms <- nb_1 * bin_size

    add_window_columns<- function(df, size, window_end_ms){
      # adds '{size}win' and 'whichwin_{size}' columns where size is "short", "med", or "long"
      df %>%
        dplyr::mutate(
          sizewin = dplyr::between(time_shifted_ms,
                                   windows_start_ms, window_end_ms),
          whichwin_size = dplyr::case_when(
            prewin ~ "pre",
            sizewin ~ size,  # e.g., "short"
            TRUE ~ "neither")) %>%
        dplyr::rename(
          '{size}win' := sizewin,
          'whichwin_{size}' := whichwin_size)
    }

    fix_mes_age %>%
      dplyr::mutate(
        time_shifted_ms = Time - TargetOnset,
        prewin = time_shifted_ms <= 0) %>%
      add_window_columns("short", short_window_time) %>%
      add_window_columns("med", med_window_time) %>%
      add_window_columns("long", long_window_time) %>%
      dplyr::mutate(
        dplyr::across(
          c(prewin, shortwin, medwin, longwin),
          ~ as.factor(ifelse(.x, "Y", "N")))) %>%
      dplyr::mutate(Nonset = ceiling(time_shifted_ms / bin_size) * bin_size) %>%
      dplyr::select(dplyr::all_of(original_columns),
                    prewin,
                    shortwin, medwin, longwin,
                    whichwin_short, whichwin_med, whichwin_long,
                    Nonset)}

#################################################################################

#' (no docs yet) Mark "low-data" trials with too few bins with fixations
#'
#' @param gazeData 
#' @param subsetWin 
#' @param window_size 
#' @param nb_2 
#' @param binSize 
#' @param propt 
#' @param timeBin 
#' @param Trial 
#' @param SubjectNumber 
#'
#' @return
#' @export
#'
#' @examples
# TODO: zhenya2zhenya: tag_bins_in_low_data_trials would be a much more appropriate name
FindLowData <- function(gazeData,
                        subsetWin,
                        window_size = NULL,
                        nb_2 = 0,
                        binSize = 20) {
  # I don't want to change the API yet but I want meaningful variable names
  # TODO: zhenya2zhenya: rename in the function definition later
  windows_indicicator_column <- subsetWin
  window_upper_bound <- window_size
  window_lower_bound <- nb_2
  bin_size <- binSize
  
  original_columns <- colnames(gazeData)

  # If the window size is not provided, use the default for the window indicator column
  if (is.null(window_upper_bound) &
      windows_indicicator_column %in% c("shortwin", "medwin", "longwin")) {
    window_size_label <- stringr::str_remove(windows_indicicator_column, "win$")
    window_upper_bound <- DEFAULT_WINDOWS_UPPER_BOUNDS[[window_size_label]]
  }

  MIN_DATA_FRACTION <- 1/3
  window_size <- window_upper_bound - window_lower_bound
  min_bins_with_data <- floor(window_size * MIN_DATA_FRACTION / bin_size)

  gazeData_tagged <- gazeData %>%
    dplyr::mutate(
      in_time_window = !!sym(subsetWin) == "Y",
      on_aoi = !is.na(propt),
      has_data = in_time_window & on_aoi) %>%
    dplyr::group_by(Trial, SubjectNumber) %>%
    dplyr::mutate(
      bins_with_data_count = sum(has_data),
      missing_TF = bins_with_data_count < min_bins_with_data) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(original_columns), missing_TF)
  
  return(gazeData_tagged)

}

#################################################################################

#RemoveLowData ----
# TODO: zh: if this function stays, it should call taglowdata instead of running its own version
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
      window_size <- DEFAULT_WINDOWS_UPPER_BOUNDS$long
    } else if (subsetWin=="medwin"){
      window_size <- DEFAULT_WINDOWS_UPPER_BOUNDS$med
    } else if (subsetWin=="shortwin"){
      window_size <- DEFAULT_WINDOWS_UPPER_BOUNDS$short
    }
  }

  maxBins <- as.integer((window_size - nb_2)/binSize)
  maxMissing <- as.integer((window_size - nb_2) - ((window_size - nb_2)/3))

  gazeData2 <- gazeData %>%
    dplyr::filter(gazeData[,subsetWin] == "Y")

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
    dplyr::filter(missing_TF == F) %>%
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
# this gives you back a little data frame where for a given FixationID,
# there's a row for each timeBin, based on the start and stop time  (i.e.
#(CURRENT_FIX_START) and (FixEnd) of that fixation.
# e.g. if FixationID #1 went from 30-310ms, it would make a range,
# ceiling(30/20) : ceiling(90/20), i.e. 2:5, so you'd see
# timeBin FixationID
# 2          1
# 3          1
# 4          1
# 5          1
# TODO: zh: This function is used in BinifyFixations, move it before the latter is defined.
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
    dplyr::filter(frozen == F) %>%
    dplyr::select(-frozen)

  message("frozen trials have been removed. To identify them in a new column without removing them, use blabr::FindFrozenTrials.")
  return(gazeData)
}

# expandFixList <- function(d, binSize=20){
#   timeBin<-(ceiling(d$CURRENT_FIX_START/binSize):ceiling(d$FixEnd/binSize))
#   data.frame(timeBin=timeBin,FixationID=d$FixationID)
# }
