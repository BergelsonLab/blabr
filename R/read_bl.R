bl_types <- c("audio", "video")

audio_cnames <- c("tier", "object", "utterance_type", "object_present",
                  "speaker", "timestamp", "basic_level", "annotid")

video_cnames <- c("ordinal", "onset", "offset",
                  "object", "utterance_type", "object_present",
                  "speaker", "basic_level", "annotid")

#' Collect basic level files
#'
#' @param input directory to scan
#' @param type datatype of the basic level file ("audio" or "video")
#'
#' @return dataframe with "file" column containing filepaths
#' @export
#'
#' @examples
#' x <- collect_bl_files("../some_directory", "video")
#' x$files
collect_bl_files <- function(input, type) {
  the_files <- tibble::tibble(files=list.files(input, full.names = TRUE))
  the_files$month <- substr(the_files$files, nchar(input)+ 5, nchar(input)+ 6)
  dplyr::filter(the_files, grepl(type, the_files$files))
}



#' Concatenate basic level files by month
#'
#' @param input directory to scan for basic level files
#' @param output directory to write concatenated by-month basic level files to
#' @param type basic level datatype ("audio" or "video")
#'
#' @return a list of tibbles, each tibble a month's aggregated basic level
#' @export
concat_month_bl <- function(input, output=NULL, type) {
  if (!(type %in% bl_types)) {
    stop("your file type is not recognized")
  }
  collect_errors <- all_errors()


  the_files <- collect_bl_files(input, type)

  read_one_video <- function(x) {
    print(x)
    df <- readr::read_csv(x) %>% rename_video_header(.)
    df <- df[,video_cnames]
    tibble::add_column(df, id=rep(basename(x), times=length(df$object)))
  }

  read_one_audio <- function(x) {
    print(x)
    df <- readr::read_csv(x) %>% rename_audio_header(.)
    df <- df[,audio_cnames]
    tibble::add_column(df, id=rep(basename(x), times=length(df$object)))
  }

  read_all <- function(x) {
    if (type == "audio") {
      print("read audio")
      ll <- purrr::map(x$files, safely(read_one_audio))

    } else {
      ll <- purrr::map(x$files, safely(read_one_video))
    }
    ll <- ll %>% transpose()
    print(ll$error)
    collect_errors(ll$error)
    bind_rows(ll$result)
  }

  out_ext <- paste0("_all_", type, ".csv")
  by_month <- the_files %>% split(.$month) %>% purrr::map(read_all)
  collect_errors()

  if (!is.null(output)) {
    for (name in names(by_month)) {
      write.csv(x=by_month[[name]],
                file=file.path(output,
                               paste0(name, out_ext)),
                row.names = FALSE)
    }
  }
  return(by_month)
}


#' Concatenate all the basic level files
#'
#' @param x list of by-month basic level tibbles
#' @param output if specified, path to the output csv file
#'
#' @return a tibble with all the basic level data
#' @export
#'
#' @examples
#' \dontrun{
#' x <- concat_month_bl("dir/with/bl/files", "output/folder", "video")
#' concat_all_bl(x, "all_video.csv")
#' }
concat_all_bl <- function(x, output=NULL) {
  df <- dplyr::bind_rows(x)
  df <- process_concat_bl(df)
  if (!is.null(output)) {
    write.csv(df, output, row.names = FALSE)
  }
  return(df)
}


#' Join the audio and video dataframes into one big dataframe
#'
#'
#' @param audiostats the full audio basic level dataframe
#' @param videostats the full video basic level dataframe
#' @param output if specified, the output name for csv and feather files (without extension)
#'
#' @return the joined dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' audiomonths <- concat_month_bl("../basiclevel_dataprep/all_bl/", "output/", "audio")
#' audiostats <- concat_all_bl(audiomonths, "all_audio.csv")

#' videomonths <- concat_month_bl("../basiclevel_dataprep/all_bl/", "output/", "video")
#' videostats <- concat_all_bl(videomonths, "all_video.csv")

#' joined_data <- join_full_audio_video(audiostats, videostats)
#' }
join_full_audio_video <- function(audiostats, videostats,
                                  output_name=NULL, keep_na=FALSE,
                                  keep_comments=FALSE) {
  df <- videostats%>%
        dplyr::full_join(audiostats)%>%
        dplyr::mutate(id = as.factor(id),
                     object = as.factor(object),
                     object_present = as.factor(object_present),
                     speaker = as.factor(speaker),
                     basic_level = as.factor(basic_level),
                     subj = as.factor(subj),
                     month = as.factor(month),
                     SubjectNumber = as.factor(SubjectNumber),
                     audio_video = as.factor(audio_video),
                     tier = as.factor(tier),
                     utterance_type = factor(utterance_type,
                                             levels = c("d",
                                                        "q",
                                                        "s",
                                                        "r",
                                                        "n",
                                                        "i",
                                                        "u")))
  if (!keep_comments) {
    df <- dplyr::filter(df, !grepl("%com:", object))
  } else {
    keep_na <- TRUE
  }

  if(!keep_na) {
    df <- dplyr::filter(df, !is.na(basic_level))
  }

  check_result <- check_annot_codes(df, keep_na)

  if (!nrow(check_result) == 0) {
    cat("\n\nTHERE WERE ERRORS FOUND IN THE CODES.....returning errors\n\n")
    return(check_result)
  } else {
    if (!is.null(output_name)) {
      write.csv(df, paste0(output_name, ".csv"), row.names = FALSE)
      arrow::write_feather(df, paste0(output_name, ".feather"))
    }
    return(df)
  }
}


#' Post-processing for the full basic level dataframe
#'
#' Post-processing includes adding a subject and month column, whether
#' it's an audio or video, and reformatting timestamps.
#'
#' The function will figure out whether it's an audio or video dataframe.
#' It must be one or the other though. It will not handle a joined audio+video
#' frame.
#'
#' @param x full (audio or video) basic level dataframe
#'
#' @return processed basic level dataframe
#' @export
process_concat_bl <- function(x) {
  if ("tier" %in% colnames(x)) {
    x %>%
      tidyr::separate(timestamp,sep = "_",into = c("onset","offset"))%>%
      dplyr::mutate(subj = factor(substring(id, 1,2)),
                   month = factor(substring(id,4,5)),
                   SubjectNumber = factor(substring(id,1,5)),
                   audio_video = as.factor("audio"),
                   onset = as.numeric(as.character(onset)),
                   offset = as.numeric(as.character(offset)))
  } else {
    x %>%
      dplyr::mutate(subj = factor(substring(id, 1,2)),
                   month = factor(substring(id,4,5)),
                   SubjectNumber = factor(substring(id,1,5)),
                   audio_video = as.factor("video"),
                   onset = onset/1)
  }
}

rename_audio_header <- function(x) {
  df <- dplyr::rename(x, object = word)
}

rename_video_header <- function(x) {
  df <- dplyr::rename(x,
                     ordinal = labeled_object.ordinal,
                     onset = labeled_object.onset,
                     offset = labeled_object.offset,
                     object = labeled_object.object,
                     utterance_type = labeled_object.utterance_type,
                     object_present = labeled_object.object_present,
                     speaker = labeled_object.speaker,
                     annotid = labeled_object.id
  )
  if ("labeled_object.basic_level" %in% colnames(df)) {
    df <- dplyr::rename(df, basic_level = labeled_object.basic_level)
  }
  return(df)
}


#' A closure to keep track of al the errors from
#' reading the csv files. There might be a more
#' elegant way to concatenate and print all
#' errors from all files, but this seems to do the
#' job for now.
all_errors <- function() {
  errors <- list()
  function(value=NULL) {
    if (is.null(value)) {
      return(errors)
    }
    else {
      errors <<- c(errors, value)
    }
  }
}

