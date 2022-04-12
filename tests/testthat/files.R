# Paths to actual corpus files used in tests

library(assertthat)

pn_opus_path <- Sys.getenv('PN_OPUS_PATH')
assert_that(is.dir(pn_opus_path))


# Seedlings

seedlings_test_files <- list()

seedlings_test_files$audio_csv_path <- function() {
  analysis_dir <-
    file.path(pn_opus_path,
              'Seedlings/Subject_Files/01/01_12/Home_Visit/Analysis/')
  file.path(analysis_dir, 'Audio_Analysis/01_12_audio_sparse_code.csv')
}


seedlings_test_files$audio_annotations <- function() {
  audio_csv_path <- seedlings_test_files$audio_csv_path
  read_seedlings_audio_annotations(audio_csv_path)
}

seedlings_test_files$its_path <- function() {
  processing_dir <-
    file.path(pn_opus_path,
              'Seedlings/Subject_Files/01/01_12/Home_Visit/Processing/')
  file.path(processing_dir, 'Audio_Files/01_12.its')
}

# VIHI
