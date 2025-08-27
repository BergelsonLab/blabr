# Paths to actual corpus files used in tests

blab_share_path <- Sys.getenv('BLAB_SHARE_PATH')
if (endsWith(blab_share_path, ':')) {
  blab_share_path <- paste0(blab_share_path, '/')
}
assertthat::assert_that(
  is.dir(blab_share_path))


# Seedlings

seedlings_test_files <- list()

seedlings_test_files$audio_csv_path <- function() {
  analysis_dir <-
    file.path(blab_share_path,
              'Seedlings/Subject_Files/01/01_12/Home_Visit/Analysis/')
  file.path(analysis_dir, 'Audio_Analysis/01_12_audio_sparse_code.csv')
}


seedlings_test_files$audio_annotations <- function() {
  audio_csv_path <- seedlings_test_files$audio_csv_path()
  read_seedlings_audio_annotations(audio_csv_path)
}

seedlings_test_files$its_path <- function() {
  processing_dir <-
    file.path(blab_share_path,
              'Seedlings/Subject_Files/01/01_12/Home_Visit/Processing/')
  file.path(processing_dir, 'Audio_Files/01_12.its')
}

seedlings_test_files$its_xml <- function() {
  its_path <- seedlings_test_files$its_path()
  rlena::read_its_file(its_path)
}

# VIHI
