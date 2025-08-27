#this function accepts a vector of column names and the output file
#type ("csv" or "feather"); returns a new tibble with unique 3 digit
#ID code for each subject number, output a file with selected columns
anonymous <- function(df, colNameList, fileType) {
  #Import data
  tibble <- df
  #get all subject number without replicates
  subjectNumcol <- tibble[["SubjectNumber"]]
  subjectNum <- subjectList(subjectNumcol)
  #get all possible ID codes
  idList <- c()
  for (i in 0:999) {
    i <- formatC(i, width = 3, flag = "0")
    idList <- c(idList, i)
  }
  #get random ID index
  idIndex <- sample.int(1000, size = length(subjectNum))
  randomID <- list()
  for (i in idIndex) {
    randomID <- c(randomID, idList[[i]])
  }
  #assign ID to each subject number
  names(randomID) <- subjectNum
  #create ID column
  IDcol <- c()
  for (i in subjectNumcol) {
    IDcol <- c(IDcol, randomID[[i]])
  }
  tibble <- tibble::add_column(tibble, "ID_code" = IDcol)
  #generate filteredData file
  newtibble <- tibble
  newtibble <- newtibble[,c(colNameList, "ID_code")]
  if (fileType == "csv") {
    readr::write_csv(newtibble, "filteredData.csv")
  }
  else {
    arrow::write_feather(newtibble, "filteredData.feather")
  }
  return (tibble)
}

subjectList <- function(list) {
  output <- c()
  for (x in list) {
    if (!(x %in% output)) {
      output <- c(output, x)
    }
  }
  return (output)
}
