# Filename: wordVector_fxn.R
# Author: m. ezekiel
#
# Dec 12, 2015
#
# Note that the order of commands allows for text to be split by punctuation symbols before the symbols are removed. 


wordVector_fxn <- function(file, split = "[.]", removePunct = FALSE) {

  # Import and remove html tags --this may slow the function speed
  text_data <- gsub("<(.*?)>", "", readLines(file))
  
  if(removePunct == TRUE) {
    wordVector <- unlist(strsplit(text_data, split = split))
    wordVector <- gsub("[,.?!+:;\"()“”◦▪•\t]", "", wordVector) }
  else
    wordVector <- unlist(strsplit(text_data, split = split))
  
  wordVector <- trimws(wordVector)
  
  return(wordVector)
}

