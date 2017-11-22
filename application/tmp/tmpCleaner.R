
suppressPackageStartupMessages(c(
  library(shinythemes),
  library(shiny),
  library(tm),
  library(stringr),
  library(markdown),
  library(stylo)))

blogs <- readLines("./data/en_US/en_US.blogs.txt", 1000, encoding = "UTF-8", skipNul = TRUE)
news <- readLines("./data/en_US/en_US.news.txt", 1000, encoding = "UTF-8", skipNul = TRUE)
twits <- readLines("./data/en_US/en_US.twitter.txt", 1000, encoding = "UTF-8", skipNul = TRUE)

dataCleaner<-function(text){
  cleanText <- tolower(text)
  cleanText <- removePunctuation(cleanText)
  cleanText <- removeNumbers(cleanText)
  cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
  cleanText <- stripWhitespace(cleanText)
  return(cleanText)
}

cleanInput <- function(text){
  textInput <- dataCleaner(text)
  textInput <- txt.to.words.ext(textInput,language="English.all",preserve.case = TRUE)
  return(textInput)
}

nextWordPrediction <- function(wordCount,textInput){
  if (wordCount>=3) {
    textInput <- textInput[(wordCount-2):wordCount] 
  }
  else if(wordCount==2) {
    textInput <- c(NA,textInput)   
  }
  else {
    textInput <- c(NA,NA,textInput)
  }
  
wordPrediction <- as.character(final4Data[final4Data$unigram==textInput[1] & 
                                              final4Data$bigram==textInput[2] & 
                                              final4Data$trigram==textInput[3],][1,]$quadgram)
  if(is.na(wordPrediction)) {
    wordPrediction1 <- as.character(final3Data[final3Data$unigram==textInput[2] & 
                                                 final3Data$bigram==textInput[3],][1,]$trigram)
    if(is.na(wordPrediction)) {
      wordPrediction <- as.character(final2Data[final2Data$unigram==textInput[3],][1,]$bigram)
    }
  }
  print(wordPrediction)
}


  
  
