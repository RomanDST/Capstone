getwd()
setwd("C:/Users/ozerov.roman.i/Documents/DS/Capstone/Application")

library(RWekajars)
library(ggplot2)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(qdap)
library(NLP)
library(tm)
library(SnowballC)
library(slam)
library(RWeka)
library(rJava)

## Load the original data set
blogs <- readLines("./data/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("./data/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twits <- readLines("./data/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

## Generate a random sapmle of all sources
sampleNews <- sample(news, 200000, TRUE)
sampleBlogs <- sample(blogs, 200000, TRUE)
sampleTwits <- sample(twits, 200000 , TRUE)
textSample <- c(sampleNews,sampleBlogs,sampleTwits)

## Save sample
writeLines(textSample, "./data/en_US/bigTextSample.txt")

## Build a clean corpus
theSampleCon <- file("./data/en_US/bigTextSample.txt")
theSample <- readLines(theSampleCon)
close(theSampleCon)

## Build the corpus, and specify the source to be character vectors 
cleanSample <- Corpus(VectorSource(theSample))

## Remove punction, numbers, URLs, stop, profanity and stem wordson
rm(theSample)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
cleanSample <- tm_map(cleanSample, content_transformer(tolower))
cleanSample <- tm_map(cleanSample, content_transformer(removePunctuation))
cleanSample <- tm_map(cleanSample, content_transformer(removeNumbers))
cleanSample <- tm_map(cleanSample, content_transformer(removeURL))
cleanSample <- tm_map(cleanSample, stripWhitespace)
cleanSample <- tm_map(cleanSample, removeWords, stopwords("english"))
cleanSample <- tm_map(cleanSample, stemDocument)

## Save the final corpus
saveRDS(cleanSample, file = "./data/en_US/finalCorpus.RData")

## Save the Term Document Matrix
theCorpus <- readRDS(file = "./data/en_US/finalCorpus.RData")
sampleTDM <- TermDocumentMatrix(theCorpus)
saveRDS(sampleTDM, file = "./data/en_US/sampleTDM.RData")

#Create the final corpus data frame#
finalCorpus <- readRDS("./data/en_US/finalCorpus.RData")
finalCorpusDF <-data.frame(text=unlist(sapply(finalCorpus,`[`, "content")), 
                           stringsAsFactors = FALSE)

## Build the tokenization function for the n-grams
ngramTokenizer <- function(theCorpus, ngramCount) {
  ngramFunction <- RWeka::NGramTokenizer(theCorpus, 
                                         RWeka::Weka_control(min = ngramCount, max = ngramCount, 
                                                             delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngramFunction <- data.frame(table(ngramFunction))
  ngramFunction <- ngramFunction[order(ngramFunction$Freq, 
                                       decreasing = TRUE),][1:10,]
  colnames(ngramFunction) <- c("String","Count")
  ngramFunction
}

unigram <- ngramTokenizer(finalCorpusDF, 1)
saveRDS(unigram, file = "./data/en_US/unigram.txt")

bigram <- ngramTokenizer(finalCorpusDF, 2)
saveRDS(bigram, file = "./data/en_US/bigram.txt")

trigram <- ngramTokenizer(finalCorpusDF, 3)
saveRDS(trigram, file = "./data/en_US/trigram.txt")

quadgram <- ngramTokenizer(finalCorpusDF, 4)
saveRDS(quadgram, file = "./data/en_US/quadgram.txt")


