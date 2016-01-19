library(mallet)
library(stringr)
library(stringi)
library(plyr)
library(ggplot2)

#tm_data_format = a function to get the data in order from raw text to proper dataframe for input into mallet

tm_data_format <- function(country_wd){
  setwd(country_wd)
  
  files <- list.files()
  name<- gsub("_en.txt", "", files)
  name<- gsub("_en copy.txt", "", name)
  
  txt <- lapply(files, readLines)
  party<- stri_sub(name, 1,-5)
  year <- as.numeric(as.character(stri_sub(name, -4, -1)))
  
  # label <- function(party, year, txt){
  years <- NULL
  parties <- NULL
  txts <- NULL
  lengths<- NULL
  for (p in 1:length(txt)){
    for (l in 1:length(txt[[p]])){
      yr<- year[p]
      years <- c(years,yr)
      pty<- party[p]
      parties <- c(parties, pty)
      t <- txt[[p]][l]
      length <- length(str_split(t,' ')[[1]])
      lengths <- c(lengths,length)
      txts <- c(txts,t)
    }
  }
  full <- as.data.frame(cbind(years,parties,lengths,txts))
  full$years <- as.numeric(full$years)
  full$txt <- as.character(full$txts)
  full$parties <- as.character(full$parties)
  
  return(full)
}

de<- tm_data_format("~/Documents/manifestos/germany")
write.csv(de, "~/Documents/test_de.csv")
de <- read.csv("~/Documents/test_de.csv")
#tm_mallet = a function to run mallet on your data and produce a list of objects

tm_mallet <- function(dat, n = 25, stop_txt, iter = 100){
  
  documents <- na.omit(dat)
  mallet.instances <- mallet.import(as.character(documents$X), as.character(documents$txts), stoplist.file = stop_txt, token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
  
  ## Create a topic trainer object.
  topic.model <- MalletLDA(num.topics=n)
  
  topic.model$loadDocuments(mallet.instances)
  
  topic.model$setAlphaOptimization(20, 50)
  
  ## Now train a model.
  ##  We can specify the number of iterations. Here we'll use a large-ish round number.
  topic.model$train(iter)
  
  topic.model$maximize(50) #rather than sampling auom the posterior, for every word just set the word to its topic of "best fit".  do this fifty times, and see if there is convergence. this procedure is called iterate conditional modes.
  
  doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T) #creating matrices which is rows=docs, columns=topics. normalized in this context means that it will all add up to 1.
  doc.topic.info <- cbind(documents$parties, documents$years, documents$lengths, as.numeric(documents$lengths)* doc.topics)
  topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T) #rows=topics, columns=size of vocabulary (# of words)
  
  topic.labels <- mallet.topic.labels(topic.model, topic.words, 3)
  
  colnames(doc.topic.info) <- c("party","year", "length", topic.labels)
  return (list(topic.model, doc.topics, doc.topic.info, topic.words))
  
  
}
this <- tm_mallet(de, stop_txt = "~/Documents/Dissie data/stop.txt")
str(this)

tm_topic_extraction <- function(tm_mallet_out){
  topic_words  <- vector("list", 25)
  for (i in 1:25){
    topic_words[[i]] <- mallet.top.words(tm_mallet_out[[1]], tm_mallet_out[[4]][i,], num.top.words = 10000)
  }
  
  topic_wordsdf <- ldply(topic_words, data.frame)
  topic_wordsdf <- matrix(topic_wordsdf[,1], nrow=500) 
  
  mturk_mat <- ldply(topic_words, function(x) x$words[c(1:6,9999,10000)])
  mturk_mat$topics <- colnames(tm_mallet_out[[3]][4:29])
  colnames(mturk_mat) <- c(1,2,3,4,5,6,"last1", "last2")
 
  
  return(list(topic_words,topic_wordsdf, mturk_mat))
  
}

