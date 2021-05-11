rm(list=ls())

################################################################################
################################ MISCELLANEOUS #################################
################################################################################

## Dependencies installation
  if("twitteR" %in% rownames(installed.packages()) == FALSE) {
    install.packages("twitteR")
  }
  if("tidyverse" %in% rownames(installed.packages()) == FALSE) {
    install.packages("tidyverse")
  }
  if("tm" %in% rownames(installed.packages()) == FALSE) {
    install.packages("tm")
  }
  if("SnowballC" %in% rownames(installed.packages()) == FALSE) {
    install.packages("SnowballC")
  }
  if("wordcloud" %in% rownames(installed.packages()) == FALSE) {
    install.packages("wordcloud")
  }
  if("textcat" %in% rownames(installed.packages()) == FALSE) {
    install.packages("textcat")
  }
  if("igraph" %in% rownames(installed.packages()) == FALSE) {
    install.packages("igraph")
  }
  if("ggraph" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggraph")
  }

if("parallel" %in% rownames(installed.packages()) == FALSE) {
  install.packages("parallel")
}

if("RWeka" %in% rownames(installed.packages()) == FALSE) {
  install.packages("RWeka")
}
if("slam" %in% rownames(installed.packages()) == FALSE) {
  install.packages("slam")
}
if("rio" %in% rownames(installed.packages()) == FALSE) {
  install.packages("rio")
}
if("samplingbook" %in% rownames(installed.packages()) == FALSE) {
  install.packages("samplingbook")
}

## Dependencies loading
  library("twitteR")
  library("tidyverse")
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("textcat")
  library("igraph")
  library("ggraph")
  library("parallel")
  library("stringr")
  library("qdap")
  library("RWeka")
  library("slam")
  library("stringr")
library("rlist")
library("tm")
library("rio")
library("xlsx")
library("samplingbook")

## Raw data loading
  # load("CyClCm_20160101to20160630.RData")

setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats Sentiment Analysis")

  load("id_tweets_EE.Rdata")
  load("CyClCm_ff.RData")
  
  df<-selec_tweets
  df<-df[df$id %in% ee,]
  
  positive_unigrams<-c("test","teacher")
  
  #deteccion de terminos en tweets
  text_tweets<-df$txtc
  l_words_to_compare<-list()
  
  words_to_compare<-positive_unigrams
  word_l<-strsplit(text_tweets, "[^A-Za-z']+")

  for(j in words_to_compare){
    no_cores <- detectCores()-1
    cl <- makeCluster(no_cores, type="PSOCK")
    clusterExport(cl, c("j","word_l", "unlist"))
    data_tweets <- parLapply(cl,word_l, function(x) (unlist(x) %in% j))
    stopCluster(cl)
    words_detected<-unlist(lapply(seq(1:length(data_tweets)),function(x) sum(data_tweets[[x]])))
    words_detected<-which(words_detected!=0)
    l_words_to_compare<-list.append(l_words_to_compare,words_detected)
  }

  names(l_words_to_compare)<-positive_unigrams
  
  l_n<-list()
  for(j in 1:length(l_words_to_compare))
  {
    txt<-(df[l_words_to_compare[[j]],"text"])
    save(txt, file=paste("EE_positive_",names(l_words_to_compare[j]),".RData", sep=""))
    n<-length(txt)
    l_n<-list.append(l_n,n)
  }  
  

  #sample size
  l_n<-unlist(l_n)
  n_sample<-lapply(l_n,function(x) sample.size.prop(e=0.05, P=0.5, N=x))
  n_sample<-lapply(seq(1:length(n_sample)), function(x) n_sample[[x]]$n)
  n_sample<-unlist(n_sample)
  results<-cbind.data.frame(names(l_words_to_compare),l_n,n_sample)
  save(results,file="n_samples_frequent_terms_EE.RData")

  #extraccion de la muestra de tweets
  load("n_samples_frequent_terms_EE.RData")
  set.seed(1234)
  l_elements<-lapply(seq(1:length(results$namesheets)), function(x) sample(results[x,"l_n"], results[x,"n_sample"]))


  for(j in 1:length(l_words_to_compare))
  {
    load(paste("EE_positive_",names(l_words_to_compare[j]),".RData", sep=""))
    datafile<-txt
    datafile<-datafile[l_elements[[j]]]
    write.xlsx2(datafile,"EE_sample.xlsx",sheetName=names(l_words_to_compare[j]),rownames=FALSE,append=TRUE)
  }
  
  convert("EE_sample.xlsx","EE_sample.csv")
