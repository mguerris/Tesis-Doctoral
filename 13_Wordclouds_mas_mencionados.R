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

## Dependencies loading
  library("twitteR")
  library("tidyverse")
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("textcat")
  library("igraph")
  library("ggraph")
  library("qdapRegex")

## Raw data loading
  # load("CyClCm_20160101to20160630.RData")

load("Tweets sin id repetido english.RData")
load("All_mentions.RData")

#Limpieza de tweets
minlenparaules<-2
docs<-df2$text
docs<-iconv(docs,to="UTF-8")
#Eliminamos cualquier caracter entre los simbolos < y >
docs<-rm_between(docs, left="<", right=">", replacement=" ")
#Eliminamos los usuarios en cualquier parte de la cadena de texto
docs<-gsub("\\S*[@]\\S*"," ",docs)
#Eliminamos las urls completas que empiezan por https
docs<-gsub("\\bhttps?[:]\\S*\\b"," ",docs)
#eliminacion de http:/.... completo
docs<-gsub('\\s*http\\S+\\s*', '', docs)
#elimina simbolos de puntuacion y deja un espacio para eliminar posteriormente 
#por ejemplo contracciones del tipo they're
docs<- gsub("[[:punct:]]", " ", docs)
#elimina cualquier caracter o simbolo fuera de las letras minusculas o mayusculas
docs<-gsub("[^a-zA-Z ']"," " ,docs)
#pone el texto en minusculas
docs<-tolower(docs)
#cambiamos los no por nonono para no eliminar el no posteriormente
#docs<-gsub("no","nonono" ,docs)
#Elimina multiples espacios a uno
docs <- gsub("\\s+", " ", docs) 
#Remove leading and traildocswhitespaces
docs <- gsub("^\\s+|\\s+$", "", docs)
#eliminar las palabras de menos o igual 1 caracter y igual o mayor que minlenparaules
palabrasmin<-paste("\\b[a-zA-Z0-9]{","1,",minlenparaules,"}\\b",sep="")
docs<-gsub(palabrasmin, "", docs) 
#sustituimos de nuevo los nonono por los no
#docs<-gsub("nonono","no" ,docs)
#Elimina multiples espacios a uno
docs <- gsub("\\s+", " ", docs) 
# remove leading and traildocswhitespaces
docs <- gsub("^\\s+|\\s+$", "", docs)

df2$txtc<-docs

#Seleccionamos las stopwords 
new_stopw<-stopwords(kind="en")

#Limpiamos las stopwords como los tweets ylos dejamos en el mismo formato
docs<-new_stopw
#elimina simbolos de puntuacion y deja un espacio para eliminar posteriormente 
#por ejemplo contracciones del tipo they're
docs<- gsub("[[:punct:]]", " ", docs)
#elimina cualquier caracter o simbolo fuera de las letras minusculas o mayusculas
docs<-gsub("[^a-zA-Z ']"," " ,docs)
#pone el texto en minusculas
docs<-tolower(docs)
#Elimina multiples espacios a uno
docs <- gsub("\\s+", " ", docs) 
#Remove leading and traildocswhitespaces
docs <- gsub("^\\s+|\\s+$", "", docs)
#eliminar las palabras de menos o igual 1 caracter y igual o mayor que minlenparaules
palabrasmin<-paste("\\b[a-zA-Z0-9]{","1,",minlenparaules,"}\\b",sep="")
docs<-gsub(palabrasmin, "", docs) 
#Elimina multiples espacios a uno
docs <- gsub("\\s+", " ", docs) 
# remove leading and traildocswhitespaces
docs <- gsub("^\\s+|\\s+$", "", docs)

new_stopw<-docs

#quitamos de la lista las que no son NA o bien no est?n vac?as
new_stopw<-new_stopw[!is.na(new_stopw) & new_stopw!=""]

#a?adimos stopwords a la lista
new_stopw<-c(new_stopw, "just", "now", "got", "will", "get", "much", "can", "no")

#eliminamos los stopwords de los tweets limpios
df2$txtc<-removeWords(df2$txtc,new_stopw)
df2$txtc<- gsub("^\\s+|\\s+$", "", df2$txtc)
df2<-subset(df2, txtc!="")

save(df2,file="Tweets_nostop_limpios_conduplicados.RData")

library(stringr)
menciones$data_mentions<-str_remove(menciones$data_mentions,"@")

#All Tweets
rm(list = setdiff(ls(), lsf.str()))
setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")
#setwd("C:/Users/mguerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")
load("Tweets_nostop_limpios_conduplicados.RData")
load("All_mentions.RData")
load("EnglishTwitterPopularity_all.RData")
load("EnglishTwitterPopularityScreen_all.RData")

#Mencionados
library(stringr)
menciones$data_mentions<-str_remove(menciones$data_mentions,"@")

EnglishTwitterPopularity<-EnglishTwitterPopularity[order(-EnglishTwitterPopularity$Repeated),]
mencionados<-EnglishTwitterPopularity[1:5,"Mentioned"]
pos<-lapply(seq(1:5), function(x) which(menciones$data_mentions %in% mencionados[x]))
id_tweets<-lapply(seq(1:5), function(x) menciones[pos[[x]],"id"])
pos_tw<-lapply(seq(1:5), function(x) which(df2$id %in% id_tweets[[x]]))
text_tw<-lapply(seq(1:5), function(x) df2[pos_tw[[x]],"txtc"])
text_tw_uni<-lapply(seq(1:5), function(x) unique(text_tw[[x]], collapse = ''))
text_tw_joint<-lapply(seq(1:5), function(x) paste(text_tw_uni[[x]], collapse = ''))

text_tw_raw<-lapply(seq(1:5), function(x) unique(df2[pos_tw[[x]],"text"]))
extrae_tw<-function(textos,usuario, tipo){
  fichero<-paste(tipo,"_",usuario,".csv")
  fichero_xlsx<-paste(tipo,"_",usuario,".xlsx")
  write.csv(textos,file=fichero)
  openxlsx::write.xlsx(textos, file=fichero_xlsx)
}
lapply(seq(1:5), function(x) extrae_tw(text_tw_raw[[x]],mencionados[x],"ALL_tweets_mencionados"))

tweets_por_mencionados<-data.frame(mencionados, unlist(lapply(seq(1:5), function(x) length(text_tw_uni[[x]]))), stringsAsFactors = FALSE)
colnames(tweets_por_mencionados)<-c("mencionados", "n_tweets")
words_in_tweets<-sapply(seq(1:5), function(x) length(unique(unlist(str_extract_all(text_tw_joint[[x]], "[[:alpha:]]+")))))
tweets_por_mencionados$unique_words<-words_in_tweets

myCorpus <- lapply(seq(1:5), function(x) VCorpus(VectorSource(text_tw_joint[[x]])))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]]))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))

pinta_wc<-function(freq, titulo, usuario, unibi){
  fichero=paste("WC_",titulo, "_", usuario, "_", unibi,".png",sep="")
  png(fichero, units="mm",width=210, height=100, res=300)
  wordcloud(names(freq[1]$`1`), freq[1]$`1`,scale=c(1.5,.5),rot.per=0,fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))
  dev.off()
} 

wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"ALL_mencionados", mencionados[[x]],1))

library(RWeka)
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]], control = list(tokenize = NumbergramTokenizer)))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"ALL_mencionados", mencionados[[x]],2))

#Los que mencionan
EnglishTwitterPopularityScreen<-EnglishTwitterPopularityScreen[order(-EnglishTwitterPopularityScreen$Repeated),]
mencionados<-EnglishTwitterPopularityScreen[1:5,"screenName"]
pos<-lapply(seq(1:5), function(x) which(menciones$screenName %in% mencionados[x]))
id_tweets<-lapply(seq(1:5), function(x) menciones[pos[[x]],"id"])
pos_tw<-lapply(seq(1:5), function(x) which(df2$id %in% id_tweets[[x]]))
text_tw<-lapply(seq(1:5), function(x) df2[pos_tw[[x]],"txtc"])
text_tw_uni<-lapply(seq(1:5), function(x) unique(text_tw[[x]], collapse = ''))
text_tw_joint<-lapply(seq(1:5), function(x) paste(text_tw_uni[[x]], collapse = ''))

text_tw_raw<-lapply(seq(1:5), function(x) unique(df2[pos_tw[[x]],"text"]))
extrae_tw<-function(textos,usuario, tipo){
  fichero<-paste(tipo,"_",usuario,".csv")
  fichero_xlsx<-paste(tipo,"_",usuario,".xlsx")
  write.csv(textos,file=fichero)
  openxlsx::write.xlsx(textos, file=fichero_xlsx)
}
lapply(seq(1:5), function(x) extrae_tw(text_tw_raw[[x]],mencionados[x],"ALL_tweets_mencionan"))

library(stringr)
tweets_por_mencionados<-data.frame(mencionados, unlist(lapply(seq(1:5), function(x) length(text_tw_uni[[x]]))), stringsAsFactors = FALSE)
colnames(tweets_por_mencionados)<-c("mencionados", "n_tweets")
words_in_tweets<-sapply(seq(1:5), function(x) length(unique(unlist(str_extract_all(text_tw_joint[[x]], "[[:alpha:]]+")))))
tweets_por_mencionados$unique_words<-words_in_tweets

myCorpus <- lapply(seq(1:5), function(x) VCorpus(VectorSource(text_tw_joint[[x]])))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]]))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"ALL_mencionan", mencionados[[x]],1))


library(RWeka)
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]], control = list(tokenize = NumbergramTokenizer)))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"ALL_mencionan", mencionados[[x]],2))



##########################################################################################################33
#AH Tweets
rm(list = setdiff(ls(), lsf.str()))
setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")
#setwd("C:/Users/mguerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")
load("Tweets_nostop_limpios_conduplicados.RData")
load("AH_mentions.RData")
load("EnglishTwitterPopularity_AH.RData")
load("EnglishTwitterPopularityScreen_AH.RData")

menciones<-menciones_AH

#Mencionados
library(stringr)
menciones$data_mentions<-str_remove(menciones$data_mentions,"@")

EnglishTwitterPopularity<-EnglishTwitterPopularity[order(-EnglishTwitterPopularity$Repeated),]
mencionados<-EnglishTwitterPopularity[1:5,"Mentioned"]
pos<-lapply(seq(1:5), function(x) which(menciones$data_mentions %in% mencionados[x]))
id_tweets<-lapply(seq(1:5), function(x) menciones[pos[[x]],"id"])
pos_tw<-lapply(seq(1:5), function(x) which(df2$id %in% id_tweets[[x]]))
text_tw<-lapply(seq(1:5), function(x) df2[pos_tw[[x]],"txtc"])
text_tw_uni<-lapply(seq(1:5), function(x) unique(text_tw[[x]], collapse = ''))
text_tw_joint<-lapply(seq(1:5), function(x) paste(text_tw_uni[[x]], collapse = ''))

text_tw_raw<-lapply(seq(1:5), function(x) unique(df2[pos_tw[[x]],"text"]))
extrae_tw<-function(textos,usuario, tipo){
  fichero<-paste(tipo,"_",usuario,".csv")
  fichero_xlsx<-paste(tipo,"_",usuario,".xlsx")
  write.csv(textos,file=fichero)
  openxlsx::write.xlsx(textos, file=fichero_xlsx)
}
lapply(seq(1:5), function(x) extrae_tw(text_tw_raw[[x]],mencionados[x],"AH_tweets_mencionados"))

library(stringr)
tweets_por_mencionados<-data.frame(mencionados, unlist(lapply(seq(1:5), function(x) length(text_tw_uni[[x]]))), stringsAsFactors = FALSE)
colnames(tweets_por_mencionados)<-c("mencionados", "n_tweets")
words_in_tweets<-sapply(seq(1:5), function(x) length(unique(unlist(str_extract_all(text_tw_joint[[x]], "[[:alpha:]]+")))))
tweets_por_mencionados$unique_words<-words_in_tweets

myCorpus <- lapply(seq(1:5), function(x) VCorpus(VectorSource(text_tw_joint[[x]])))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]]))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
pinta_wc<-function(freq, titulo, usuario, unibi){
  fichero=paste("WC_",titulo, "_", usuario, "_", unibi,".png",sep="")
  png(fichero, units="mm",width=210, height=100, res=300)
  wordcloud(names(freq[1]$`1`), freq[1]$`1`,scale=c(1.5,.5),rot.per=0,fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))
  dev.off()
} 
  
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"AH_mencionados", mencionados[[x]],1))

library(RWeka)
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]], control = list(tokenize = NumbergramTokenizer)))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"AH_mencionados", mencionados[[x]],2))

#Los que mencionan
EnglishTwitterPopularityScreen<-EnglishTwitterPopularityScreen[order(-EnglishTwitterPopularityScreen$Repeated),]
mencionados<-EnglishTwitterPopularityScreen[1:5,"screenName"]
pos<-lapply(seq(1:5), function(x) which(menciones$screenName %in% mencionados[x]))
id_tweets<-lapply(seq(1:5), function(x) menciones[pos[[x]],"id"])
pos_tw<-lapply(seq(1:5), function(x) which(df2$id %in% id_tweets[[x]]))
text_tw<-lapply(seq(1:5), function(x) df2[pos_tw[[x]],"txtc"])
text_tw_uni<-lapply(seq(1:5), function(x) unique(text_tw[[x]], collapse = ''))
text_tw_joint<-lapply(seq(1:5), function(x) paste(text_tw_uni[[x]], collapse = ''))

text_tw_raw<-lapply(seq(1:5), function(x) unique(df2[pos_tw[[x]],"text"]))
extrae_tw<-function(textos,usuario, tipo){
  fichero<-paste(tipo,"_",usuario,".csv")
  fichero_xlsx<-paste(tipo,"_",usuario,".xlsx")
  write.csv(textos,file=fichero)
  openxlsx::write.xlsx(textos, file=fichero_xlsx)
}
lapply(seq(1:5), function(x) extrae_tw(text_tw_raw[[x]],mencionados[x],"AH_tweets_mencionan"))

library(stringr)
tweets_por_mencionados<-data.frame(mencionados, unlist(lapply(seq(1:5), function(x) length(text_tw_uni[[x]]))), stringsAsFactors = FALSE)
colnames(tweets_por_mencionados)<-c("mencionados", "n_tweets")
words_in_tweets<-sapply(seq(1:5), function(x) length(unique(unlist(str_extract_all(text_tw_joint[[x]], "[[:alpha:]]+")))))
tweets_por_mencionados$unique_words<-words_in_tweets


myCorpus <- lapply(seq(1:5), function(x) VCorpus(VectorSource(text_tw_joint[[x]])))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]]))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
  
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"AH_mencionan", mencionados[[x]],1))

library(RWeka)
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]], control = list(tokenize = NumbergramTokenizer)))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"AH_mencionan", mencionados[[x]],2))


##########################################################################################################33
#EE Tweets
rm(list = setdiff(ls(), lsf.str()))
#setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")
setwd("C:/Users/mguerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")
load("Tweets_nostop_limpios_conduplicados.RData")
load("EE_mentions.RData")
load("EnglishTwitterPopularity_EE.RData")
load("EnglishTwitterPopularityScreen_EE.RData")

menciones<-menciones_EE

#Mencionados
library(stringr)
menciones$data_mentions<-str_remove(menciones$data_mentions,"@")

EnglishTwitterPopularity<-EnglishTwitterPopularity[order(-EnglishTwitterPopularity$Repeated),]
mencionados<-EnglishTwitterPopularity[1:5,"Mentioned"]
pos<-lapply(seq(1:5), function(x) which(menciones$data_mentions %in% mencionados[x]))
id_tweets<-lapply(seq(1:5), function(x) menciones[pos[[x]],"id"])
pos_tw<-lapply(seq(1:5), function(x) which(df2$id %in% id_tweets[[x]]))
text_tw<-lapply(seq(1:5), function(x) df2[pos_tw[[x]],"txtc"])
text_tw_uni<-lapply(seq(1:5), function(x) unique(text_tw[[x]], collapse = ''))
text_tw_joint<-lapply(seq(1:5), function(x) paste(text_tw_uni[[x]], collapse = ''))

text_tw_raw<-lapply(seq(1:5), function(x) unique(df2[pos_tw[[x]],"text"]))
extrae_tw<-function(textos,usuario, tipo){
  fichero<-paste(tipo,"_",usuario,".csv")
  fichero_xlsx<-paste(tipo,"_",usuario,".xlsx")
  write.csv(textos,file=fichero)
  openxlsx::write.xlsx(textos, file=fichero_xlsx)
}
lapply(seq(1:5), function(x) extrae_tw(text_tw_raw[[x]],mencionados[x],"EE_tweets_mencionados"))

tweets_por_mencionados<-data.frame(mencionados, unlist(lapply(seq(1:5), function(x) length(text_tw_uni[[x]]))), stringsAsFactors = FALSE)
colnames(tweets_por_mencionados)<-c("mencionados", "n_tweets")
words_in_tweets<-sapply(seq(1:5), function(x) length(unique(unlist(str_extract_all(text_tw_joint[[x]], "[[:alpha:]]+")))))
tweets_por_mencionados$unique_words<-words_in_tweets

myCorpus <- lapply(seq(1:5), function(x) VCorpus(VectorSource(text_tw_joint[[x]])))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]]))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
pinta_wc<-function(freq, titulo, usuario, unibi){
  fichero=paste("WC_",titulo, "_", usuario, "_", unibi,".png",sep="")
  png(fichero, units="mm",width=210, height=100, res=300)
  wordcloud(names(freq[1]$`1`), freq[1]$`1`,scale=c(1.5,.5),rot.per=0,fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))
  dev.off()
} 

wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"EE_mencionados", mencionados[[x]],1))

library(RWeka)
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]], control = list(tokenize = NumbergramTokenizer)))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"EE_mencionados", mencionados[[x]],2))

#Los que mencionan
EnglishTwitterPopularityScreen<-EnglishTwitterPopularityScreen[order(-EnglishTwitterPopularityScreen$Repeated),]
mencionados<-EnglishTwitterPopularityScreen[1:5,"screenName"]
pos<-lapply(seq(1:5), function(x) which(menciones$screenName %in% mencionados[x]))
id_tweets<-lapply(seq(1:5), function(x) menciones[pos[[x]],"id"])
pos_tw<-lapply(seq(1:5), function(x) which(df2$id %in% id_tweets[[x]]))
text_tw<-lapply(seq(1:5), function(x) df2[pos_tw[[x]],"txtc"])
text_tw_uni<-lapply(seq(1:5), function(x) unique(text_tw[[x]], collapse = ''))
text_tw_joint<-lapply(seq(1:5), function(x) paste(text_tw_uni[[x]], collapse = ''))

text_tw_raw<-lapply(seq(1:5), function(x) unique(df2[pos_tw[[x]],"text"]))
extrae_tw<-function(textos,usuario, tipo){
  fichero<-paste(tipo,"_",usuario,".csv")
  fichero_xlsx<-paste(tipo,"_",usuario,".xlsx")
  write.csv(textos,file=fichero)
  openxlsx::write.xlsx(textos, file=fichero_xlsx)
}
lapply(seq(1:5), function(x) extrae_tw(text_tw_raw[[x]],mencionados[x],"EE_tweets_mencionan"))

library(stringr)
tweets_por_mencionados<-data.frame(mencionados, unlist(lapply(seq(1:5), function(x) length(text_tw_uni[[x]]))), stringsAsFactors = FALSE)
colnames(tweets_por_mencionados)<-c("mencionados", "n_tweets")
words_in_tweets<-sapply(seq(1:5), function(x) length(unique(unlist(str_extract_all(text_tw_joint[[x]], "[[:alpha:]]+")))))
tweets_por_mencionados$unique_words<-words_in_tweets

myCorpus <- lapply(seq(1:5), function(x) VCorpus(VectorSource(text_tw_joint[[x]])))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]]))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"EE_mencionan", mencionados[[x]],1))

library(RWeka)
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]], control = list(tokenize = NumbergramTokenizer)))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"EE_mencionan", mencionados[[x]],2))
