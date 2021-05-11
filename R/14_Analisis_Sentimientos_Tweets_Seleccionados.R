# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
#pckgs<-c("syuzhet", "lexicon","rlist","tm", "RWeka", "NLP", "openNLP", "devtools")
#pckgs<-c("rlist")
#pckgs<-c("devtools")
pckgs<-c("lexicon", "syuzhet", "ggplot2", "parallel", "slam", 
         "dplyr", "stringr", "data.table", "qdapRegex", "qdap",
         "tm", "wordcloud", "RWeka", "spatstat", "rlist", "xlsx")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}


# Establir directori de treball
#Variables globales
pathorigin<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
pathsortida<-paste(pathorigin,"/Resultats Sentiment Analysis",sep="")
setwd(pathorigin)


load("Corpus_CyClCm_f.RData")
load("CyClCm_ff.RData")
setwd("D:/Resultats 15cores")
load("Bestskm_100clusters.RData")
#load("20160720_skmeans_CyClCm_wCh_woNo_k150f30r170best.RData")
km_result<-bestskm[[1]]
setwd(pathorigin)

idmyCorpus<-selec_tweets$id

NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")

#Cargamos el resultado de la categorizacion de los clusters
setwd(pathorigin)
load("Clusters_por_categoria.RData")

#Directorio de trabajo
path<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/"
setwd(path)



#-------------------------------------------------------------------------------------------


#trabajamos con los tweets y el lexicon final
file<-"CyClCm_ff.RData"
load(file)
file<-"Sentiword_mod.RData"
load(file)
load("Clusters_por_categoria.RData")
load("Bestsskm_100clusters.RData")
load("Corpus_CyClCm_f.RData")
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")

km_result<-bestskm[[1]]
setwd(pathorigin)
idmyCorpus<-selec_tweets$id

for(j in 1:6){
  categoria<-j
  seleccion<-which(clusters_catmax==categoria)
  
  corpus_docs<-unlist(lapply(myCorpus,as.character))
  names(corpus_docs)<-idmyCorpus
  texts_cl<-NULL
  for(cl in seleccion) {
    docs_cl<-names(km_result$cluster)[km_result$cluster==cl]
    equal<-match(docs_cl, names(corpus_docs))
    texts_cl<-c(texts_cl,corpus_docs[equal])
  }
  
  if(j==1)
  {
    ah<-names(corpus_docs[equal])
    save(ah,filename="id_tweets_AH.Rdata")
  }  

  if(j==3)
  {
    ah<-names(corpus_docs[equal])
    save(ah,filename="id_tweets_EE.Rdata")
  }  
  
  #Comparamos los tweets con el SentiWordNet extended
  #Comparamos los tweet limpios que utilizamos en el Corpus
  lexicon<-Sentiword_mod
  char_v<-texts_cl
  word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")
  
  #Retorna vector logico de las palabras de los tweets
  #que estÃ¡n en el lexicon
  no_cores <- detectCores()-1
  cl <- makeCluster(no_cores, type="PSOCK")
  clusterExport(cl, c("lexicon","word_l", "unlist"))
  data_tweets <- parLapply(cl,word_l, function(x) (unlist(x) %in% lexicon$Terms))
  stopCluster(cl)
  
  filename<-paste("Tweetslimpios_Cat_",names_long_cats[j],"_SentiWordExt.RData", sep="")
  save(data_tweets,file=filename)

}  
  
#Deteccion de palabras totales y de palabras reconocidas
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")

for (j in 1:6){
  setwd(pathorigin)
  filename<-paste("Tweetslimpios_Cat_",names_long_cats[j],"_SentiWordExt.RData", sep="")
  load(filename)
  
  df<-NULL
  
  Words_per_tweet<-unlist(lapply(seq(1:length(data_tweets)),function(x) length(data_tweets[[x]])))
  words_detected<-unlist(lapply(seq(1:length(data_tweets)),function(x) sum(data_tweets[[x]])))
  percent_detected<- words_detected/Words_per_tweet  
  
  setwd(pathsortida)
  
  filename<-paste("Hist_", names_long_cats[j],"_percent_words_detected.png", sep="")
  png(filename, units="mm", width=210, height=100, res=300)
  hist(percent_detected, main=paste(names_long_cats[j], " Percent words detected", sep=""))
  dev.off()
  
  total_words_per_tweet<-sum(Words_per_tweet)
  stat_words_per_tweet<-summary(Words_per_tweet)
  total_words_detected<-sum(words_detected)
  tota_percent_detected<-sum(words_detected)/sum(Words_per_tweet)
  df<-rbind(total_words_detected,total_words_per_tweet,tota_percent_detected)
  
  filename<-paste("Stats_", names_long_cats[j],"_percent_words_detected.xlsx", sep="")
  write.xlsx2(df, file=filename, sheetName="Words detected",
              col.names=TRUE, row.names=TRUE, append=FALSE, password=NULL)
  write.xlsx2(as.array(stat_words_per_tweet), file=filename, sheetName="Stats Words detected",
              col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
  
}  


#Retorna las palabras del lexicon que coinciden con la de los tweets 
#con toda la info del lexicon
for(j in 1:6){
  categoria<-j
  seleccion<-which(clusters_catmax==categoria)
  
  corpus_docs<-unlist(lapply(myCorpus,as.character))
  names(corpus_docs)<-idmyCorpus
  texts_cl<-NULL
  for(cl in seleccion) {
    docs_cl<-names(km_result$cluster)[km_result$cluster==cl]
    equal<-match(docs_cl, names(corpus_docs))
    texts_cl<-c(texts_cl,corpus_docs[equal])
  }
  
  #Comparamos los tweets con el SentiWordNet extended
  #Comparamos los tweet limpios que utilizamos en el Corpus
  lexicon<-Sentiword_mod
  char_v<-texts_cl
  word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")


  no_cores <- detectCores()-1
  cl <- makeCluster(no_cores, type="PSOCK")
  clusterExport(cl, c("lexicon","word_l"))
  data_dict <- parLapply(cl,word_l, function(x) which(lexicon$Terms %in% x))
  stopCluster(cl)
  
  filename<-paste("Tweetslimpios_",names_long_cats[j],"_en_SentiWordExt.RData", sep="")
  save(data_dict,file=filename)

}    

#Cargamos resultados para calcular polaridades y objectividad de los tweets
setwd(pathorigin)
file<-"Sentiword_mod.RData"
load(file)
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")

for (j in 1:6){
  
  setwd(pathorigin)
  filename<-paste("Tweetslimpios_",names_long_cats[j],"_en_SentiWordExt.RData", sep="")
  load(filename)
  sentiment_SWN<-NULL
  
  SWN_tweets<-lapply(seq(1:length(data_dict)), function(x) Sentiword_mod[data_dict[[x]],])
  pol_tweets<-unlist(lapply(seq(1:length(SWN_tweets)), function(x) sum(SWN_tweets[[x]]$Pol)))
  obj_tweets<-unlist(lapply(seq(1:length(SWN_tweets)), function(x) sum(SWN_tweets[[x]]$Obj)))
  sum_pos_tweets<-unlist(lapply(seq(1:length(SWN_tweets)), function(x) sum(SWN_tweets[[x]]$PosScore)))
  sum_neg_tweets<-unlist(lapply(seq(1:length(SWN_tweets)), function(x) sum(SWN_tweets[[x]]$NegScore)))
  Words_per_tweet<-unlist(lapply(seq(1:length(data_dict)),function(x) length(data_dict[[x]])))
  #cuidado que words_pos_tweets y words_neg_tweets cuenta las palabras que tienen polaridad
  # positiva o negativa pero no su valor
  words_pos_tweets<-unlist(lapply(seq(1:length(SWN_tweets)), function(x) sum(SWN_tweets[[x]]$PosScore>0)))
  words_neg_tweets<-unlist(lapply(seq(1:length(SWN_tweets)), function(x) sum(SWN_tweets[[x]]$NegScore>0)))
  #Tweets que tienen igual valor positivo o negativo
  equal_score<-which(sum_pos_tweets==sum_neg_tweets)
  eq_score<-logical(length=length(SWN_tweets))
  eq_score[equal_score]<-TRUE
  #Twwets que tienen igual valor positivo o negativo Y que pos o negScore=0
  zero_score<-which(eq_score==TRUE & sum_pos_tweets==0)
  zer_score<-logical(length=length(SWN_tweets))
  zer_score[zero_score]<-TRUE
  
  sentiment_SWN<-cbind.data.frame(pol_tweets,obj_tweets, sum_pos_tweets,sum_neg_tweets, 
                                  Words_per_tweet, words_pos_tweets, words_neg_tweets,
                                  eq_score, zer_score)
  rownames(sentiment_SWN)<-names(data_dict[])
  
  setwd(pathsortida)
  filename<-paste("Polarity_Objectivity_Tweets_",names_long_cats[j],".RData", sep="")
  save(sentiment_SWN,file=filename)
}  
  


#Analizamos resultados estadisticos
#Tweets que no se ha encontrado ninguna palabra (son los que en la tabla, valor = 0)
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
setwd(pathsortida)
for (j in 1:6){
  
  filename<-paste("Polarity_Objectivity_Tweets_",names_long_cats[j],".RData", sep="")
  load(filename)
  
  filename<-paste("Stats2_", names_long_cats[j],"_percent_words_detected.xlsx", sep="")
  
  table_words<-table(sentiment_SWN$Words_per_tweet)
  write.xlsx2(table_words, file=filename, sheetName="Words per tweet detected",
              col.names=TRUE, row.names=TRUE, append=FALSE, password=NULL)

  DF <- data.frame(table_words)
  
  weighted_mean<-weighted.mean(as.integer(DF$Var1),DF$Freq)
  Weighted_median<-weighted.median(as.integer(DF$Var1),DF$Freq)
  
  pmfY <- apply(as.matrix.noquote(DF),2,as.numeric)
  tweets_not_found<-pmfY[pmfY[,1]==0,2]
  per_not_found<-tweets_not_found/sum(table_words)
  
  aux<-rbind(weighted_mean,Weighted_median,per_not_found)
  write.xlsx2(aux, file=filename, sheetName="Stats Means Median",
              col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)

  
  
  #Tweets TOTALES que tienen igual valor positivo o negativo de Pol
  table_eqscore<-table(sentiment_SWN$eq_score)
  write.xlsx2(table_eqscore, file=filename, sheetName="Total Tweets EqScore",
              col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
  

  #Tweets TOTALES que tienen igual valor positivo o negativo de Pol Y oos_score o neg_score=0
  table_eqzeroscore<-table(sentiment_SWN$zer_score)
  write.xlsx2(table_eqzeroscore, file=filename, sheetName="Total Tweets 0 EqScore",
              col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
  
  #Tweets NO VACIOS
  DF<-sentiment_SWN[sentiment_SWN$Words_per_tweet!=0,]
  #Tweets que tienen igual valor positivo o negativo de Pol
  table_nonempty_eqscore<-table(DF$eq_score)
  write.xlsx2(table_nonempty_eqscore, file=filename, sheetName="Nonempty Tweets EqScore",
              col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
  #Tweets que tienen igual valor positivo o negativo de Pol Y pos_score o neg_score=0
  table_nonempty_eqzeroscore<-table(DF$zer_score)
  write.xlsx2(table_nonempty_eqzeroscore, file=filename, sheetName="Nonempty Tweets 0 EqScore",
              col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
  
  sentiment_final_SWN<-DF
  
  #Analizamos resultados de tweets NO vACIOS y resultados estadísticos de polaridad
  pol_tweets<-sentiment_final_SWN$pol_tweets
  
  summary(pol_tweets)
  
  positive.tweets.SWN <- pol_tweets[pol_tweets>0]
  negative.tweets.SWN <- pol_tweets[pol_tweets<0]
  neutral.tweets.SWN <- pol_tweets[pol_tweets==0]
  
  category_senti_SWN <- ifelse(pol_tweets < 0, "Negatiu", 
                               ifelse(pol_tweets > 0, "Positiu", "Neutre"))
  
  Table.SWN <- data.frame(table(category_senti_SWN))
  maxim_valor<-max(Table.SWN$Freq)
  
  filename<-paste("Chart_", names_long_cats[j],"_polarity.png", sep="")
  png(filename, units="mm", width=100, height=100, res=300)
  myplot<-ggplot(Table.SWN,aes(x=category_senti_SWN,y=Freq)) + 
    geom_bar(aes(fill=category_senti_SWN),stat = "identity") + 
  ggtitle(paste("Tweets by Polarity\n", names_long_cats[j], sep=""))+
    xlab("Sentiment diccionari SentiWordNet 3.0") + ylab("Nombre de tuits") + 
    scale_y_continuous(limits = c(0,maxim_valor)) +
    scale_fill_manual(values=c("red1","#4292C6", "green1")) +
    theme_bw() + theme(legend.position='none')+
    theme(plot.title = element_text(hjust=0.5))
  print(myplot)
  dev.off()
}

#-----------------------------------------------------------------------------------------------------

#### WORDCLOUDS COMPARATIUS ####
# Positus vs Negatius
setwd(pathorigin)
load("CyClCm_20160101to20160630_ff.RData")
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
load("Sentiword_mod.RData")

for(j in 1:6){
  setwd(pathsortida)
  filename<-paste("Polarity_Objectivity_Tweets_",names_long_cats[j],".RData", sep="")
  load(filename)
  
  seleccion_tweets<-which(selec_tweets$id %in% rownames(sentiment_SWN))
  text_tweets<-selec_tweets[seleccion_tweets,]$txtc
  
  negative.tweets<-text_tweets[which(sentiment_SWN$pol_tweets<0)]
  positive.tweets<-text_tweets[which(sentiment_SWN$pol_tweets>0)]
  negativos <- paste(negative.tweets, collapse = " ")
  positivos <- paste(positive.tweets, collapse = " ")
  comparar <- c(positivos, negativos)
  corpus.comparar <- Corpus(VectorSource(comparar))
  tdm.comparar <- as.matrix(TermDocumentMatrix(corpus.comparar))
  colnames(tdm.comparar) <- c("Tuits Positius", "Tuits Negatius")
  
  filename<-paste("WCcomparatius_", names_long_cats[j],"_Unigrams.png", sep="")
  png(filename, units="mm", width=210, height=210, res=300)
  comparison.cloud(tdm.comparar,random.order = FALSE, max.words = 200,
                   title.size = 1)
  dev.off()
  
  words<-rownames(tdm.comparar)
  pos_words_swn<-match(words,Sentiword_mod$Terms)
  tdm.SWN.words<-as.data.frame(tdm.comparar)
  tdm.SWN.words<-cbind(tdm.SWN.words,pos_words_swn)
  senti.words.positive<-Sentiword_mod[tdm.SWN.words$pos_words_swn,"PosScore"]
  senti.words.negative<-Sentiword_mod[tdm.SWN.words$pos_words_swn,"NegScore"]
  tdm.SWN.words<-cbind(tdm.SWN.words,senti.words.positive)
  tdm.SWN.words<-cbind(tdm.SWN.words,senti.words.negative)
  colnames(tdm.SWN.words)<-c("Tuits Positius", "Tuits Negatius", "Pos Sentimod", "PosScore", "NegScore")
  tdm.SWN.words<-cbind(tdm.SWN.words,tdm.SWN.words$`Tuits Positius`*tdm.SWN.words$PosScore)
  tdm.SWN.words<-cbind(tdm.SWN.words,tdm.SWN.words$`Tuits Negatius`*tdm.SWN.words$NegScore)
  colnames(tdm.SWN.words)<-c("Tuits Positius", "Tuits Negatius", "Pos Sentimod", 
                             "PosScore", "NegScore", "WPosScore", "WNegScore")
  small.tdm<-tdm.SWN.words[which(tdm.SWN.words$`Pos Sentimod`!="NA"),c("WPosScore", "WNegScore")]
  
  filename<-paste("WCcomparatius_score_weigthed", names_long_cats[j],"_Unigrams.png", sep="")
  png(filename, units="mm", width=210, height=210, res=300)
  comparison.cloud(small.tdm,random.order = FALSE, max.words = 200,
                   title.size = 1)
  dev.off()
  

  #### WORDCLOUDS DE 2 PARAULES ##### 
  NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  
  ## Tuits negatius
  tweets.negativos <- negative.tweets
  tweets.negativos <- gsub("\\<\\w{0,2}\\>"," ", tweets.negativos) # paraules de dos caracters o menys
  tweets.negativos <- paste(tweets.negativos,collapse=" ")
  corpus_negative <- VCorpus(VectorSource(tweets.negativos))
  tdm_negative <- TermDocumentMatrix(corpus_negative, control=list(tokenize=NumbergramTokenizer))
  frequent_neg <- findMostFreqTerms(tdm_negative, n = 100)
  
  #windows()
  # wordcloud(names(frequent_neg$`1`),frequent_neg$`1`,rot.per=0,
  #          colors=brewer.pal(4,"Spectral"),
  #          scale=c(2,.5),random.order = FALSE)
  
  
  filename<-paste("WC_Bigrams_",names_long_cats[j],"_Neg.png", sep="")
  png(filename, units="mm", width=210, height=100, res=300)
  wordcloud(names(frequent_neg$`1`),frequent_neg$`1`,rot.per=0,
            colors=brewer.pal(4,"Spectral"),
            scale=c(2,.5),random.order = FALSE)
  dev.off()
  

  
  ## Tuits positius
  tweets.positivos <- positive.tweets
  tweets.positivos <- gsub("\\<\\w{0,2}\\>"," ", tweets.positivos) # paraules de dos caracters o menys
  tweets.positivos <- paste(tweets.positivos,collapse=" ")
  corpus_positive <- VCorpus(VectorSource(tweets.positivos))
  tdm_positive <- TermDocumentMatrix(corpus_positive, control=list(tokenize=NumbergramTokenizer))
  frequent_pos <- findMostFreqTerms(tdm_positive, n = 100)
 
  filename<-paste("WC_Bigrams_",names_long_cats[j],"_Pos.png", sep="")
  mypalette<-c("aquamarine4", "blue2", "blue3", "blue4")
  png(filename, units="mm", width=210, height=100, res=300)
  wordcloud(names(frequent_pos$`1`),frequent_pos$`1`,rot.per=0,
            colors=mypalette,
            scale=c(2,.5),random.order = FALSE)
  dev.off()
  

}  


#-----------------------------------------------------------------------------------------------

#Wordclouds de tweets con polaridad 0 Unigramas y Bigramas
setwd(pathorigin)
load("CyClCm_20160101to20160630_ff.RData")
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
load("Sentiword_mod.RData")

for(j in 1:6){
  setwd(pathsortida)
  filename<-paste("Polarity_Objectivity_Tweets_",names_long_cats[j],".RData", sep="")
  load(filename)
  
  df<-sentiment_SWN  
  
  seleccion_tweets<-which(selec_tweets$id %in% rownames(df[which(df$pol_tweets==0),]))
  neutral.tweets<-selec_tweets[seleccion_tweets,"txtc"]
  names(neutral.tweets)<-selec_tweets[seleccion_tweets,"id"]
  
  neutros <- paste(neutral.tweets, collapse = " ")
  
  corpus.neutros <- Corpus(VectorSource(neutros))
  tdm.neutros <- as.matrix(TermDocumentMatrix(corpus.neutros))
  colnames(tdm.neutros) <- c("Neutral_Tweets")
  
  filename<-paste("WCNeutros_", names_long_cats[j],"_Unigrams.png", sep="")
  png(filename, units="mm", width=420, height=420, res=300)
  
  plot.new()
  wordcloud(words=rownames(tdm.neutros),tdm.neutros,random.order = FALSE, rot.per=0,max.words = 200)
  dev.off()
  
  library(RWeka)
  corpus.neutros <- VCorpus(VectorSource(neutros))
  NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  tdm.comparar <- as.matrix(TermDocumentMatrix(corpus.neutros, control=list(tokenize=NumbergramTokenizer)))
  
  colnames(tdm.comparar) <- c("Neutral_Tweets")
  
  filename<-paste("WCNeutros_", names_long_cats[j],"_Bigrams.png", sep="")
  png(filename, units="mm", width=420, height=420, res=300)

  plot.new()
  wordcloud(words=rownames(tdm.comparar),tdm.comparar,random.order = FALSE, rot.per=0,max.words = 200)
  dev.off()
  
  
}

  
