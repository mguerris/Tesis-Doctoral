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

#funcion global para pintar las emociones
grafemociones<-function(titulo, emotions, tityaxis){
  chart<-ggplot(data=emotions,aes(x=names,y=count)) + 
    geom_bar(aes(fill=names),stat = "identity") +
    xlab("Emoción expresada") + ylab(tityaxis) +
    scale_fill_manual(values=c("tomato","firebrick1","red","darkred","green",
                               "limegreen","springgreen4","darkgreen")) +
    geom_text(aes(label=round(emotions$count), vjust=-0.3, size=3.5))+
    theme_bw() + theme(legend.position='none') +
    theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                    hjust = 0.5,color = "black"))+
    ggtitle(titulo)
  return(chart)
}

# Establir directori de treball
#Variables globales
pathorigin<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
pathorigin<-"C:/Users/mguerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
setwd(pathorigin)

#emocions de NRC
#devuelve si un vector de cadena de palabras estÃ¡ cada cadena de palabras asociada o no
#a una emocion a partir del lexicon nrc
#Ojo como funciona
#Si dos palabras son iguales solo considera una de las dos iguales para devolver la asociacion al sentimiento
#Si son diferentes acumula las emociones, las suma segun la emocion de esa palabra
#con la funcion get_nrc_sentiment si una cadena esta vacia la cuenta como 0, pero la cuenta!
#Si un tweet (cadena de palabras) tiene varias palabras iguales NO acumulara los sentimientos!!!!!
#Asimismo si esta vacio lo contara como 0, pero lo contara
#https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#http://sentiment.nrc.ca/lexicons-for-research/

#por tanto podriamos analizar:
#1-NÂº de tweets asociados a cada emocion 
#2-NÂº de tweets si son positivos o negativos (esta al final del dataframe de las emociones)
#3-Polaridad de los tweets (positive - negative) ojo que son nÂº de palabras
#3-Valores de emociones ?
#4-emociones de bigramas (a partir de las palabras) y relacionarlo con la frecuencia?
#5-con SWN, con SLN, con Apache (con todas las palabras o con adjetivos solo)



#Calcula les emocions de cada tweet paraula a paraula
#Si no el get_nrc si té paraules repetides nomès té en compte una de les dues
load("CyClCm_ff.RData")
tweets<-selec_tweets$txtc
names(tweets)<-selec_tweets$id
word_l<-word_split(tweets)
ntweets<-length(tweets)

#ntweets<-100

no_cores <- detectCores()-1
clus <- makeCluster(no_cores, type="PSOCK")
list_words_emotions<-lapply(seq(1:ntweets), function(x) get_nrc_sentiment(word_l[[x]],cl=clus,language = "english"))
stopCluster(clus)

#ejemplo_tweet<-list_words_emotions[[10]]
#rownames(ejemplo_tweet)<-word_l[[10]]
#xlsx::write.xlsx2(ejemplo_tweet, file="Ejemplo_tweet_emocion.xlsx") 


matrix_emotions<-sapply(seq(1:ntweets),function(x) colSums(list_words_emotions[[x]]))
emotions<-as.data.frame(t(matrix_emotions))
rownames(emotions)<-selec_tweets$id

save(emotions,file="Tweets_Emotions_NRC_raw_perword.RData")
load("Tweets_Emotions_NRC_raw_perword.RData")


#--------------------------------------------------------------------------------------------------
load("CyClCm_ff.RData")
tweets<-selec_tweets$txtc
word_l<-word_split(tweets)
available_data('nrc_emotions')
lexicon<-nrc_emotions$term

char_v<-tweets
word_l <- strsplit(tolower(char_v), "[^A-Za-z']+")

no_cores <- detectCores()-1
cl <- makeCluster(no_cores, type="PSOCK")
clusterExport(cl, c("lexicon","word_l"))
data_dict <- parLapply(cl,word_l, function(x) which(lexicon %in% x))
stopCluster(cl)

filename<-paste("Words_detected_by_tweet_in_NRC.RData", sep="")
save(data_dict,file=filename)

load("Words_detected_by_tweet_in_NRC.RData")
words_detected<-sapply(seq(1:length(data_dict)), function(x) length(data_dict[[x]]))

setwd(pathsortida)

#--------------------------------------------------------------------------------------------------

#Calculo extendido (polaridad y porcentaje de palabras aproximado reconocido) 
#de emociones y estadisticas de todos los tweets
load("Tweets_Emotions_NRC_raw_perword.RData")
load("CyClCm_ff.RData")

emotion.df<-emotions

tweets<-selec_tweets$txtc
word_l<-word_split(tweets)
nword_l<-sapply(word_l, function(x) length(x))
names(nword_l)<-selec_tweets$id

emotion.df$Global <- emotion.df[10]-emotion.df[9]
emotion.df$Suma<-rowSums(emotion.df[,1:8])
emotion.df$Nwords<-nword_l
emotion.df$Porcentaje<-emotion.df$Suma/emotion.df$Nwords
colnames(emotion.df)<-c('Ira','Anticipacion','Disgusto','Miedo',
                        'Alegria','Tristeza','Sorpresa','Confianza',
                        'Negativo','Positivo','Polaridad','Suma',
                        'Palabras','Porcentaje_palabras')
for (i in 1:8) {
  emotion.df[,i] <- round(emotion.df[,i]/emotion.df$Suma,2)
}
emotion.df[emotion.df=="NaN"] <- as.numeric('0')

save(emotion.df,file="Tweets_Emotions_NRC_perword_modified.RData")

load("Tweets_Emotions_NRC_perword_modified.RData")
emotion.df$words_detected=words_detected
emotion.df$Porcentaje_palabras<-emotion.df$words_detected/emotion.df$Palabras
summary(emotion.df$Porcentaje_palabras)
save(emotion.df,file="Tweets_Emotions_NRC_perword_modified.RData")

setwd(pathsortida)
load("Tweets_Emotions_NRC_perword_modified.RData")

#incorporacion en el dataframe de los tweets no duplicados y la categoria de cada tweet
pathorigin<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
setwd(pathorigin)
load("Tweets_Emotions_NRC_perword_modified.RData")
load("Tweets con palabras no duplicadas.RData")
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")


pos_no_dupli<-match(tweets_no_dupli,rownames(emotion.df))
emotion.df$Duplicado<-TRUE
emotion.df[pos_no_dupli,"Duplicado"]<-FALSE
emotion.df$Categoria<-0

for(j in 1:6){
  #j<-1
  pathsortida<-paste(pathorigin,"/Resultats Sentiment Analysis",sep="")
  setwd(pathsortida)
  filename<-paste("Polarity_Objectivity_Tweets_",names_long_cats[j],".RData", sep="")
  load(filename)
  
  seleccion_tweets<-which(rownames(emotion.df) %in% rownames(sentiment_SWN))
  emotion.df[seleccion_tweets,"Categoria"]<-j
  
  
  
}  

pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
setwd(pathsortida)
save(emotion.df,file="Tweets_Emotions_NRC_perword_modified.RData")
#-------------------------------------------------------------------------------------------------

pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
setwd(pathsortida)
load("Tweets_Emotions_NRC_perword_modified.RData")



#Estadisticas AH y EE
ah<-emotion.df[emotion.df$Categoria==1,]
summary(ah$Palabras)
summary(ah$words_detected)
summary(ah$Porcentaje_palabras)
table(ah$Duplicado)

ah_emocion<-ah[,1:8]

pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")

setwd(pathsortida)
save(ah_emocion, file="Emociones_AH_perword.RData")
save(ah, file="Emociones_AH_extended_perword.RData")

ee<-emotion.df[emotion.df$Categoria==3,]
summary(ee$Palabras)
summary(ee$words_detected)
summary(ee$Porcentaje_palabras)
table(ee$Duplicado)

ee_emocion<-ee[,1:8]
save(ee_emocion, file="Emociones_EE_perword.RData")
save(ee, file="Emociones_EE_extended_perword.RData")

#--------------------------------------------------------------------------------------------

#pintamos Emociones de Actividad Humana y de Entorno Educativo
pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
setwd(pathsortida)
load("Emociones_EE_extended_perword.RData")
load("Emociones_AH_extended_perword.RData")


#AH
#Pintamos todos con los tweets con palabras duplicadas
dforder<-c(1,5,2,3,7,4,6,8)
emotions<-data.frame("count"=colSums(ah[,c(1:8)]))
emotions<-cbind(emotions,rownames(emotions))
emotions<-data.frame(emotions[order(dforder),])
colnames(emotions)<-c("count","names")
emotions$names <- factor(emotions$names, levels = emotions$names)

titulo<-""
#windows()
graf<-grafemociones(titulo,emotions,"Nº de tweets temática AH")
#print(graf)
file<-paste("Emociones_AH_perword.png",sep="")
ggsave(filename = file,plot=graf)


#Pintamos las emociones sin los tweets con palabras duplicadas
ah_nodupli<-ah[ah$Duplicado==FALSE,]
emotions<-data.frame("count"=colSums(ah_nodupli[,c(1:8)]))
emotions<-cbind(emotions,rownames(emotions))
emotions<-data.frame(emotions[order(dforder),])
colnames(emotions)<-c("count","names")
emotions$names <- factor(emotions$names, levels = emotions$names)
titulo<-""
#windows()
graf<-grafemociones(titulo,emotions,"Nº de tweets sin palabras duplicadas temática AH")
#print(graf)
file<-paste("Emociones_AH_nodupli_perword.png",sep="")
ggsave(filename = file,plot=graf)

#EE
#Pintamos las emociones de todos los tweets con palabras duplicadas y no duplicadas
dforder<-c(1,5,2,3,7,4,6,8)
emotions<-data.frame("count"=colSums(ee[,c(1:8)]))
emotions<-cbind(emotions,rownames(emotions))
emotions<-data.frame(emotions[order(dforder),])
colnames(emotions)<-c("count","names")
emotions$names <- factor(emotions$names, levels = emotions$names)

titulo<-""
#windows()
graf<-grafemociones(titulo,emotions,"Nº de tweets temática EE")
#print(graf)
file<-paste("Emociones_EE_perword.png",sep="")
ggsave(filename = file,plot=graf)

#Pintamos las emociones de todos los tweets SIN palabras duplicadas
ee_nodupli<-ee[ee$Duplicado==FALSE,]
dforder<-c(1,5,2,3,7,4,6,8)
emotions<-data.frame("count"=colSums(ee_nodupli[,c(1:8)]))
emotions<-cbind(emotions,rownames(emotions))
emotions<-data.frame(emotions[order(dforder),])
colnames(emotions)<-c("count","names")
emotions$names <- factor(emotions$names, levels = emotions$names)

titulo<-""
#windows()
graf<-grafemociones(titulo,emotions,"Nº de tweets sin palabras duplicadas temática EE")
#print(graf)
file<-paste("Emociones_EE_perword_nodupli.png",sep="")
ggsave(filename = file,plot=graf)





#---------------------------------------------------------------------------------
#Miramos en los tweets, en cada tweet cuantas palabras existen duplicadas
load("CyClCm_ff.RData")

tweets<-selec_tweets$txtc
word_l<-word_split(tweets)

dupli_l<-lapply(seq(1:length(word_l)), function(x) duplicated(word_l[[x]]))
dupli_resul<-unlist(lapply(seq(1:length(word_l)), function(x) any(duplicated(word_l[[x]]))))
names(dupli_resul)<-selec_tweets$id
tweets_no_dupli<-names(dupli_resul[dupli_resul!=TRUE])
n_dupli<-lapply(seq(1:length(word_l)), function(x) sum(dupli_l[[x]]))

df_ndupli<-as.data.frame(table(unlist(n_dupli)))
colnames(df_ndupli)<-c("NPalabras Diferentes Duplicadas", "NTweets")
porcentaje_tweets_dupli<-sum(df_ndupli[2:13,2])/sum(df_ndupli[1:13,2])
porcentaje_words_dupli<-sum(unlist(n_dupli))/length(unlist(dupli_l))

words_per_tweet<-lapply(seq(1:length(word_l)), function(x) length(word_l[[x]]))
Hmisc::describe(unlist(words_per_tweet))
df_nwpt<-as.data.frame(table(unlist(words_per_tweet)))

distr_dupli<-lapply(seq(1:length(word_l)), function(x) sum(dupli_l[[x]])/length(word_l[[x]]))
windows()
hist(unlist(distr_dupli))
Hmisc::describe(unlist(distr_dupli))

save(tweets_no_dupli,file="Tweets con palabras no duplicadas.RData")

#-----------------------------------------------------------------------------------------

#Calculo de emociones de todos los tweets
load("CyClCm_20160101to20160630_ff.RData")
load("CyClCm_20160101to20160630_ff.RData")

tweets<-selec_tweets$txtc
names(tweets)<-selec_tweets$id

no_cores <- detectCores()-1
clus <- makeCluster(no_cores, type="PSOCK")
emotion.df<-get_nrc_sentiment(tweets,cl=clus,language = "english")
stopCluster(clus)

save(emotion.df,file="Tweets_Emotions_NRC_raw.RData")
setwd(pathorigin)
load("Tweets_Emotions_NRC_raw.RData")

#Calculo extendido (polaridad y porcentaje de palabras aproximado reconocido) 
#de emociones y estadisticas de todos los tweets
load("Tweets_Emotions_NRC_raw.RData")
load("CyClCm_ff.RData")

tweets<-selec_tweets$txtc
word_l<-word_split(tweets)
nword_l<-sapply(word_l, function(x) length(x))
names(nword_l)<-selec_tweets$id

emotion.df$Global <- emotion.df[10]-emotion.df[9]
emotion.df$Suma<-rowSums(emotion.df[,1:8])
emotion.df$Nwords<-nword_l
emotion.df$Porcentaje<-emotion.df$Suma/emotion.df$Nwords
colnames(emotion.df)<-c('Ira','Anticipacion','Disgusto','Miedo',
                        'Alegria','Tristeza','Sorpresa','Confianza',
                        'Negativo','Positivo','Polaridad','Suma',
                        'Palabras','Porcentaje_palabras')
for (i in 1:8) {
  emotion.df[,i] <- round(emotion.df[,i]/emotion.df$Suma,2)
}
emotion.df[emotion.df=="NaN"] <- as.numeric('0')

save(emotion.df,file="Tweets_Emotions_NRC_modified.RData")

#incorporacion en el dataframe de los tweets no duplicados y la categoria de cada tweet
pathorigin<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
setwd(pathorigin)
load("Tweets_Emotions_NRC_modified.RData")
load("Tweets con palabras no duplicadas.RData")
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")


pos_no_dupli<-match(tweets_no_dupli,rownames(emotion.df))
emotion.df$Duplicado<-TRUE
emotion.df[pos_no_dupli,"Duplicado"]<-FALSE
emotion.df$Categoria<-0

for(j in 1:6){
  #j<-1
  pathsortida<-paste(pathorigin,"/Resultats Sentiment Analysis",sep="")
  setwd(pathsortida)
  filename<-paste("Polarity_Objectivity_Tweets_",names_long_cats[j],".RData", sep="")
  load(filename)
  
  seleccion_tweets<-which(rownames(emotion.df) %in% rownames(sentiment_SWN))
  emotion.df[seleccion_tweets,"Categoria"]<-j

  
  
}  

#------------------------------------------------------------------------------------------

#Estadisticas AH y EE
ah<-emotion.df[emotion.df$Categoria==1,]
summary(ah$Palabras)
summary(ah$Porcentaje_palabras)
table(ah$Duplicado)

ah_emocion<-ah[,1:8]

pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")

setwd(pathsortida)
save(ah_emocion, file="Emociones_AH.RData")
save(ah, file="Emociones_AH_extended.RData")

ee<-emotion.df[emotion.df$Categoria==3,]
summary(ee$Palabras)
summary(ee$Porcentaje_palabras)
table(ee$Duplicado)

ee_emocion<-ee[,1:8]
save(ee_emocion, file="Emociones_EE.RData")
save(ee, file="Emociones_EE_extended.RData")


pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")

setwd(pathsortida)
save(emotion.df,file="Tweets_Emotions_NRC_modified.RData")


#--------------------------------------------------------------------------------------------

#pintamos Emociones de Actividad Humana y de Entorno Educativo
pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
setwd(pathsortida)
load("Emociones_EE_extended.RData")
load("Emociones_AH_extended.RData")


#AH
#Pintamos todos con los tweets con palabras duplicadas
dforder<-c(1,5,2,3,7,4,6,8)
emotions<-data.frame("count"=colSums(ah[,c(1:8)]))
emotions<-cbind(emotions,rownames(emotions))
emotions<-data.frame(emotions[order(dforder),])
colnames(emotions)<-c("count","names")
emotions$names <- factor(emotions$names, levels = emotions$names)

titulo<-""
#windows()
graf<-grafemociones(titulo,emotions,"Nº de tweets temática AH")
#print(graf)
file<-paste("Emociones_AH.png",sep="")
ggsave(filename = file,plot=graf)


#Pintamos las emociones sin los tweets con palabras duplicadas
ah_nodupli<-ah[ah$Duplicado==FALSE,]
emotions<-data.frame("count"=colSums(ah_nodupli[,c(1:8)]))
emotions<-cbind(emotions,rownames(emotions))
emotions<-data.frame(emotions[order(dforder),])
colnames(emotions)<-c("count","names")
emotions$names <- factor(emotions$names, levels = emotions$names)
titulo<-""
#windows()
graf<-grafemociones(titulo,emotions,"Nº de tweets sin palabras duplicadas temática AH")
#print(graf)
file<-paste("Emociones_AH_nodupli.png",sep="")
ggsave(filename = file,plot=graf)

#EE
#Pintamos las emociones de todos los tweets con palabras duplicadas y no duplicadas
dforder<-c(1,5,2,3,7,4,6,8)
emotions<-data.frame("count"=colSums(ee[,c(1:8)]))
emotions<-cbind(emotions,rownames(emotions))
emotions<-data.frame(emotions[order(dforder),])
colnames(emotions)<-c("count","names")
emotions$names <- factor(emotions$names, levels = emotions$names)

titulo<-""
#windows()
graf<-grafemociones(titulo,emotions,"Nº de tweets temática EE")
#print(graf)
file<-paste("Emociones_EE.png",sep="")
ggsave(filename = file,plot=graf)

#Pintamos las emociones de todos los tweets SIN palabras duplicadas
ee_nodupli<-ee[ee$Duplicado==FALSE,]
dforder<-c(1,5,2,3,7,4,6,8)
emotions<-data.frame("count"=colSums(ee_nodupli[,c(1:8)]))
emotions<-cbind(emotions,rownames(emotions))
emotions<-data.frame(emotions[order(dforder),])
colnames(emotions)<-c("count","names")
emotions$names <- factor(emotions$names, levels = emotions$names)

titulo<-""
#windows()
graf<-grafemociones(titulo,emotions,"Nº de tweets sin palabras duplicadas temática EE")
#print(graf)
file<-paste("Emociones_EE_nodupli.png",sep="")
ggsave(filename = file,plot=graf)



windows()
tityaxis<-""
ggplot(data=emotions,aes(x=names,y=count)) + 
  geom_bar(aes(fill=names),stat = "identity") +
  xlab("Emocion expresada") + ylab(tityaxis) +
  scale_fill_manual(values=c("tomato","firebrick1","red","darkred","skyblue1","skyblue3",
                             "green","green4")) +
  geom_text(aes(label=round(emotions$count), vjust=-0.3, size=3.5))+
  theme_bw() + theme(legend.position='none') +
  theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                  hjust = 0.5,color = "black"))+
  ggtitle(titulo)

pl(chart)

#Emociones de todos los tweets, graficos y estadisticas
#incluidos aquellos que tienen palabras duplicadas
#pero solo de los tweets 
setwd(pathsortida)
load("Tweets_Emotions_NRC_modified.RData")
setwd(pathorigin)
load("CyClCm_ff.RData")


#Solo trabajamos con los clusterizados
em_real<-emotion.df[!is.na(emotion.df$Categoria),]

list_df<-list()
list_tit<-list()

#emociones de tweets clusterizados y con palabras duplicadas
emotions<-data.frame("count"=colSums(em_real[,c(1:8)]))
titulo<-"Tweets clusterizados y con palabras duplicadas"
list_df<-list.append(list_df,emotions)
list_tit<-list.append(list_tit,titulo)

#emociones de tweets clusterizados y sin palabras duplicadas
em_nodupli<-em_real[em_real$Duplicado==FALSE,]
emotions<-data.frame("count"=colSums(em_nodupli[,c(1:8)]))
titulo<-"Tweets clusterizados y SIN palabras duplicadas"
list_df<-list.append(list_df,emotions)
list_tit<-list.append(list_tit,titulo)



#emociones de tweets clusterizados y sin palabras duplicadas
#por categoria
for (categoria in 1:6){
  em_cat<-em_nodupli[em_nodupli$Categoria==categoria,]
  emotions<-data.frame("count"=colSums(em_cat[,c(1:8)]))
  titulo<-paste("Tweets clusterizados y SIN palabras duplicadas", names_long_cats[categoria],sep="\n")
  list_df<-list.append(list_df,emotions)
  list_tit<-list.append(list_tit,titulo)
}



setwd(pathsortida)
for (i in 1:length(list_df)){

  emotions <-list_df[[i]]
  titulo<-list_tit[[i]]
  
  #windows()
  chart<-ggplot(emotions,aes(x=rownames(emotions),y=emotions$count)) + 
    geom_bar(aes(fill=rownames(emotions)),stat = "identity") +
    xlab("EmociÃ³n expresada") + ylab("Numero de tweets") +
    scale_fill_manual(values=c("yellow","chocolate1","green1","magenta1","red1","green4",
                               "cyan1","blue1")) +
    theme_bw() + theme(legend.position='none') +
    theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                    hjust = 0.5,color = "black"))+
    ggtitle(titulo)
  #print(chart)
  titulo<-gsub("[\r\n]", "", titulo)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=chart)
  
  category_senti <- ifelse(emotions$Polaridad < 0, "Negativo", ifelse(emotions$Polaridad > 0, "Positivo",
                                                             "Neutro"))
  Table <- data.frame(table(category_senti))
  #windows()
  chart<-ggplot(Table,aes(x=Table$category_senti,y=Table$Freq)) + 
    geom_bar(aes(fill=Table$category_senti),stat = "identity") +
    xlab("Polaridad") + ylab("Numero de tweets") +
    scale_fill_manual(values=c("red1","yellow","green1")) +
    theme_bw() + theme(legend.position='none') +
    theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                    hjust = 0.5,color = "black"))+
    ggtitle(titulo)
    #print(chart)
    titulo<-gsub("[\r\n]", "", titulo)
    file<-paste("Polaridad_NRC_",titulo,".png",sep="")
    ggsave(filename = file,plot=chart)
  
  Hmisc::describe(emotion.df$Porcentaje_palabras)
}


#-------------------------------------------------------------------------------------------------
#A?adimos a las emotions el calculo de los sentimientos positivos o negativos
#con lo que hab?amos calculado los wordclouds comparativos
setwd(pathsortida)
load("Tweets_Emotions_NRC_perword_modified.RData")
pathsortidassentiment<-paste(pathorigin,"/Resultats Sentiment Analysis",sep="")
setwd(pathsortidassentiment)
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")

sentiment.df<-NULL
sentemotion.df<-NULL
for(j in 1:6){
  filename<-paste("Polarity_Objectivity_Tweets_",names_long_cats[j],".RData", sep="")
  load(filename)
  sentiment.df<-rbind(sentiment.df, sentiment_SWN)
}
seleccion_tweets<-which(rownames(emotion.df) %in% rownames(sentiment.df))
sentemotion.df<-cbind(emotion.df[seleccion_tweets,],sentiment.df)

setwd(pathsortida)
titulo.ejey<-"Nº de tweets"
dforder<-c(1,5,2,3,7,4,6,8)

j<-1
for(j in 1:6){
  seleccion<-sentemotion.df[sentemotion.df$Categoria==j,]
  
  positivos<-seleccion[seleccion$pol_tweets>0,]
  emotions<-data.frame("count"=colSums(positivos[,c(1:8)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Positivos", sep="")
  #windows()
  if(j==1){
    titulo<-" "
    titulo.ejey<-"Nº de tweets positivos temática AH"
  }
  if(j==3){
    titulo<-" "
    titulo.ejey<-"Nº de tweets positivos temática EE"
  }
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  titulo<-paste(names_long_cats[j]," Tweets Positivos", sep="")
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  negativos<-seleccion[seleccion$pol_tweets<0,]
  emotions<-data.frame("count"=colSums(negativos[,c(1:8)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)

  
    titulo<-paste(names_long_cats[j]," Tweets Negativos", sep="")
  #windows()
  if(j==1){
    titulo<-" "
    titulo.ejey<-"Nº de tweets negativos temática AH"
  }
  if(j==3){
    titulo<-" "
    titulo.ejey<-"Nº de tweets negativos temática EE"
  }
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  titulo<-paste(names_long_cats[j]," Tweets Negativos", sep="")
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  neutros<-seleccion[seleccion$pol_tweets==0,]
  emotions<-data.frame("count"=colSums(neutros[,c(1:8)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Neutros", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
}

#-------------------------------------------------------------------------------------------------

#### EMOCIONES DE LOS WORDCLOUDS COMPARATIVOS de UNIGRAMAS ####
# Positus vs Negatius
setwd(pathorigin)
load("CyClCm_ff.RData")
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
load("Sentiword_mod.RData")

for(j in 1:6){
  pathsortida<-paste(pathorigin,"/Resultats Sentiment Analysis",sep="")
  setwd(pathsortida)
  filename<-paste("Polarity_Objectivity_Tweets_",names_long_cats[j],".RData", sep="")
  load(filename)
  
  df<-sentiment_SWN  
  
  seleccion_tweets<-which(selec_tweets$id %in% rownames(df[which(df$pol_tweets<0),]))
  negative.tweets<-selec_tweets[seleccion_tweets,"txtc"]
  names(negative.tweets)<-selec_tweets[seleccion_tweets,"id"]
  
  seleccion_tweets<-which(selec_tweets$id %in% rownames(df[which(df$pol_tweets>0),]))
  positive.tweets<-selec_tweets[seleccion_tweets,"txtc"]
  names(positive.tweets)<-selec_tweets[seleccion_tweets,"id"]
  
  negativos <- paste(negative.tweets, collapse = " ")
  positivos <- paste(positive.tweets, collapse = " ")
  comparar <- c(positivos, negativos)
  corpus.comparar <- Corpus(VectorSource(comparar))
  tdm.comparar <- as.matrix(TermDocumentMatrix(corpus.comparar))
  colnames(tdm.comparar) <- c("Positive Tweets", "Negative Tweets")
  
  #Calcula en quÃ© grupo queda cada palabra - variable group
  term.matrix<-tdm.comparar
  ndoc <- ncol(term.matrix)
  for (i in 1:ndoc) {
    term.matrix[, i] <- term.matrix[, i]/sum(term.matrix[,i])
  }
  mean.rates <- rowMeans(term.matrix)
  for (i in 1:ndoc) {
    term.matrix[, i] <- term.matrix[, i] - mean.rates
  }
  group <- as.integer(apply(term.matrix, 1, function(x) which.max(x)))
  words <- as.character(rownames(term.matrix))
  freq <- as.numeric(apply(term.matrix, 1, function(x) max(x)))
  
  words.df<-cbind.data.frame(words,freq,group)
  no_cores <- detectCores()-1
  clus <- makeCluster(no_cores, type="PSOCK")
  emotion.df<-get_nrc_sentiment(as.character(words.df$words),cl=clus,language = "english")
  stopCluster(clus)
  
  emotion.words<-cbind(words.df,emotion.df)
  
  pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
  setwd(pathsortida)
  filename<-paste(names_long_cats[j],"_Emociones_unigra_comparativos",".RData",sep="")
  save(emotion.words,file=filename)
  
  max.words<-200
  ord <- rank(-freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  group <- group[ord <= max.words]
  ord <- order(freq, decreasing = TRUE)
  words <- words[ord]
  freq <- freq[ord]
  group <- group[ord]
  
  wordcloud.words<-as.data.frame(cbind(words,freq,group))
  
  seleccion_words<-which(wordcloud.words$words %in% emotion.words$words)
  seleccion_words<-which(emotion.words$words %in% wordcloud.words$words)
  
  emotion.wordcloud<-emotion.words[seleccion_words,]
  
  pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
  setwd(pathsortida)
  filename<-paste(names_long_cats[j],"_Emociones_unigra_wordclouds",".RData",sep="")
  save(emotion.wordcloud,file=filename)
}

#Cambia columnas a castellano
setwd(pathsortida)
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
for(j in 1:6){
  filename<-paste(names_long_cats[j],"_Emociones_unigra_comparativos",".RData",sep="")
  load(filename)
  colnames(emotion.words)[4:13]<-c('Ira','Anticipacion','Disgusto','Miedo',
                                   'Alegria','Tristeza','Sorpresa','Confianza',
                                   'Negativo','Positivo')
  save(emotion.words,file=filename)
  
  filename<-paste(names_long_cats[j],"_Emociones_unigra_wordclouds",".RData",sep="")
  load(filename)
  colnames(emotion.wordcloud)[4:13]<-c('Ira','Anticipacion','Disgusto','Miedo',
                                   'Alegria','Tristeza','Sorpresa','Confianza',
                                   'Negativo','Positivo')
  save(emotion.wordcloud,file=filename)
}  

#Generamos los graficos correspondientes a los diferentes ficheros de emociones
setwd(pathsortida)
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
dforder<-c(1,5,2,3,7,4,6,8)
titulo.ejey<-"No de palabras"

for(j in 1:6){
  
  filename<-paste(names_long_cats[j],"_Emociones_unigra_comparativos",".RData",sep="")
  load(filename)
  
  positivos<-emotion.words[emotion.words$group==1,]
  emotions<-data.frame("count"=colSums(positivos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Positivos Unigramas", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  negativos<-emotion.words[emotion.words$group==2,]
  emotions<-data.frame("count"=colSums(negativos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Negativos Unigramas", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  filename<-paste(names_long_cats[j],"_Emociones_unigra_wordclouds",".RData",sep="")
  load(filename)

  positivos<-emotion.wordcloud[emotion.wordcloud$group==1,]
  emotions<-data.frame("count"=colSums(positivos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Positivos Unigramas_WC", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  negativos<-emotion.wordcloud[emotion.wordcloud$group==2,]
  emotions<-data.frame("count"=colSums(negativos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Negativos Unigramas_WC", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
}  



#Generamos los graficos correspondientes a los diferentes ficheros de emociones
#Unigramas PONDERADOS
setwd(pathsortida)
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
dforder<-c(1,5,2,3,7,4,6,8)
titulo.ejey<-"No de palabras ponderadas"

for(j in 1:6){
  
  filename<-paste(names_long_cats[j],"_Emociones_unigra_comparativos",".RData",sep="")
  load(filename)
  
  positivos<-emotion.words[emotion.words$group==1,]
  for(i in 4:11){ set(positivos, i=NULL, j=i, value=positivos[[i]]*positivos[['freq']]) }
  emotions<-data.frame("count"=colSums(positivos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Positivos Unigramas Ponderados", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  negativos<-emotion.words[emotion.words$group==2,]
  for(i in 4:11){ set(negativos, i=NULL, j=i, value=negativos[[i]]*negativos[['freq']]) }
  emotions<-data.frame("count"=colSums(negativos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Negativos Unigramas Ponderados", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  filename<-paste(names_long_cats[j],"_Emociones_unigra_wordclouds",".RData",sep="")
  load(filename)
  
  positivos<-emotion.wordcloud[emotion.wordcloud$group==1,]
  for(i in 4:11){ set(positivos, i=NULL, j=i, value=positivos[[i]]*positivos[['freq']]) }
  emotions<-data.frame("count"=colSums(positivos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Positivos Unigramas_WC Ponderados", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  negativos<-emotion.wordcloud[emotion.wordcloud$group==2,]
  for(i in 4:11){ set(negativos, i=NULL, j=i, value=negativos[[i]]*negativos[['freq']]) }
  emotions<-data.frame("count"=colSums(negativos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Negativos Unigramas_WC Ponderados", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
}  

#-------------------------------------------------------------------------------------------------

#### EMOCIONES DE LOS WORDCLOUDS COMPARATIVOS de BIGRAMAS ####
# Positus vs Negatius
setwd(pathorigin)
load("CyClCm_ff.RData")
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
load("Sentiword_mod.RData")

for(j in 1:6){
  pathsortida<-paste(pathorigin,"/Resultats Sentiment Analysis",sep="")
  setwd(pathsortida)
  filename<-paste("Polarity_Objectivity_Tweets_",names_long_cats[j],".RData", sep="")
  load(filename)
  
  df<-sentiment_SWN  
  
  seleccion_tweets<-which(selec_tweets$id %in% rownames(df[which(df$pol_tweets<0),]))
  negative.tweets<-selec_tweets[seleccion_tweets,"txtc"]
  names(negative.tweets)<-selec_tweets[seleccion_tweets,"id"]
  
  seleccion_tweets<-which(selec_tweets$id %in% rownames(df[which(df$pol_tweets>0),]))
  positive.tweets<-selec_tweets[seleccion_tweets,"txtc"]
  names(positive.tweets)<-selec_tweets[seleccion_tweets,"id"]
  
  NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  
  negativos <- paste(negative.tweets, collapse = " ")
  positivos <- paste(positive.tweets, collapse = " ")
  comparar <- c(positivos,negativos)
  
  corpus.comparar <- VCorpus(VectorSource(comparar))
  tdm.comparar <- as.matrix(TermDocumentMatrix(corpus.comparar, control=list(tokenize=NumbergramTokenizer)))
  colnames(tdm.comparar) <- c("Positive Tweets", "Negative Tweets")
  
  #Calcula en quÃ© grupo queda cada palabra - variable group
  term.matrix<-tdm.comparar
  ndoc <- ncol(term.matrix)
  for (i in 1:ndoc) {
    term.matrix[, i] <- term.matrix[, i]/sum(term.matrix[,i])
  }
  mean.rates <- rowMeans(term.matrix)
  for (i in 1:ndoc) {
    term.matrix[, i] <- term.matrix[, i] - mean.rates
  }
  group <- as.integer(apply(term.matrix, 1, function(x) which.max(x)))
  words <- as.character(rownames(term.matrix))
  freq <- as.numeric(apply(term.matrix, 1, function(x) max(x)))
  
  words.df<-cbind.data.frame(words,freq,group)
  no_cores <- detectCores()-1
  clus <- makeCluster(no_cores, type="PSOCK")
  emotion.df<-get_nrc_sentiment(as.character(words.df$words),cl=clus,language = "english")
  stopCluster(clus)
  
  emotion.words<-cbind.data.frame(words.df,emotion.df)
  
  pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
  setwd(pathsortida)
  filename<-paste(names_long_cats[j],"_Emociones_bigra_comparativos",".RData",sep="")
  save(emotion.words,file=filename)
  
  max.words<-200
  ord <- rank(-freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  group <- group[ord <= max.words]
  ord <- order(freq, decreasing = TRUE)
  words <- words[ord]
  freq <- freq[ord]
  group <- group[ord]
  
  wordcloud.words<-as.data.frame(cbind(words,freq,group))
  seleccion_words<-which(emotion.words$words %in% wordcloud.words$words)
  emotion.wordcloud<-emotion.words[seleccion_words,]
  
  pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
  setwd(pathsortida)
  filename<-paste(names_long_cats[j],"_Emociones_bigra_wordclouds",".RData",sep="")
  save(emotion.wordcloud,file=filename)
}  

#Cambia columnas a castellano
setwd(pathsortida)
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
for(j in 1:6){
  filename<-paste(names_long_cats[j],"_Emociones_bigra_comparativos",".RData",sep="")
  load(filename)
  colnames(emotion.words)[4:13]<-c('Ira','Anticipacion','Disgusto','Miedo',
                                   'Alegria','Tristeza','Sorpresa','Confianza',
                                   'Negativo','Positivo')
  save(emotion.words,file=filename)
  
  filename<-paste(names_long_cats[j],"_Emociones_bigra_wordclouds",".RData",sep="")
  load(filename)
  colnames(emotion.wordcloud)[4:13]<-c('Ira','Anticipacion','Disgusto','Miedo',
                                       'Alegria','Tristeza','Sorpresa','Confianza',
                                       'Negativo','Positivo')
  save(emotion.wordcloud,file=filename)
}  

#Generamos los graficos correspondientes a los diferentes ficheros de emociones
#DE BIGRAMAS
setwd(pathsortida)
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
dforder<-c(1,5,2,3,7,4,6,8)
titulo.ejey<-"No de bigramas"

for(j in 1:6){
  
  filename<-paste(names_long_cats[j],"_Emociones_bigra_comparativos",".RData",sep="")
  load(filename)
  
  positivos<-emotion.words[emotion.words$group==1,]
  emotions<-data.frame("count"=colSums(positivos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Positivos Bigramas", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  negativos<-emotion.words[emotion.words$group==2,]
  emotions<-data.frame("count"=colSums(negativos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Negativos Bigramas", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  filename<-paste(names_long_cats[j],"_Emociones_bigra_wordclouds",".RData",sep="")
  load(filename)
  
  positivos<-emotion.wordcloud[emotion.wordcloud$group==1,]
  emotions<-data.frame("count"=colSums(positivos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Positivos Bigramas_WC", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  negativos<-emotion.wordcloud[emotion.wordcloud$group==2,]
  emotions<-data.frame("count"=colSums(negativos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Negativos Bigramas_WC", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
}  


#Generamos los graficos correspondientes a los diferentes ficheros de emociones
#DE BIGRAMAS PONDERADOS POR SU FRECUENCIA CALCULADA SEGUN EL WORDCLOUD
setwd(pathsortida)
names_long_cats<-c("1_Actividad_Humana", "2_Conocimiento_Cientifico",
                   "3_Entorno_Educativo", "4_Entretenimiento",
                   "5_Relaciones_Humanas", "6_Indeterminado")
dforder<-c(1,5,2,3,7,4,6,8)
titulo.ejey<-"No de bigramas ponderados"

for(j in 1:6){
  
  filename<-paste(names_long_cats[j],"_Emociones_bigra_comparativos",".RData",sep="")
  load(filename)
  
  positivos<-emotion.words[emotion.words$group==1,]
  for(i in 4:11){ set(positivos, i=NULL, j=i, value=positivos[[i]]*positivos[['freq']]) }
  emotions<-data.frame("count"=colSums(positivos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Positivos Bigramas Ponderados", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  negativos<-emotion.words[emotion.words$group==2,]
  for(i in 4:11){ set(negativos, i=NULL, j=i, value=negativos[[i]]*negativos[['freq']]) }
  emotions<-data.frame("count"=colSums(negativos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Negativos Bigramas Ponderados", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  filename<-paste(names_long_cats[j],"_Emociones_bigra_wordclouds",".RData",sep="")
  load(filename)
  
  positivos<-emotion.wordcloud[emotion.wordcloud$group==1,]
  for(i in 4:11){ set(positivos, i=NULL, j=i, value=positivos[[i]]*positivos[['freq']]) }
  emotions<-data.frame("count"=colSums(positivos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Positivos Bigramas_WC Ponderados", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
  negativos<-emotion.wordcloud[emotion.wordcloud$group==2,]
  for(i in 4:11){ set(negativos, i=NULL, j=i, value=negativos[[i]]*negativos[['freq']]) }
  emotions<-data.frame("count"=colSums(negativos[,c(4:11)]))
  emotions<-cbind(emotions,rownames(emotions))
  emotions<-data.frame(emotions[order(dforder),])
  colnames(emotions)<-c("count","names")
  emotions$names <- factor(emotions$names, levels = emotions$names)
  
  titulo<-paste(names_long_cats[j]," Tweets Negativos Bigramas_WC Ponderados", sep="")
  #windows()
  graf<-grafemociones(titulo,emotions,titulo.ejey)
  #print(graf)
  file<-paste("Emociones_",titulo,".png",sep="")
  ggsave(filename = file,plot=graf)
  
}  

#------------------------------------------------------------------------------------------
#Wordclouds de los tweets positivos y negativos según las emociones del NRC
pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
setwd(pathsortida)
load("Tweets_Emotions_NRC_perword_modified.RData")
setwd(pathorigin)
load("CyClCm_ff.RData")

for (j in seq(1:2))
{  

  if(j==1){ df<-emotion.df[emotion.df$Categoria==1,]}
  if(j==2){ df<-emotion.df[emotion.df$Categoria==3,]}
  
  #Busqueda de tweets positivos y negativos segun NRC
  positivos<-df[df$Alegria!=0 | df$Sorpresa!=0 | df$Anticipacion!=0 | df$Confianza!=0,]
  negativos<-df[df$Ira!=0 | df$Disgusto!=0 |df$Miedo | df$Tristeza!=0, ]
  
  pos<-rownames(positivos)
  neg<-rownames(negativos)
  
  t_pos<-selec_tweets[which(selec_tweets$id %in% pos),"txtc"]
  t_neg<-selec_tweets[which(selec_tweets$id %in% neg),"txtc"]
  
  for(x in seq(1:2))
  {
    if(x==1){ t_df<-t_pos}
    if(x==2){ t_df<-t_neg}
    
    #wordclouds de unigramas y bigramas
    #unigramas
    maxterms<-100
    corpus_gr<-VCorpus(VectorSource(t_df))
    tdm_gr<-TermDocumentMatrix(corpus_gr)
    lisWC_gr<-data.frame(terms=Terms(tdm_gr),freq=row_sums(tdm_gr))
    lisWC_gr<-lisWC_gr[order(lisWC_gr$freq,decreasing=TRUE),]
    minfreq<-lisWC_gr$freq[maxterms+1]+1
    if(is.na(minfreq)) {minfreq<-2}
    lisWC_gr<-lisWC_gr[lisWC_gr$freq>=minfreq,]
    
    Sys.sleep(0.001)
    print("Genera Wordclouds unigramas")
    
    #png(name_fig1, units="mm", width=210, height=100, res=300)
    windows()
    wordcloud(lisWC_gr$terms,lisWC_gr$freq,scale=c(1.5,.5),rot.per=0,fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))
    #dev.off()
    
    #bigramas
    NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    tdm_gr <- TermDocumentMatrix(corpus_gr, control = list(tokenize = NumbergramTokenizer))
    lisWC_gr<-data.frame(terms=Terms(tdm_gr),freq=row_sums(tdm_gr))
    lisWC_gr<-lisWC_gr[order(lisWC_gr$freq,decreasing=TRUE),]
    minfreq<-lisWC_gr$freq[maxterms+1]+1
    if(is.na(minfreq)) {minfreq<-2}
    lisWC_gr<-lisWC_gr[lisWC_gr$freq>=minfreq,]
    
    Sys.sleep(0.001)
    print("Genera Wordclouds bigramas")
    
    #png(name_fig2, units="mm",width=210, height=100, res=300)
    windows()
    wordcloud(lisWC_gr$terms,lisWC_gr$freq,scale=c(1.5,.5),rot.per=0,fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))
    #dev.off()
    
    
    if(j==1){ name<-"AH"}
    if(j==2){ name<-"EE"}
    
    #wordclouds comparativos de unigramas
    negativos <- paste(t_neg, collapse = " ")
    positivos <- paste(t_pos, collapse = " ")
    comparar <- c(positivos, negativos)
    corpus.comparar <- Corpus(VectorSource(comparar))
    tdm.comparar <- as.matrix(TermDocumentMatrix(corpus.comparar))
    colnames(tdm.comparar) <- c("Positive Tweets", "Negative Tweets")
    
    fila<-which(rownames(tdm.comparar)=="amp")
    tdm.comparar<-tdm.comparar[-fila,]
    
    setwd(pathsortida)
    
    filename<-paste("WCcomparatius_emotions_", name,"_Unigrams.png", sep="")
    png(filename, units="mm", width=500, height=500, res=600)
    
    #windows()

    #OJITO QUE EL PLOT NEW ES NECESITA SI ES VOL TREURE EN FITXER
    plot.new()
    colnames(tdm.comparar)<-c("","")
    comparison.cloud(tdm.comparar,random.order = FALSE, max.words = 200,
                     title.size = 1, rot.per=0)
    
    dev.off()
    
    
    #wordclouds comparativos de bigramas
    NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

    corpus.comparar <- VCorpus(VectorSource(comparar))
    tdm.comparar <- as.matrix(TermDocumentMatrix(corpus.comparar, control=list(tokenize=NumbergramTokenizer)))
    colnames(tdm.comparar) <- c("Positive Tweets", "Negative Tweets")
    
    filas<-which(grepl("\\<amp\\>", rownames(tdm.comparar)))
    tdm.comparar<-tdm.comparar[-filas,]
    
    #setwd(pathsortidagrafs)
    filename<-paste("WCcomparatius_emotions_", name,"_Bigrams.png", sep="")
    png(filename, units="mm", width=500, height=500, res=600)
    
    #windows()

    plot.new()
    colnames(tdm.comparar)<-c("","")
    comparison.cloud(tdm.comparar,random.order = FALSE, max.words = 200,
                     title.size = 1, rot.per=0)
    dev.off()
    
  }
}