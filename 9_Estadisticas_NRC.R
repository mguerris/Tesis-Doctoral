# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
#pckgs<-c("syuzhet", "lexicon","rlist","tm", "RWeka", "NLP", "openNLP", "devtools")
#pckgs<-c("rlist")
#pckgs<-c("devtools")
pckgs<-c("lexicon", "syuzhet", "ggplot2", "parallel", "slam", 
         "dplyr", "stringr", "data.table", "qdapRegex", "qdap",
         "tm", "wordcloud", "RWeka", "spatstat", "rlist", "xlsx")
#pckgs<-c("lexicon","syuzhet", "stringr", "qdap", "parallel")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

pathorigin<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
pathsortida<-paste(pathorigin,"/Resultats Emocions",sep="")
setwd(pathorigin)

available_data('nrc_emotions')
nrc<-nrc_emotions$term

word_l<-word_split(nrc)
n<-length(word_l)

no_cores <- detectCores()-1
clus <- makeCluster(no_cores, type="PSOCK")
list_words_emotions<-lapply(seq(1:n), function(x) get_nrc_sentiment(word_l[[x]],cl=clus,language = "english"))
stopCluster(clus)

matrix_nrc<-sapply(seq(1:n),function(x) list_words_emotions[[x]])
emotions<-as.data.frame(t(matrix_nrc))
rownames(emotions)<-nrc_emotions$term

setwd(pathsortida)
save(emotions,file="NRC.RData")

#-------------------------------------------------------------------------
setwd(pathsortida)
load("NRC.RData")

nrc<-emotions
nrc<-data.frame(unlist(nrc[1]),unlist(nrc[2]),unlist(nrc[3]),unlist(nrc[4]),
               unlist(nrc[5]),unlist(nrc[6]),unlist(nrc[7]),unlist(nrc[8]),
               unlist(nrc[9]),unlist(nrc[10]))

rownames(nrc)<-rownames(emotions)
colnames(nrc)<-colnames(emotions)

polaritat<-nrc$positive-nrc$negative
summary(polaritat)
hist(polaritat)
nrc$pol=polaritat

total_emo<-colSums(nrc[1:8])
barplot(total_emo)
total_pols<-colSums(nrc[9:11])
barplot(total_pols)
total_pols<-(as.data.frame(table(nrc$pol)))$Freq

suma_emociones<-rowSums(nrc[1:8])
emos<-as.data.frame(table(suma_emociones))
suma_emociones<-suma_emociones[suma_emociones!=0]
summary(suma_emociones)
length(suma_emociones)

df<-nrc
total_emo<-cbind.data.frame(colnames(df[1:8]),colSums(df[1:8]))
colnames(total_emo)<-c("nombre","count")
rownames(total_emo)<-c('Ira','Anticipacion','Disgusto','Miedo',
                       'Alegria','Tristeza','Sorpresa','Confianza')
total_emo$nombre<-c('Ira','Anticipacion','Disgusto','Miedo',
                    'Alegria','Tristeza','Sorpresa','Confianza')
dforder<-c(1,5,2,3,7,4,6,8)
total_emo<-total_emo[order(dforder),]
barplot(total_emo$count)



total_emo$nombre<-factor(total_emo$nombre, levels=c('Ira','Disgusto','Miedo','Tristeza',
                                                    'Anticipacion','Sorpresa',
                                                    'Alegria','Confianza'))


windows()
chart<-ggplot(data=total_emo,aes(x=nombre,y=count)) + 
  geom_bar(aes(fill=nombre),stat = "identity") +
  scale_fill_manual(values=c("grey","grey","grey","grey","grey","grey",
                             "grey","grey")) +
  xlab("Emoción expresada") + ylab("Nº de palabras") +
  geom_text(aes(label=round(total_emo$count), vjust=-0.3, size=3.5))+
  theme_bw() + theme(legend.position='none') +
  theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                  hjust = 0.5,color = "black"))+
  ggtitle("")
plot(chart)



nombre<-c("Negativa", "Neutra","Positiva")
polaridad<-total_pols[1:2]

total_pols<-data.frame(nombre,(as.data.frame(table(nrc$pol)))$Freq)
colnames(total_pols)<-c("nombre","polaridad")

windows()
chart<-ggplot(data=total_pols,aes(x=nombre,y=polaridad)) + 
  geom_bar(aes(fill=nombre),stat = "identity") +
  scale_fill_manual(values=c("grey","grey","grey")) +
  xlab("Polaridad") + ylab("Nº de palabras") +
  geom_text(aes(label=total_pols$polaridad, vjust=-0.3, size=3.5))+
  theme_bw() + theme(legend.position='none') +
  theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                  hjust = 0.5,color = "black"))+
  ggtitle("")
plot(chart)





emotions<-colnames(nrc[1:8])
for(x in emotions){
  df<-nrc[,c(which(colnames(nrc)==x),which(colnames(nrc)!=x))]
  emo<-df[1]
  resto_emos<-rowSums(df[,2:9])
  pol_emo<-df[11]
  df.emo<-cbind.data.frame(emo,resto_emos,pol_emo)
  df.restozero<-df.emo[df.emo[2]==0 & df.emo[1]==1,c(1,3)]
  print(table(df.restozero))
}


df<-nrc[nrc$pol==1,]
total_emo<-cbind.data.frame(colnames(df[1:8]),colSums(df[1:8]))
colnames(total_emo)<-c("nombre","count")
rownames(total_emo)<-c('Ira','Anticipacion','Disgusto','Miedo',
                        'Alegria','Tristeza','Sorpresa','Confianza')
total_emo$nombre<-c('Ira','Anticipacion','Disgusto','Miedo',
                   'Alegria','Tristeza','Sorpresa','Confianza')
dforder<-c(1,5,2,3,7,4,6,8)
total_emo<-total_emo[order(dforder),]
barplot(total_emo$count)


total_emo$nombre<-factor(total_emo$nombre, levels=c('Ira','Disgusto','Miedo','Tristeza',
                                                    'Anticipacion','Sorpresa',
                                                    'Alegria','Confianza'))


windows()
chart<-ggplot(data=total_emo,aes(x=nombre,y=count)) + 
  geom_bar(aes(fill=nombre),stat = "identity") +
  scale_fill_manual(values=c("grey","grey","grey","grey","grey","grey",
                                                      "grey","grey")) +
  xlab("Emoci?n expresada") + ylab("N? de palabras con polaridad +1") +
  geom_text(aes(label=round(total_emo$count), vjust=-0.3, size=3.5))+
  theme_bw() + theme(legend.position='none') +
  theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                  hjust = 0.5,color = "black"))+
  ggtitle("")
plot(chart)

#------------------------------------------------------------------------

df<-nrc[nrc$pol==-1,]
total_emo<-colSums(df[1:8])
barplot(total_emo)

total_emo<-cbind.data.frame(colnames(df[1:8]),colSums(df[1:8]))
colnames(total_emo)<-c("nombre","count")
rownames(total_emo)<-c('Ira','Anticipacion','Disgusto','Miedo',
                       'Alegria','Tristeza','Sorpresa','Confianza')
total_emo$nombre<-c('Ira','Anticipacion','Disgusto','Miedo',
                    'Alegria','Tristeza','Sorpresa','Confianza')
dforder<-c(1,5,2,3,7,4,6,8)
total_emo<-total_emo[order(dforder),]
barplot(total_emo$count)


total_emo$nombre<-factor(total_emo$nombre, levels=c('Ira','Disgusto','Miedo','Tristeza',
                                                    'Anticipacion','Sorpresa',
                                                    'Alegria','Confianza'))


windows()
chart<-ggplot(data=total_emo,aes(x=nombre,y=count)) + 
  geom_bar(aes(fill=nombre),stat = "identity") +
  scale_fill_manual(values=c("grey","grey","grey","grey","grey","grey",
                             "grey","grey")) +
  xlab("Emoci?n expresada") + ylab("N? de palabras con polaridad -1") +
  geom_text(aes(label=round(total_emo$count), vjust=-0.3, size=3.5))+
  theme_bw() + theme(legend.position='none') +
  theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                  hjust = 0.5,color = "black"))+
  ggtitle("")
plot(chart)

#------------------------------------------------------------------------------


df<-nrc[nrc$pol==0,]
total_emo<-colSums(df[1:8])
barplot(total_emo)

total_emo<-cbind.data.frame(colnames(df[1:8]),colSums(df[1:8]))
colnames(total_emo)<-c("nombre","count")
rownames(total_emo)<-c('Ira','Anticipacion','Disgusto','Miedo',
                       'Alegria','Tristeza','Sorpresa','Confianza')
total_emo$nombre<-c('Ira','Anticipacion','Disgusto','Miedo',
                    'Alegria','Tristeza','Sorpresa','Confianza')
dforder<-c(1,5,2,3,7,4,6,8)
total_emo<-total_emo[order(dforder),]
barplot(total_emo$count)


total_emo$nombre<-factor(total_emo$nombre, levels=c('Ira','Disgusto','Miedo','Tristeza',
                                                    'Anticipacion','Sorpresa',
                                                    'Alegria','Confianza'))


windows()
chart<-ggplot(data=total_emo,aes(x=nombre,y=count)) + 
  geom_bar(aes(fill=nombre),stat = "identity") +
  scale_fill_manual(values=c("grey","grey","grey","grey","grey","grey",
                             "grey","grey")) +
  xlab("Emoci?n expresada") + ylab("N? de palabras con polaridad 0") +
  geom_text(aes(label=round(total_emo$count), vjust=-0.3, size=3.5))+
  theme_bw() + theme(legend.position='none') +
  theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                  hjust = 0.5,color = "black"))+
  ggtitle("")
plot(chart)

#------------------------------------------------------------------------------------------

dfpos<-nrc[nrc$pol==1,]
dfneutro<-nrc[nrc$pol==0,]

total_emo<-cbind.data.frame(colnames(df[1:8]),colSums(dfpos[1:8])-colSums(dfneutro[1:8]))
colnames(total_emo)<-c("nombre","count")
rownames(total_emo)<-c('Ira','Anticipacion','Disgusto','Miedo',
                       'Alegria','Tristeza','Sorpresa','Confianza')
total_emo$nombre<-c('Ira','Anticipacion','Disgusto','Miedo',
                    'Alegria','Tristeza','Sorpresa','Confianza')
dforder<-c(1,5,2,3,7,4,6,8)
total_emo<-total_emo[order(dforder),]
barplot(total_emo$count)


total_emo$nombre<-factor(total_emo$nombre, levels=c('Ira','Disgusto','Miedo','Tristeza',
                                                    'Anticipacion','Sorpresa',
                                                    'Alegria','Confianza'))


windows()
chart<-ggplot(data=total_emo,aes(x=nombre,y=count)) + 
  geom_bar(aes(fill=nombre),stat = "identity") +
  scale_fill_manual(values=c("grey","grey","grey","grey","grey","grey",
                             "grey","grey")) +
  xlab("Emoci?n expresada") + ylab("N? de palabras con polaridad +1") +
  geom_text(aes(label=round(total_emo$count), vjust=-0.3, size=3.5))+
  theme_bw() + theme(legend.position='none') +
  theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                  hjust = 0.5,color = "black"))+
  ggtitle("")
plot(chart)

#---------------------------------------------------------------------------------------


dfneg<-nrc[nrc$pol==-1,]
dfneutro<-nrc[nrc$pol==0,]

total_emo<-cbind.data.frame(colnames(df[1:8]),colSums(dfneg[1:8])-colSums(dfneutro[1:8]))
colnames(total_emo)<-c("nombre","count")
rownames(total_emo)<-c('Ira','Anticipacion','Disgusto','Miedo',
                       'Alegria','Tristeza','Sorpresa','Confianza')
total_emo$nombre<-c('Ira','Anticipacion','Disgusto','Miedo',
                    'Alegria','Tristeza','Sorpresa','Confianza')
dforder<-c(1,5,2,3,7,4,6,8)
total_emo<-total_emo[order(dforder),]
barplot(total_emo$count)


total_emo$nombre<-factor(total_emo$nombre, levels=c('Ira','Disgusto','Miedo','Tristeza',
                                                    'Anticipacion','Sorpresa',
                                                    'Alegria','Confianza'))


windows()
chart<-ggplot(data=total_emo,aes(x=nombre,y=count)) + 
  geom_bar(aes(fill=nombre),stat = "identity") +
  scale_fill_manual(values=c("grey","grey","grey","grey","grey","grey",
                             "grey","grey")) +
  xlab("Emoci?n expresada") + ylab("N? de palabras con polaridad -1") +
  geom_text(aes(label=round(total_emo$count), vjust=-0.3, size=3.5))+
  theme_bw() + theme(legend.position='none') +
  theme(plot.title = element_text(size=rel(1.1),vjust = 2, 
                                  hjust = 0.5,color = "black"))+
  ggtitle("")
plot(chart)

