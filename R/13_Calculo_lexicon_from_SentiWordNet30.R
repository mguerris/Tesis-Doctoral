# Fent neteja
rm(list=ls())

pckgs<-c("lexicon", "syuzhet", "ggplot2", "parallel", "slam", 
         "dplyr", "stringr", "data.table", "qdapRegex", "qdap",
         "tm", "wordcloud", "RWeka", "spatstat", "rlist")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}


#Directorio de trabajo
path<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/"
setwd(path)

#Cargamos el SentiWordNet
#Calculamos la polaridad, objetividad y limpiamos de numeros y simbolo#

SentiWordNet<-read.delim("SentiWordNet_3.0.0_20130122.txt", header = F, sep = "\t", quote="\r")
colnames(SentiWordNet) <- c("POS", "ID","PosScore","NegScore","Terms","Gloss")
SentiWordNet$Pol<-SentiWordNet$PosScore-SentiWordNet$NegScore
SentiWordNet$Obj<-1-(SentiWordNet$PosScore+SentiWordNet$NegScore)
SentiWordNet$Terms<-gsub("[[:digit:]]", "", SentiWordNet$Terms)
SentiWordNet$Terms<-gsub("#", "", SentiWordNet$Terms)

#Analisis Rapido de SentiWord
DT<-SentiWordNet
#Nº de palabras que tiene Pos=0 y Neg=0
length(which(DT$Obj==1))
#Nº de palabras con polaridad =0
length(which(DT$Pol==0))
#Nº de palabras con polaridad=0 pero pos o neg <>0
length(which(DT$Pol==0))-length(which(DT$Obj==1))
#Nº de palabras positivas
length(which(DT$PosScore>0))
#Nº de palabras negativas
length(which(DT$NegScore>0))
#Distribucion de polaridades
length(which(DT$Pol>0))
length(which(DT$Pol==0))
length(which(DT$Pol<0))
#Nº de palabras compuestas o expresiones de mas de una palabra (continenen guiones bajos o medios)
length(which(str_count(DT$Terms,"-")!=0))
length(which(str_count(DT$Terms,"_")!=0))
#Terminos unicos de SentiWordNet
length(unique(DT$Terms))
rm(DT)

#Creacion del SentiWord extendido debido a que en una fila existen mas de una palabra
#Ponemos cada termino en una fila y lo a�adimos en el data table
DT_aux <- data.table(SentiWordNet)
DT_aux$Terms<-as.character(DT_aux$Terms)
SentiWord_ext<- DT_aux[, c(.SD, list(posv=strsplit(Terms, " ")))]
SentiWord_ext<-SentiWord_ext[, list(Terms=unlist(posv)), by=list(POS, ID,PosScore,NegScore, Gloss, Pol,Obj)]
rm(DT_aux)

#Analisis Rapido de SentiWord_extended
DT<-SentiWord_ext
#Nº de palabras que tiene Pos=0 y Neg=0
length(which(DT$Obj==1))
#Nº de palabras con polaridad =0
length(which(DT$Pol==0))
#Nº de palabras con polaridad=0 pero pos o neg <>0
length(which(DT$Pol==0))-length(which(DT$Obj==1))
#Nº de palabras positivas
length(which(DT$PosScore>0))
#Nº de palabras negativas
length(which(DT$NegScore>0))
#Distribucion de polaridades
length(which(DT$Pol>0))
length(which(DT$Pol==0))
length(which(DT$Pol<0))
#Nº de palabras compuestas o expresiones de mas de una palabra (continenen guiones bajos o medios)
length(which(str_count(DT$Terms,"-")!=0))
length(which(str_count(DT$Terms,"_")!=0))
#Terminos unicos de SentiWordNet
length(unique(DT$Terms))
#Caracteres en sentiwordnet
table(unlist(strsplit(DT$Terms, ""), use.names=FALSE))

rm(DT)

DT<-SentiWord_ext
DT<-aggregate(SentiWord_ext[,c("PosScore", "NegScore")], list(SentiWord_ext$Terms), mean)
DT$Pol<-DT$PosScore-DT$NegScore
DT$Obj<-1-(DT$PosScore+DT$NegScore)
colnames(DT) <- c("Terms", "PosScore","NegScore","Pol","Obj")
Sentiword_mod<-DT
#Caracteres en sentiwordnet
table(unlist(strsplit(Sentiword_mod$Terms, ""), use.names=FALSE))
rm(DT)

save(Sentiword_mod, file="Sentiword_mod.RData")

#histogrames i estad�stics del sentiwordmod
#Directorio de trabajo
path<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/"
setwd(path)

load("Sentiword_mod.RData")

positivos<-Sentiword_mod[Sentiword_mod$Pol>0,]
negativos<-Sentiword_mod[Sentiword_mod$Pol<0,]
todos<-Sentiword_mod[Sentiword_mod$Pol!=0,]

summary(positivos$Pol)
summary(negativos$Pol)
summary(todos$Pol)


windows()
hist(todos$Pol, ylab="Frecuencia", xlab="Polaridad", main=NULL, xlim=c(-1,1), ylim=c(0,10000), breaks =c(seq(-1,1,0.2)),
     col=c("red", "red", "red", "red", "red", "green", "green","green","green","green"))
windows()
hist(positivos$Pol, ylab="Frecuencia", xlab="Polaridad", main=NULL, ylim=c(0,10000), breaks=4)
windows()
hist(negativos$Pol, ylab="Frecuencia", xlab="Polaridad", ylim=c(0,10000), breaks=4)
