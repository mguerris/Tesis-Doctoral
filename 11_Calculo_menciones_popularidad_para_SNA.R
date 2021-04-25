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
if("xlsx" %in% rownames(installed.packages()) == FALSE) {
  install.packages("xlsx")
}

if("parallel" %in% rownames(installed.packages()) == FALSE) {
  install.packages("parallel")
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
  library("doParallel")

setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")

###--------------------------------------------------------------------------··
###mencions dins TOTS els tweets, inclosos retweets###

fichero<-"CyClCm_20160101to20160630.RData"
load(fichero)
df<-tweetsGlobalTable
#Hem de treure tweets que tenen el id duplicats!!!
df<-df[duplicated(df$id)==FALSE,]
Encoding(df$text) <- "UTF-8"

lan<-textcat(df$text)
df2<-cbind(df,lan)
save(df2,file="Tweets sin id repetido con idioma.RData")
df2<-df2[df2$lan=="english", ]
save(df2,file="Tweets sin id repetido english.RData")

df<-df2

mentions <- stringr::str_extract_all(df$text, "@[a-zA-Z0-9_]{1,}", simplify = FALSE)
full_mentions<-sapply(seq(1:125188) , function(x) length(mentions[[x]]))
names(full_mentions)<-df$id
pos_list<-which(full_mentions!=0)
full_mentions<-lapply(pos_list, function(x) mentions[[x]])
full_mentions<-unlist(full_mentions)
id<-substr(names(full_mentions),1,18)
menciones<-cbind.data.frame(full_mentions,id, stringsAsFactors=FALSE)
colnames(menciones)<-c("data_mentions","id")
menciones<-menciones[!is.na(menciones$data_mentions),]
id<-menciones$id

df_id<-df$id
no_cores <- detectCores()-1
cl <- makeCluster(no_cores, type="PSOCK")
clusterExport(cl, c("seq","id","df_id"))
pos<- parLapply(cl,seq(1:length(id)), function(x) which(df_id %in% id[x]))
stopCluster(cl)
pos<-unlist(pos)
save(pos,file="posicion_tweets.RData")

load("posicion_tweets.RData")

retweet<-df[pos,"isRetweet"]
retweeted<-df[pos,"retweeted"]
retweetcount<-df[pos,"retweetCount"]
screenName<-df[pos,"screenName"]
text<-df[pos,"text"]

menciones<-cbind.data.frame(menciones,screenName, retweet, retweeted, retweetcount, text, stringsAsFactors=FALSE)
save(menciones, file="All_mentions.RData")



#Relacion e mencionados vs screennames
load("All_mentions.RData")
backup1E<-menciones
backup2E <- unique(backup1E[,1:3])
backup3E <- backup2E %>% group_by(data_mentions,screenName) %>% summarize(Relations=n())
backup3E <- backup3E[order(-backup3E$Relations),]
backup3E$data_mentions <- stringr::str_replace_all(backup3E$data_mentions, "@", "")
# backup3E$Mentioned <- gsub("@","",backup3E[,1])
# backup3E[,1] <- substr(backup3E[,1],2,nchar(backup3E[,1]))
EnglishRelations <- backup3E
save(EnglishRelations, file="EnglishMentioned_VS_ScreenName_all.RData")

######################### POPULARITY MENTIONED #################################


load("EnglishMentioned_VS_ScreenName_all.RData")
backup4E <-aggregate(EnglishRelations$Relations,by=list(Mentioned=EnglishRelations$data_mentions), FUN=sum)
colnames(backup4E)<- c("Mentioned", "Repeated")
backup4E <- backup4E[order(-backup4E$Repeated),]
rownames(backup4E) <- (1:nrow(backup4E))
EnglishTwitterPopularity<- backup4E
save(EnglishTwitterPopularity, file="EnglishTwitterPopularity_all.RData")

####################### POPULARITY SCREENNAMES #################################

load("EnglishMentioned_VS_ScreenName_all.RData")
backup4EScreen <-aggregate(EnglishRelations$Relations,by=list(screenName=EnglishRelations$screenName), FUN=sum)
colnames(backup4EScreen)<- c("screenName", "Repeated")
backup4EScreen <- backup4EScreen[order(-backup4EScreen$Repeated),]
rownames(backup4EScreen) <- (1:nrow(backup4EScreen))
EnglishTwitterPopularityScreen <- backup4EScreen
save(EnglishTwitterPopularityScreen, file="EnglishTwitterPopularityScreen_all.RData")


##################################################################################
#------------------CALCULATIONS WITH AH-------------------------------------------
##################################################################################

#Busqueda de menciones dentro de TODAS las menciones encontradas
load("All_mentions.RData")

#Conjunto 1: tweets clasificados como AH
load("id_tweets_AH.Rdata")
id1<-menciones[menciones$id %in% ah,"id"]

#Conjunto 2: retweets de los tweets clasificados como AH
fichero<-"Tweets sin id repetido english.RData"
load(fichero)
df<-df2

text<-df[df$id %in% ah,"text"]
df2<-df[df$text %in% text,]
dupli<-which(duplicated(df2$text))
id_dupli<-df2[dupli,"id"]
id2<-menciones[menciones$id %in% id_dupli,"id"]

#Conjunto 3: menciones de screenNames de tweets clasificados como AH
load("id_tweets_AH.Rdata")
df<-df[df$id %in% ah,]
users<-df[!duplicated(df$screenName),"screenName"]
users<-paste0("@",users)
id3<-menciones[menciones$data_mentions %in% users,"id"]

#Juntamos todos los ids y nos quedamos con los unicos
ids<-c(id1,id2,id3)
ids<-unique(ids)

#Obtenemos la tabla de menciones de AH
menciones_AH<-menciones[menciones$id %in% ids,]
save(menciones_AH,file="AH_mentions.RData")

####################### AH Relacion e mencionados vs screennames ###################

load("AH_mentions.RData")
backup1E<-menciones_AH
backup2E <- unique(backup1E[,1:3])
backup3E <- backup2E %>% group_by(data_mentions,screenName) %>% summarize(Relations=n())
backup3E <- backup3E[order(-backup3E$Relations),]
backup3E$data_mentions <- stringr::str_replace_all(backup3E$data_mentions, "@", "")
# backup3E$Mentioned <- gsub("@","",backup3E[,1])
# backup3E[,1] <- substr(backup3E[,1],2,nchar(backup3E[,1]))
EnglishRelations <- backup3E
save(EnglishRelations, file="EnglishMentioned_VS_ScreenName_AH.RData")

#########################  AH POPULARITY MENTIONED #################################

load("EnglishMentioned_VS_ScreenName_AH.RData")
backup4E <-aggregate(EnglishRelations$Relations,by=list(Mentioned=EnglishRelations$data_mentions), FUN=sum)
colnames(backup4E)<- c("Mentioned", "Repeated")
backup4E <- backup4E[order(-backup4E$Repeated),]
rownames(backup4E) <- (1:nrow(backup4E))
EnglishTwitterPopularity<- backup4E
save(EnglishTwitterPopularity, file="EnglishTwitterPopularity_AH.RData")


####################### AH POPULARITY SCREENNAMES #################################

load("EnglishMentioned_VS_ScreenName_AH.RData")
backup4EScreen <-aggregate(EnglishRelations$Relations,by=list(screenName=EnglishRelations$screenName), FUN=sum)
colnames(backup4EScreen)<- c("screenName", "Repeated")
backup4EScreen <- backup4EScreen[order(-backup4EScreen$Repeated),]
rownames(backup4EScreen) <- (1:nrow(backup4EScreen))
EnglishTwitterPopularityScreen <- backup4EScreen
save(EnglishTwitterPopularityScreen, file="EnglishTwitterPopularityScreen_AH.RData")

##################################################################################
#------------------CALCULATIONS WITH EE-------------------------------------------
##################################################################################

#Busqueda de menciones dentro de TODAS las menciones encontradas
load("All_mentions.RData")

#Conjunto 1: tweets clasificados como EE
load("id_tweets_EE.Rdata")
id1<-menciones[menciones$id %in% ee,"id"]

#Conjunto 2: retweets de los tweets clasificados como AH
fichero<-"Tweets sin id repetido english.RData"
load(fichero)
df<-df2

text<-df[df$id %in% ee,"text"]
df2<-df[df$text %in% text,]
dupli<-which(duplicated(df2$text))
id_dupli<-df2[dupli,"id"]
id2<-menciones[menciones$id %in% id_dupli,"id"]

#Conjunto 3: menciones de screenNames de tweets clasificados como AH
load("id_tweets_EE.Rdata")
df<-df[df$id %in% ee,]
users<-df[!duplicated(df$screenName),"screenName"]
users<-paste0("@",users)
id3<-menciones[menciones$data_mentions %in% users,"id"]

#Juntamos todos los ids y nos quedamos con los unicos
ids<-c(id1,id2,id3)
ids<-unique(ids)

#Obtenemos la tabla de menciones de AH
menciones_EE<-menciones[menciones$id %in% ids,]
save(menciones_EE,file="EE_mentions.RData")

####################### EE Relacion e mencionados vs screennames ###################

load("EE_mentions.RData")
backup1E<-menciones_EE
backup2E <- unique(backup1E[,1:3])
backup3E <- backup2E %>% group_by(data_mentions,screenName) %>% summarize(Relations=n())
backup3E <- backup3E[order(-backup3E$Relations),]
backup3E$data_mentions <- stringr::str_replace_all(backup3E$data_mentions, "@", "")
# backup3E$Mentioned <- gsub("@","",backup3E[,1])
# backup3E[,1] <- substr(backup3E[,1],2,nchar(backup3E[,1]))
EnglishRelations <- backup3E
save(EnglishRelations, file="EnglishMentioned_VS_ScreenName_EE.RData")

#########################  EE POPULARITY MENTIONED #################################

load("EnglishMentioned_VS_ScreenName_EE.RData")
backup4E <-aggregate(EnglishRelations$Relations,by=list(Mentioned=EnglishRelations$data_mentions), FUN=sum)
colnames(backup4E)<- c("Mentioned", "Repeated")
backup4E <- backup4E[order(-backup4E$Repeated),]
rownames(backup4E) <- (1:nrow(backup4E))
EnglishTwitterPopularity<- backup4E
save(EnglishTwitterPopularity, file="EnglishTwitterPopularity_EE.RData")


####################### EE POPULARITY SCREENNAMES #################################

load("EnglishMentioned_VS_ScreenName_EE.RData")
backup4EScreen <-aggregate(EnglishRelations$Relations,by=list(screenName=EnglishRelations$screenName), FUN=sum)
colnames(backup4EScreen)<- c("screenName", "Repeated")
backup4EScreen <- backup4EScreen[order(-backup4EScreen$Repeated),]
rownames(backup4EScreen) <- (1:nrow(backup4EScreen))
EnglishTwitterPopularityScreen <- backup4EScreen
save(EnglishTwitterPopularityScreen, file="EnglishTwitterPopularityScreen_EE.RData")


################################################################################
############################## EXPORTAMOS TABLAS A XLSX ########################
###############################################################################

library(xlsx)

#Tablas de menciones
load("All_mentions.RData")
write.csv(menciones, file="Tabla de menciones TOTAL.csv", row.names = FALSE)
load("AH_mentions.RData")
write.csv(menciones_AH, file="Tabla de menciones AH.csv", row.names = FALSE)
load("EE_mentions.RData")
write.csv(menciones_EE, file="Tabla de menciones EE.csv", row.names = FALSE)

library("openxlsx")
write.xlsx(read.csv("Tabla de menciones TOTAL.csv"), "Tabla de menciones TOTAL.xlsx")
write.xlsx(read.csv("Tabla de menciones AH.csv"), "Tabla de menciones AH.xlsx")
write.xlsx(read.csv("Tabla de menciones EE.csv"), "Tabla de menciones EE.xlsx")

#Usuarios mencionados
library(Hmisc)

load("EnglishTwitterPopularity_all.RData")
write.csv(EnglishTwitterPopularity, file="Usuarios mencionados TOTAL.csv", row.names = FALSE)
summary(EnglishTwitterPopularity$Repeated)
describe(EnglishTwitterPopularity$Repeated)
quantile(EnglishTwitterPopularity$Repeated, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))
load("EnglishTwitterPopularity_AH.RData")
write.csv(EnglishTwitterPopularity, file="Usuarios mencionados AH.csv", row.names = FALSE)
summary(EnglishTwitterPopularity$Repeated)
describe(EnglishTwitterPopularity$Repeated)
quantile(EnglishTwitterPopularity$Repeated, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))
load("EnglishTwitterPopularity_EE.RData")
write.csv(EnglishTwitterPopularity, file="Usuarios mencionados EE.csv", row.names = FALSE)
summary(EnglishTwitterPopularity$Repeated)
describe(EnglishTwitterPopularity$Repeated)
quantile(EnglishTwitterPopularity$Repeated, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))

write.xlsx(read.csv("Usuarios mencionados TOTAL.csv"), "Usuarios mencionados TOTAL.xlsx")
write.xlsx(read.csv("Usuarios mencionados AH.csv"), "Usuarios mencionados AH.xlsx")
write.xlsx(read.csv("Usuarios mencionados EE.csv"), "Usuarios mencionados EE.xlsx")

#Usuarios que mencionan
load("EnglishTwitterPopularityScreen_all.RData")
write.csv(EnglishTwitterPopularityScreen, file="Usuarios que mencionan TOTAL.csv", row.names = FALSE)
summary(EnglishTwitterPopularityScreen$Repeated)
describe(EnglishTwitterPopularityScreen$Repeated)
quantile(EnglishTwitterPopularityScreen$Repeated, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))

load("EnglishTwitterPopularityScreen_AH.RData")
write.csv(EnglishTwitterPopularityScreen, file="Usuarios que mencionan AH.csv", row.names = FALSE)
summary(EnglishTwitterPopularityScreen$Repeated)
describe(EnglishTwitterPopularityScreen$Repeated)
quantile(EnglishTwitterPopularityScreen$Repeated, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))

load("EnglishTwitterPopularityScreen_EE.RData")
write.csv(EnglishTwitterPopularityScreen, file="Usuarios que mencionan EE.csv", row.names = FALSE)
summary(EnglishTwitterPopularityScreen$Repeated)
describe(EnglishTwitterPopularityScreen$Repeated)
quantile(EnglishTwitterPopularityScreen$Repeated, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))


library("openxlsx")
write.xlsx(read.csv("Usuarios que mencionan TOTAL.csv"), "Usuarios que mencionan TOTAL.xlsx")
write.xlsx(read.csv("Usuarios que mencionan AH.csv"), "Usuarios que mencionan AH.xlsx")
write.xlsx(read.csv("Usuarios que mencionan EE.csv"), "Usuarios que mencionan EE.xlsx")



################################################################################
###################### MENTIONS' EXTRACTION FROM TEXT WITH #####################
#################### THEIR RESPECTIVE TWEETID AND SCREENNAME ###################
################################################################################

  load("id_tweets_AH.Rdata")
  load("id_tweets_EE.Rdata")
  load("CyClCm_20160101to20160630_ff.RData")
  
  fichero<-"CyClCm_20160101to20160630.RData"
  load(fichero)
  
  #busquem totes les mencions de tots els tweets#
  
  
  
  #df<-selec_tweets
  df<-tweetsGlobalTable
  df<-df[df$id %in% ah,]

  TwitterUsefulDataE<-data.frame(df$text,df$id,df$screenName, stringsAsFactors = FALSE)
  colnames(TwitterUsefulDataE)<-c("text","id","screenName")
  
  backupE <- data.frame(matrix(ncol = 3, nrow=0))
  colnames(backupE) <- colnames(TwitterUsefulDataE)
  for (i in 1:nrow(TwitterUsefulDataE)) {
    screenName <- TwitterUsefulDataE$screenName[i]
    TweetId <- TwitterUsefulDataE$id[i]
    Encoding(TwitterUsefulDataE$text[i]) <- "UTF-8"
    mentions <- stringr::str_extract_all(TwitterUsefulDataE$text[i], "@[a-zA-Z0-9_]{1,}", simplify = TRUE)
    if (length(mentions) > 0) {
      for (j in 1:length(mentions)) {
        backupE[nrow(backupE)+1,] <- c(mentions[j], TweetId, screenName)
      }
    }
    if (i %% 10000 == 1) {
      message(i);
    }
  }
  colnames(backupE)<-c("Mentioned","TweetId","screenName")
  #backupE <- backupE1
  m_AH<-backupE

  
    df<-selec_tweets
    df<-df[df$id %in% ee,]
    
    TwitterUsefulDataE<-data.frame(df$text,df$id,df$screenName, stringsAsFactors = FALSE)
    colnames(TwitterUsefulDataE)<-c("text","id","screenName")
    
    backupE <- data.frame(matrix(ncol = 3, nrow=0))
    colnames(backupE) <- colnames(TwitterUsefulDataE)
    for (i in 1:nrow(TwitterUsefulDataE)) {
      screenName <- TwitterUsefulDataE$screenName[i]
      TweetId <- TwitterUsefulDataE$id[i]
      Encoding(TwitterUsefulDataE$text[i]) <- "UTF-8"
      mentions <- stringr::str_extract_all(TwitterUsefulDataE$text[i], "@[a-zA-Z0-9_]{1,}", simplify = TRUE)
      if (length(mentions) > 0) {
        for (j in 1:length(mentions)) {
          backupE[nrow(backupE)+1,] <- c(mentions[j], TweetId, screenName)
        }
      }
      if (i %% 10000 == 1) {
        message(i);
      }
    }
    colnames(backupE)<-c("Mentioned","TweetId","screenName")
    m_EE<-backupE
  
  save(m_AH, file="EnglishOrderedMentionsTable_AH_alltweets.rda")
  save(m_EE, file="EnglishOrderedMentionsTable_EE_alltweets.rda")
  
  load("EnglishOrderedMentionsTable_EE_alltweets.rda")
  df<-selec_tweets
  df<-df[df$id %in% ee,]
  pp<-df[df$id %in% backupAH$TweetId,]
  
  load("EnglishOrderedMentionsTable_AH.rda")
  load("EnglishOrderedMentionsTable_EE.rda")
  
  library(xlsx)
  write.xlsx2(m_AH, "Tabla de menciones AH.xlsx")
  write.xlsx2(m_EE, "Tabla de menciones EE.xlsx")
  
################################################################################
################################ RELATION BETWEEN ##############################
############################ MENTIONED VS SCREENNAMES ##########################
################################################################################
  
  load("EnglishOrderedMentionsTable_AH.rda")
  backup1E<-m_AH
  backup2E <- unique(backup1E[,1:3])
  backup3E <- backup2E %>% group_by(Mentioned,screenName) %>% summarize(Relations=n())
  backup3E <- backup3E[order(-backup3E$Relations),]
  backup3E$Mentioned <- stringr::str_replace_all(backup3E$Mentioned, "@", "")
  # backup3E$Mentioned <- gsub("@","",backup3E[,1])
  # backup3E[,1] <- substr(backup3E[,1],2,nchar(backup3E[,1]))
  EnglishRelations_AH <- backup3E
  save(EnglishRelations_AH, file="EnglishMentioned_VS_ScreenName_AH.rda")
  
  #Seleccio dels que mes mencionen i a qui mencionen
  load("EnglishMentioned_VS_ScreenName_AH.rda")
  m<-EnglishRelations_AH
  m<-m[m$screenName=="BigD_KnowsAll",]
  
  write.csv(EnglishRelations_AH, file="Relacion mencionados_menciones AH.csv")
  write.csv(m, file="Relacion primer mencionados_menciones AH.csv")
  
  load("EnglishMentioned_VS_ScreenName_EE.rda")
  m<-EnglishRelations_EE
  m<-m[m$screenName=="ifriqiyah",]
  
  write.csv(EnglishRelations_EE, file="Relacion mencionados_menciones EE.csv")
  write.csv(m, file="Relacion primer mencionados_menciones EE.csv")
  
  
  load("EnglishOrderedMentionsTable_EE.rda")
  backup1E<-m_EE
  backup2E <- unique(backup1E[,1:3])
  backup3E <- backup2E %>% group_by(Mentioned,screenName) %>% summarize(Relations=n())
  backup3E <- backup3E[order(-backup3E$Relations),]
  backup3E$Mentioned <- stringr::str_replace_all(backup3E$Mentioned, "@", "")
  # backup3E$Mentioned <- gsub("@","",backup3E[,1])
  # backup3E[,1] <- substr(backup3E[,1],2,nchar(backup3E[,1]))
  EnglishRelations_EE <- backup3E
  save(EnglishRelations_EE, file="EnglishMentioned_VS_ScreenName_EE.rda")
  
################################################################################
######################### POPULARITY MENTIONED #################################
################################################################################
  
  load("EnglishMentioned_VS_ScreenName_AH.rda")
  backup4E <-aggregate(EnglishRelations_AH$Relations,by=list(Mentioned=EnglishRelations_AH$Mentioned), FUN=sum)
  colnames(backup4E)<- c("Mentioned", "Repeated")
  backup4E <- backup4E[order(-backup4E$Repeated),]
  rownames(backup4E) <- (1:nrow(backup4E))
  EnglishTwitterPopularity_AH <- backup4E
  save(EnglishTwitterPopularity_AH, file="EnglishTwitterPopularity_AH.rda")
  
  load("EnglishMentioned_VS_ScreenName_EE.rda")
  backup4E <-aggregate(EnglishRelations_EE$Relations,by=list(Mentioned=EnglishRelations_EE$Mentioned), FUN=sum)
  colnames(backup4E)<- c("Mentioned", "Repeated")
  backup4E <- backup4E[order(-backup4E$Repeated),]
  rownames(backup4E) <- (1:nrow(backup4E))
  EnglishTwitterPopularity_EE <- backup4E
  save(EnglishTwitterPopularity_EE, file="EnglishTwitterPopularity_EE.rda")
  
  load("EnglishTwitterPopularity_AH.rda")
  load("EnglishTwitterPopularity_EE.rda")
  
  summary(EnglishTwitterPopularity_AH)
  summary(EnglishTwitterPopularity_EE)
  
  write.xlsx2(EnglishTwitterPopularity_AH,"Usuarios mencionados AH.xlsx")
  write.xlsx2(EnglishTwitterPopularity_EE,"Usuarios mencionados EE.xlsx")
  
################################################################################
####################### POPULARITY SCREENNAMES #################################
################################################################################
  
  load("EnglishMentioned_VS_ScreenName_AH.rda")
  backup4EScreen <-aggregate(EnglishRelations_AH$Relations,by=list(screenName=EnglishRelations_AH$screenName), FUN=sum)
  colnames(backup4EScreen)<- c("screenName", "Repeated")
  backup4EScreen <- backup4EScreen[order(-backup4EScreen$Repeated),]
  rownames(backup4EScreen) <- (1:nrow(backup4EScreen))
  EnglishTwitterPopularityScreen_AH <- backup4EScreen
  save(EnglishTwitterPopularityScreen_AH, file="EnglishTwitterPopularityScreen_AH.rda")

  
  load("EnglishMentioned_VS_ScreenName_EE.rda")
  backup4EScreen <-aggregate(EnglishRelations_EE$Relations,by=list(screenName=EnglishRelations_EE$screenName), FUN=sum)
  colnames(backup4EScreen)<- c("screenName", "Repeated")
  backup4EScreen <- backup4EScreen[order(-backup4EScreen$Repeated),]
  rownames(backup4EScreen) <- (1:nrow(backup4EScreen))
  EnglishTwitterPopularityScreen_EE <- backup4EScreen
  save(EnglishTwitterPopularityScreen_EE, file="EnglishTwitterPopularityScreen_EE.rda")

  load("EnglishTwitterPopularityScreen_AH.rda")
  load("EnglishTwitterPopularityScreen_EE.rda")
  summary(EnglishTwitterPopularityScreen_AH)
  summary(EnglishTwitterPopularityScreen_EE)
  write.xlsx2(EnglishTwitterPopularityScreen_AH, "Usuarios que mencionan AH.xlsx")
  write.xlsx2(EnglishTwitterPopularityScreen_EE, "Usuarios que mencionan EE.xlsx")
