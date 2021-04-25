# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("qdapRegex", "textcat", "tm", "RWeka", "stringr", 
         "qdapDictionaries", "textclean", "lexicon", "parallel",
         "tibble","digest","RSQLite", "RecordLinkage")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg, repos="https://cloud.r-project.org/")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")
#setwd(".")

#Carga de datos
fichero<-"CyClCm_20160101to20160630.RData"
load(fichero)

text<-tweetsGlobalTable$text

# Creem una matriu logica on detectar si hi ha emoticons de la llista emoticon
x<-rep(FALSE, length(text)*length(emoticon$emoticon))
emotis<-matrix(x, nrow=length(text), ncol=length(emoticon$emoticon), dimnames=list(names(text), emoticon$meaning))

# Detectem si en cadascun dels tweets hi ha algun dels emoticons de la llista emoticon
emotis<-(lapply(text, function(x) str_detect(x, fixed(emoticon$emoticon))))
emotis<-matrix(unlist(emotis), byrow=TRUE, nrow=length(emotis) )
colnames(emotis)<-emoticon$meaning
rownames(emotis)<-names(text)
# resul_emotis<-lapply(emotis, function(x) sum(x,na.rm=TRUE))
resul_emotis<-rowSums(emotis)

#distribucion de emoticonos en funcion de su frecuencia en los tweets
emotis_distrib<-rle(sort(resul_emotis))
table(resul_emotis)

windows()
my_bar<-barplot(emotis_distrib$lengths, ylim=c(0,50000), names.arg=emotis_distrib$values)
text(my_bar, emotis_distrib$lengths+0.4 , adj=c(0.5,-1), emotis_distrib$lengths,cex=1) 

#n? de tweets con emoticonos y porcentaje del total de tweets
tweets_emotis<-length(resul_emotis[resul_emotis>0])
tweets_emotispor<-tweets_emotis/length(resul_emotis)

#distribucion de emoticonos segun el tipo de emoticono
resul_emotis<-colSums(emotis)
names(resul_emotis)<-colnames(emotis)
emotis_distrib<-sort(resul_emotis[resul_emotis>0], decreasing = TRUE)

windows()
my_bar<-barplot(emotis_distrib, ylim=c(0,25000), names.arg=names(emotis_distrib),
                cex.names=0.8, horiz=FALSE, las=2)
text(my_bar, emotis_distrib$value+0.4 , adj=c(0.5,-1), emotis_distrib$value,cex=0.8) 

#distribucion de emoticonos segun el tipo de emoticono pero
#sumando aquellos tienen el mismo significado
emotis_distribagg <-aggregate(emotis_distrib, by=list(names(emotis_distrib)), FUN=sum, na.rm=TRUE)
names(emotis_distribagg)<-c("names", "value")
emotis_distribagg<-emotis_distribagg[order(-emotis_distribagg$value),] 

windows()
my_bar<-barplot(emotis_distribagg$value, ylim=c(0,25000), names.arg=emotis_distribagg$names,
                cex.names=0.8, horiz=FALSE, las=2)
text(my_bar, emotis_distribagg$value+0.4 , adj=c(0.5,-1), emotis_distribagg$value,cex=0.8) 

#distribucion de emoticonos segun el tipo de emoticono pero
#sumando aquellos tienen el mismo significado sin Mention y Hashtaggs
emotis_mention<-emotis_distribagg[emotis_distribagg$names=="Mention",] 
sum(emotis_mention$value)
emotis_hashtag<-emotis_distribagg[emotis_distribagg$names=="Hashtag",] 
sum(emotis_hashtag$value)
emotis_distribagg<-emotis_distribagg[emotis_distribagg$names!="Mention",] 
emotis_distribagg<-emotis_distribagg[emotis_distribagg$names!="Hashtag",] 

#numero de tweets con emoticonos sin Mention ni Hashtag y porcentaje del total de tweets
sum(emotis_distribagg$value)
sum(emotis_distribagg$value)/length(text)


windows()
limmax<-max(emotis_distribagg$value)
my_bar<-barplot(emotis_distribagg$value, ylim=c(0,limmax+100), 
                names.arg=emotis_distribagg$names, cex.names=0.8, horiz=FALSE, las=2)
text(my_bar, emotis_distribagg$value+0.4 , adj=c(0.5,-1), emotis_distribagg$value,cex=0.8) 


#Buscamos los emojis basado en el lexicon
no_cores <- 3
cl <- makeCluster(no_cores, type="PSOCK", outfile="debug_file.txt")
clusterExport(cl, c("replace_emoji_identifier"))
emojis<-parLapply(cl,text, function(x) replace_emoji_identifier(x))
stopCluster(cl)

save(emojis,file="Emojis_replaced_in_tweets.RData")

#emojis<-lapply(text, function(x) replace_emoji_identifier(x))

id<-emojis_sentiment$id
no_cores <- 3
cl <- makeCluster(no_cores, type="PSOCK", outfile="debug_file.txt")
clusterExport(cl, c("id","str_detect"))
emojis_detect<-parLapply(cl,emojis, function(x) str_detect(x, id))
stopCluster(cl)

save(emojis_detect,file="Emojis_detected_in_tweets.RData")
load("Emojis_detected_in_tweets.RData")

#emojis_detect<-lapply(emojis, function(x) str_detect(x, emojis_sentiment$id))
emojis_detect<-matrix(unlist(emojis_detect), byrow=TRUE, nrow=length(emojis_detect) )

# resul_emotis<-lapply(emotis, function(x) sum(x,na.rm=TRUE))
resultweet_emojis<-rowSums(emojis_detect)
resulemoji_emojis<-colSums(emojis_detect)
names(resulemoji_emojis)<-emojis_sentiment$id

emojis_detected<-data.frame(resulemoji_emojis,emojis_sentiment)
emojis_detected<-emojis_detected[emojis_detected$resulemoji_emojis>0,]

table(resultweet_emojis)
table(resulemoji_emojis)

tweets_emojisnozero<-resultweet_emojis[resultweet_emojis>0]
sum(emojis_detected$resulemoji_emojis)
pp<-resulemoji_emojis[resulemoji_emojis>0]
sum(emojis_detected$resulemoji_emojis*emojis_detected$sentiment)

citation("textclean")
citation("qdapDictionaries")
