rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("stringr", "qdapDictionaries", "lexicon", "tm", "wordcloud", "slam")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

citation("stringr")
citation("lexicon")
emoticon

#Directorio de trabajo
#setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")
setwd("C:/Users/mguerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")

#Carga de datos
#fichero<-"CyClCm_20160101to20160630.RData"
fichero<-"CyClCm.RData"
load(fichero)

write.csv2(tweetsGlobalTable$text, file="Tweets_Brutos_raw.csv", sep=";")

tweetsGlobalTable$conRT=grepl('\\brt\\b',tweetsGlobalTable$text, ignore.case=TRUE)

datos<-tweetsGlobalTable$text
datos<-iconv(datos,to="UTF-8")
datos_ascii<-iconv(tweetsGlobalTable$text,to="ASCII")

#----------------------------------------------------------------------------------------------------
#Caracteres por tweet---------------------------------------------------------
#----------------------------------------------------------------------------------------------------
chars_per_tweet<-str_length(datos)
summary(unlist(chars_per_tweet))
#Existen tweets con más de 144 caracteres debido a que los codigos de control ejemplo u0081 quedan dentro
#de los tweets como caracteres cuando no lo son para un ordenador
total_char<-sum(unlist(chars_per_tweet))

#----------------------------------------------------------------------------------------------------
#Número de TERMINOS ENTRE ESPACIOS por tweet---------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#word_l <- strsplit(datos, "[^A-Za-z']+")
word_l <- strsplit(datos, " ")

#Retorna vector logico de las palabras de los tweets
terms_per_tweet<-lapply(seq(1:length(word_l)),function(x) length(word_l[[x]]))
summary(unlist(terms_per_tweet))
quantile(unlist(terms_per_tweet), c(0.90,0.95,0.98,0.99))


#----------------------------------------------------------------------------------------------------
#Número de CARACTERES diferentes de a-z y de A-Z por tweet------------------------------------------------------------------------
#Los espacios no se consideran. Numeros quedaran tambien reflejados-------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
caract<-str_extract_all(datos, "[^a-zA-Z ']")
caract_per_tweet<-lapply(seq(1:length(caract)),function(x) length(caract[[x]]))
summary(unlist(caract_per_tweet))
quantile(unlist(caract_per_tweet), c(0.90,0.95,0.98,0.99))
por_caract_dif<-sum(unlist(caract_per_tweet))/total_char

#----------------------------------------------------------------------------------------------------
#Número de HASHTAGS por tweet------------------------------------------------------------------------
#Revision de hashtags  en una tabla-------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
hashtags<-str_extract_all(datos,"\\S*[#]\\S*")
hashtags_per_tweet<-lapply(seq(1:length(hashtags)),function(x) length(hashtags[[x]]))
table(unlist(hashtags_per_tweet))
summary(unlist(hashtags_per_tweet))
quantile(unlist(hashtags_per_tweet), c(0.90,0.95,0.98,0.99))

hashtags<-unlist(hashtags)
tabla_hashtags<-as.data.frame(table(hashtags))
tabla_hashtags<-tabla_hashtags[order(-tabla_hashtags$Freq),]
sum(tabla_hashtags$Freq)

#----------------------------------------------------------------------------------------------------
#Número de MENCIONES por tweet------------------------------------------------------------------------
#Revision de hashtags  en una tabla-------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
usuarios<-str_extract_all(datos,"\\S*[@]\\S*")
usuarios_per_tweet<-lapply(seq(1:length(usuarios)),function(x) length(usuarios[[x]]))
summary(unlist(usuarios_per_tweet))
quantile(unlist(usuarios_per_tweet), c(0.90,0.95,0.98,0.99))

usuarios<-unlist(usuarios)
tabla_usuarios<-as.data.frame(table(usuarios))
tabla_usuarios<-tabla_usuarios[order(-tabla_usuarios$Freq),]

#----------------------------------------------------------------------------------------------------
#Número de urls por tweet------------------------------------------------------------------------
#Revision de urls  en una tabla-------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
urls<-str_extract_all(datos,"\\bhttps?[:]\\S*\\b")
urls_per_tweet<-lapply(seq(1:length(urls)),function(x) length(urls[[x]]))
table(unlist(urls_per_tweet))
summary(unlist(urls_per_tweet))
quantile(unlist(urls_per_tweet), c(0.90,0.95,0.98,0.99))

urls<-unlist(urls)
tabla_urls<-as.data.frame(table(urls))
tabla_urls<-tabla_urls[order(-tabla_urls$Freq),]

#----------------------------------------------------------------------------------------------------
#Número de HTML < > por tweet------------------------------------------------------------------------
#Revision en una tabla-------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
datos<-tweetsGlobalTable$text

write.csv2(datos, file="Tweets_brutos_para_html.csv")
#analisis en fichero xlsx



#----------------------------------------------------------------------------------------------------
#Número de retweets ------------------------------------------------------------------------
#-------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
retweets<-subset(tweetsGlobalTable, conRT==TRUE | isRetweet==TRUE)
nrow(retweets)


#----------------------------------------------------------------------------------------------------
#Número de emoticonos ------------------------------------------------------------------------
#Emoticonos en el package Qdpadictionaries-------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Creem una matriu logica on detectar si hi ha emoticons de la llista emoticon
x<-rep(FALSE, length(datos)*length(emoticon$emoticon))
emotis<-matrix(x, nrow=length(datos), ncol=length(emoticon$emoticon), dimnames=list(names(datos), emoticon$meaning))

# Detectem si en cadascun dels tweets hi ha algun dels emoticons de la llista emoticon
emotis<-(lapply(datos, function(x) str_detect(x, fixed(emoticon$emoticon))))
emotis<-matrix(unlist(emotis), byrow=TRUE, nrow=length(emotis) )
colnames(emotis)<-emoticon$meaning
rownames(emotis)<-names(datos)
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
sum(emotis_distribagg$value)/length(datos)


windows()
limmax<-max(emotis_distribagg$value)
my_bar<-barplot(emotis_distribagg$value, ylim=c(0,2000), 
                names.arg=emotis_distribagg$names, cex.names=0.8, horiz=FALSE, las=2)
text(my_bar, emotis_distribagg$value+0.4 , adj=c(0.5,-1), emotis_distribagg$value,cex=0.8) 

#----------------------------------------------------------------------------------------------------
#Número de emojis calculado ------------------------------------------------------------------------
#Emojis dentro del package textclean, en el package lexicon-------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
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

#Buscamos los emojis basado en el lexicon
no_cores <- 3
cl <- makeCluster(no_cores, type="PSOCK", outfile="debug_file.txt")
clusterExport(cl, c("replace_emoji_identifier"))
emojis<-parLapply(cl,text, function(x) replace_emoji_identifier(x))
stopCluster(cl)

save(emojis,file="Emojis_replaced_in_tweets.RData")

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

data(emojis_sentiment)

emojis_detected<-data.frame(resulemoji_emojis,emojis_sentiment)
emojis_detected<-emojis_detected[emojis_detected$resulemoji_emojis>0,]

tweets_sin_emojis<-length(which(resultweet_emojis==0))
por_tweets_sin_emojis<-tweets_sin_emojis/length(resultweet_emojis)
por_tweets_con_emojis<-1-por_tweets_sin_emojis

summary(resultweet_emojis)
table(resultweet_emojis)
quantile(unlist(resultweet_emojis), c(0.90,0.95,0.98,0.99))

#Worclouds 
#Ejemplo de tweets antes de limpiarlos
#

#t1<-"RT @lucas_bohannon: @holly_becker WHEN YOU LOOKED AT DANNY IN CHEM OMG<ed><U+00A0><U+00BD><ed><U+00B8><U+0082><ed><U+00A0><U+00BD><ed><U+00B8><U+0082>"
t1<-"RT @lucas_bohannon: @holly_becker WHEN YOU LOOKED AT DANNY IN CHEM OMG<ed><U+00A0><U+00BD><ed>"
t2<-"<U+6B21><U+306F>."
t3<-"My grade on the Chem test if we still have school tommorow #closeFCPS http://t.co/Lj8OORQmwc"
t4<-"RT @sernaum: D'D¾D¹D½D¾D²D¸Ñ???: Â«D~D½D¾D³D´D° ÑD¼DµÑ???Ñ,ÑO D¾D´D½D¾D³D¾ Ñ???DµD»D¾D²DµDºD° "
t5<-"RT @kaylajodell: Walking out of o chem.. @colleen_penny95 @nathanepstein17 http://t.co/ZTefjKWg2t"
t6<-"Chem: C-C chemokine receptor type 2 (CCR2) signaling protects neonatal male mice with hypoxic-ischemic "
t7<-"Been working on this chem lab for 3 hours not even close to done :))))"

datos<-c(t1,t2,t3,t4,t5,t6,t7)

texts_cl<-datos
corpus_gr<-VCorpus(VectorSource(texts_cl))
tdm_gr<-TermDocumentMatrix(corpus_gr)
lisWC_gr<-data.frame(terms=Terms(tdm_gr),freq=row_sums(tdm_gr))
lisWC_gr<-lisWC_gr[order(lisWC_gr$freq,decreasing=TRUE),] 
lisWC_gr<-lisWC_gr[!(lisWC_gr$terms=="chemical"),]
lisWC_gr<-lisWC_gr[!(lisWC_gr$terms=="chemistry"),]
lisWC_gr<-lisWC_gr[!(lisWC_gr$terms=="chem"),]
windows()
wordcloud(lisWC_gr$terms,lisWC_gr$freq,scale=c(1.5,.5),rot.per=0,
          max.words = 200, fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))


#Limpieza de tweets
minlenparaules<-2
docs<-datos
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


texts_cl<-docs
corpus_gr<-VCorpus(VectorSource(texts_cl))
tdm_gr<-TermDocumentMatrix(corpus_gr)
lisWC_gr<-data.frame(terms=Terms(tdm_gr),freq=row_sums(tdm_gr))
lisWC_gr<-lisWC_gr[order(lisWC_gr$freq,decreasing=TRUE),] 
lisWC_gr<-lisWC_gr[!(lisWC_gr$terms=="chemical"),]
lisWC_gr<-lisWC_gr[!(lisWC_gr$terms=="chemistry"),]
lisWC_gr<-lisWC_gr[!(lisWC_gr$terms=="chem"),]
windows()
wordcloud(lisWC_gr$terms,lisWC_gr$freq,scale=c(1.5,.5),rot.per=0,
          max.words = 200, fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))





