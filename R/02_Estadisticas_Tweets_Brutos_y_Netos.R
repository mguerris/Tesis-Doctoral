# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("qdapRegex", "textcat", "tm", "RWeka", "stringr", "plyr", "skmeans", "slam")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")

#Carga de datos
#fichero<-"CyClCm_20160101to20160630.RData"
fichero<-"CyClCm.RData"
load(fichero)

#Deteccion de tweets que contienen las PALABRAS chemical, chemistry y chem
tweetsGlobalTable$cy=grepl("\\bchemistry\\b", tweetsGlobalTable$text, ignore.case=TRUE)
tweetsGlobalTable$cl=grepl("\\bchemical\\b", tweetsGlobalTable$text, ignore.case=TRUE)
tweetsGlobalTable$cm=grepl("\\bchem\\b", tweetsGlobalTable$text, ignore.case=TRUE)
#Tweets que contiene la CADENA DE TEXTO chem en alguna parte del tweet
tweetsGlobalTable$cm2=grepl("chem", tweetsGlobalTable$text, ignore.case=TRUE)

#Analisis de hashtags para ver si aportan o no informacion a los tweets
hashtags<-str_extract_all(tweetsGlobalTable$text,"\\S*[#]\\S*")
hashtags<-unlist(hashtags)
tabla_hashtags<-as.data.frame(table(hashtags))
tabla_hashtags<-tabla_hashtags[order(-tabla_hashtags$Freq),]
#Existen hashtags que aportan informaci?n relevante y por tanto mejor no eliminar los hashtags

#Analisis de usuarios para ver si aportan o no informacion a los tweets
usuarios<-str_extract_all(tweetsGlobalTable$text,"\\S*[@]\\S*")
usuarios<-unlist(usuarios)
tabla_usuarios<-as.data.frame(table(usuarios))
tabla_usuarios<-tabla_usuarios[order(-tabla_usuarios$Freq),]
#Los usuarios no parecen aportar informaci?n y por tanto los eliminaremos


df<-tweetsGlobalTable
#Deteccion de tweets que contienen las palabras chemical (cl), chemistry (cy) y chem(cm)
nrow(df)
count(df$cl==TRUE)
count(df$cy==TRUE)
count(df$cm==TRUE)
#Deteccion de tweet que contienen la cadena de texto CHEM
count(df$cm2==TRUE)

#ejemplo de tweets que contienen la cadena de texto CHEM y no con contienen las palabras
pepe<-df[df$cm2==TRUE & df$cl==FALSE & df$cy==FALSE & df$cm==FALSE,"text"]
pepe[10:20]

#ejemplo de tweets que NO contienen la cadena de texto CHEM 
nochem<-df[df$cm2==FALSE,]
pepe[50:60]
nochem$sc=grepl("chem", nochem$screenName, ignore.case=TRUE)
count(nochem$cm==TRUE)

#Tweets repetidos entre pares de las palabras chemical, chemistry y chem
table(df$cl,df$cy)
table(df$cl,df$cm)
table(df$cy,df$cm)

fichero<-"CyClCm_20160101to20160630_c.RData"
load(fichero)

#deteccion de tweets sin retweets
df<-subset(tweetsGlobalTable, conRT==FALSE & isRetweet==FALSE)
nrow(df)
count(df$cl==TRUE)
count(df$cy==TRUE)
count(df$cm==TRUE)

#deteccion de tweets sin retweets y no vacios
df<-subset(df, txtc!="")
nrow(df)
count(df$cl==TRUE)
count(df$cy==TRUE)
count(df$cm==TRUE)

#deteccion de tweets sin retweets y no vacios y ingles
df<-subset(df, lang_txtc=="english")
nrow(df)
count(df$cl==TRUE)
count(df$cy==TRUE)
count(df$cm==TRUE)

#tweets unicos
df<-df[!duplicated(df$txtc),]
nrow(df)
count(df$cl==TRUE)
count(df$cy==TRUE)
count(df$cm==TRUE)

fichero<-"CyClCm_20160101to20160630_f.RData"
load(fichero)
df<-selec_tweets
nrow(df)
count(df$cl==TRUE)
count(df$cy==TRUE)
count(df$cm==TRUE)

#unicos y sin las stopwords
fichero<-"CyClCm_20160101to20160630_ff.RData"
load(fichero)
df<-selec_tweets
nrow(df)
count(df$cl==TRUE)
count(df$cy==TRUE)
count(df$cm==TRUE)

#------------------------------------------------------------------------------------
#Analisis de contenidos de los tweets segun los procesos de limpieza
# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("qdapRegex", "textcat", "tm", "RWeka", "stringr", "plyr", "skmeans", "slam")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")


#Analisis de los tweets resultantes de todo el proceso a nivel de contenido
load("CyClCm_ff.RData")


#Deteccion de tweets que contienen las PALABRAS chemical, chemistry y chem
selec_tweets$cy=grepl("\\bchemistry\\b", selec_tweets$text, ignore.case=TRUE)
selec_tweets$cl=grepl("\\bchemical\\b", selec_tweets$text, ignore.case=TRUE)
selec_tweets$cm=grepl("\\bchem\\b", selec_tweets$text, ignore.case=TRUE)
#Tweets que contiene la CADENA DE TEXTO chem en alguna parte del tweet
selec_tweets$cm2=grepl("chem", selec_tweets$text, ignore.case=TRUE)

df<-selec_tweets
#Deteccion de tweets que contienen las palabras chemical (cl), chemistry (cy) y chem(cm)
nrow(df)
count(df$cl==TRUE)
count(df$cy==TRUE)
count(df$cm==TRUE)
#Deteccion de tweet que contienen la cadena de texto CHEM
count(df$cm2==TRUE)

#Tweets repetidos entre pares de las palabras chemical, chemistry y chem
table(df$cl,df$cy)
table(df$cl,df$cm)
table(df$cy,df$cm)

#ejemplo de tweets que contienen la cadena de texto CHEM y no con contienen las palabras
pepe<-df[df$cm2==TRUE & df$cl==FALSE & df$cy==FALSE & df$cm==FALSE,]
pepe[10:20]

#ejemplo de tweets que NO contienen la cadena de texto CHEM 
chem2<-df[df$cm2==TRUE,]
nochem<-df[df$cm2==FALSE,]
pepe[50:60]
nochem$sc=grepl("chem", nochem$screenName, ignore.case=TRUE)
count(nochem$cm==TRUE)


chemical<-df[df$cl==TRUE,]
chemistry<-df[df$cy==TRUE,]
chem<-df[df$cm==TRUE,]
chemalone<-df[df$cm2==TRUE & df$cl==FALSE & df$cy==FALSE & df$cm==FALSE,]

newdf<-rbind(chemical,chemistry,chem)
pepe<-unique(newdf)
#Cuidado: Hay un problema con los repetidos. Com unique(newdf) los repetidos deberian ser 548
#En lugar de los 551 calculados a traves de los pares

chem2<-df[df$cm2==TRUE,]

#Analisis de los contenidos de texto limpiados
#Deteccion de tweets que contienen las PALABRAS chemical, chemistry y chem
selec_tweets$cy=grepl("\\bchemistry\\b", selec_tweets$txtc, ignore.case=TRUE)
selec_tweets$cl=grepl("\\bchemical\\b", selec_tweets$txtc, ignore.case=TRUE)
selec_tweets$cm=grepl("\\bchem\\b", selec_tweets$txtc, ignore.case=TRUE)
#Tweets que contiene la CADENA DE TEXTO chem en alguna parte del tweet
selec_tweets$cm2=grepl("chem", selec_tweets$txtc, ignore.case=TRUE)

df<-selec_tweets
#Deteccion de tweets que contienen las palabras chemical (cl), chemistry (cy) y chem(cm)
nrow(df)
count(df$cl==TRUE)
count(df$cy==TRUE)
count(df$cm==TRUE)
#Deteccion de tweet que contienen la cadena de texto CHEM
count(df$cm2==TRUE)

#Tweets repetidos entre pares de las palabras chemical, chemistry y chem
table(df$cl,df$cy)
table(df$cl,df$cm)
table(df$cy,df$cm)

#ejemplo de tweets que contienen la cadena de texto CHEM y no con contienen las palabras
pepe<-df[df$cm2==TRUE & df$cl==FALSE & df$cy==FALSE & df$cm==FALSE,]
pepe[10:20]

#ejemplo de tweets que NO contienen la cadena de texto CHEM 
chem2<-df[df$cm2==TRUE,]
nochem<-df[df$cm2==FALSE,]
pepe[50:60]
nochem$sc=grepl("chem", nochem$screenName, ignore.case=TRUE)
count(nochem$cm==TRUE)


chemical<-df[df$cl==TRUE,]
chemistry<-df[df$cy==TRUE,]
chem<-df[df$cm==TRUE,]
chemalone<-df[df$cm2==TRUE & df$cl==FALSE & df$cy==FALSE & df$cm==FALSE,]

newdf<-rbind(chemical,chemistry,chem)
pepe<-unique(newdf)
pepe2<-!unique(newdf)
#Cuidado: Hay un problema con los repetidos. Com unique(newdf) los repetidos deberian ser 548
#En lugar de los 551 calculados a traves de los pares

chem2<-df[df$cm2==TRUE,]


