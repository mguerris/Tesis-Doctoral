# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("qdapRegex", "textcat", "tm", "RWeka", "stringr")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")

#Carga de datos
fichero<-"CyClCm_20160101to20160630.RData"
load(fichero)

#Deteccion de tweets que contienen las PALABRAS chemical, chemistry y chem
#tweetsGlobalTable$cy=grepl("\\bchemistry\\b", tweetsGlobalTable$text, ignore.case=TRUE)
#tweetsGlobalTable$cl=grepl("\\bchemical\\b", tweetsGlobalTable$text, ignore.case=TRUE)
#tweetsGlobalTable$cm=grepl("\\bchem\\b", tweetsGlobalTable$text, ignore.case=TRUE)

#Analisis de hashtags para ver si aportan o no informacion a los tweets
#hashtags<-str_extract_all(tweetsGlobalTable$text,"\\S*[#]\\S*")
#hashtags<-unlist(hashtags)
#tabla_hashtags<-as.data.frame(table(hashtags))
#tabla_hashtags<-tabla_hashtags[order(-tabla_hashtags$Freq),]
#Existen hashtags que aportan informaci?n relevante y por tanto mejor no eliminar los hashtags

#Analisis de usuarios para ver si aportan o no informacion a los tweets
#usuarios<-str_extract_all(tweetsGlobalTable$text,"\\S*[@]\\S*")
#usuarios<-unlist(usuarios)
#tabla_usuarios<-as.data.frame(table(usuarios))
#tabla_usuarios<-tabla_usuarios[order(-tabla_usuarios$Freq),]
#Los usuarios no parecen aportar informaci?n y por tanto los eliminaremos

#Conversion a utf-8
#La conversion a utf-8 de los tweets que no lo son afecta al contenido y hace que se pierdan
#tweets que una vez limpios no se detectan correctamente con el idioma ingles
#se perdian 7567 tweets por hacer la conversion
#tweetsGlobalTable$text<-iconv(tweetsGlobalTable$text,'utf-8')

#Deteccion de tweets que contienen RT. Servir? analizar los que son retweets
tweetsGlobalTable$conRT=grepl('\\brt\\b',tweetsGlobalTable$text, ignore.case=TRUE)

#Limpieza de tweets
minlenparaules<-2
docs<-tweetsGlobalTable$text
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

tweetsGlobalTable$txtc=docs

#Deteccion de lenguaje de tweets no limpiados y limpiados
tweetsGlobalTable$lang_txt=textcat(tweetsGlobalTable$text)
tweetsGlobalTable$lang_txtc=textcat(tweetsGlobalTable$txtc)

#Guardamos todas las operaciones realizadas en un fichero
save(tweetsGlobalTable,file="CyClCm_20160101to20160630_c.RData")

#Seleccionamos aquellos que no son RT y reconocidos con el lenguaje ingles en el texto limpio
#y que la cadena de texto no esta vacia y que no estan repetidos
selec_tweets<-NULL
selec_tweets<-subset(tweetsGlobalTable, lang_txtc=="english" & conRT==FALSE & isRetweet==FALSE & txtc!="")
selec_tweets<-selec_tweets[!duplicated(selec_tweets$txtc),]

#Guardamos el conjunto de tweets seleccionados
save(selec_tweets,file="CyClCm_20160101to20160630_f.RData")

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
selec_tweets$txtc<-removeWords(selec_tweets$txtc,new_stopw)
selec_tweets$txtc<- gsub("^\\s+|\\s+$", "", selec_tweets$txtc)
selec_tweets<-subset(selec_tweets, txtc!="")
selec_tweets<-selec_tweets[!duplicated(selec_tweets$txtc),]

#Guardamos los tweets finales
save(selec_tweets,file="CyClCm_20160101to20160630_ff.RData")

#Creamos el Corpus 
myCorpus <- VCorpus(VectorSource(selec_tweets$txtc))
#asignamos los id de los tweets al id del corpus
meta(myCorpus, "id")<-selec_tweets$id

#Guardamos el Corpus
save(myCorpus,file="Corpus_CyClCm_20160101to20160630_f.RData")

#Calculamos la matriz tdm de bigramas de palabras contiguas
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = NumbergramTokenizer))
#asignamos los id de los tweets al nombre de los docs del tdm
tdm$dimnames$Docs<-selec_tweets$id

#Guardamos el tdm
save(tdm,file="Tdm_CyClCm_20160101to20160630_f.RData")
load("Tdm_CyClCm_20160101to20160630_f.RData")


