# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("tm", "skmeans", "slam")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")

#Carga de datos
#fichero<-"Tdm_CyClCm_20160101to20160630_f.RData"
fichero<-"Tdm_f.RData"
load(fichero)

#Convertimos la tdm a dtm con TfIdf
#Seleccionamos aquellos terminos con una frecmin en la tdm
frecmin<-30
tdm_small<-tdm[findFreqTerms(tdm,frecmin),]
#Seleccionamos aquellos documentos no vacios
dtm_aux<-t(tdm_small)
dtm_aux<-dtm_aux[row_sums(dtm_aux)!=0,]
tdm_small<-t(dtm_aux)

#save(tdm_small, file="Tdm_CyClCm_20160101to20160630_ff.RData")
save(tdm_small, file="Tdm_ff.RData")

#Calculamos la traspuesta dtm con TfIdf en lugar de con las frecuencias 
#Y s?lo cogemos aquellos
dtmTdIdf<-t(weightTfIdf(tdm_small, normalize = TRUE))
dtm_small<-dtmTdIdf[row_sums(dtmTdIdf)!=0,]

#Guardamos el dtm
#save(dtm_small,file="Dtm_CyClCm_20160101to20160630_f.RData")
save(dtm_small,file="Dtm_f.RData")

