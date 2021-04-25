rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("skmeans", "rlist", "qcc", "dplyr", "pastecs")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
#for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Install) {install.packages(pckg, repos="https://cloud.r-project.org/")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}


#Directorio de trabajo
setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")
#setwd(".")

#Carga de datos
#fichero<-"Dtm_CyClCm_20160101to20160630_f.RData"
#load(fichero)
#fichero<-"Seeds.RData"
#load(fichero)

nclusters<-100
ncores<-15
ntimes<-6000/ncores

#skmglobal<-NULL

skmvalue <- vector(mode="numeric", length=6000)
repeticion <- vector(mode="integer", length=6000)
bloque <- vector(mode="integer", length=6000)
posicion <- vector(mode="integer", length=6000)
#Pone todos los resultados en un Ãºnico fichero y sus respectivos seeds
for (j in 1:ntimes){
  Sys.sleep(0.0001)
  print(j)
  load(paste("Skmeans_ncluster_",nclusters, "_rep",j ,"_ncores",ncores,".RData", sep=""))
  for(i in 1:15){
      skmvalue[(j-1)*ncores+i]<-skm[[i]]$value  
      bloque[(j-1)*ncores+i]<-j 
      posicion[(j-1)*ncores+i]<-i 
  }
}  
repeticion<-seq(1:6000)
resultados<-data.frame(repeticion,bloque,posicion,skmvalue)

save(resultados,file="Resultados6000rep_100clusters.RData")
windows()
plot(repeticion,skmvalue)

summary(resultados$skmvalue)

minim<-which.min(skmvalue)
j<-resultados[minim,]$bloque
pos<-resultados[minim,]$posicion

load(paste("Skmeans_ncluster_",nclusters, "_rep",j ,"_ncores",ncores,".RData", sep=""))
bestskm<-skm[pos]

save(bestskm,file="Bestskm6000rep_100clusters.RData")

#Analitza les Series de resultats 1,2 i 3
#Serie1
setwd("D:/Skmeans_Serie01")
ntimes<-1229
skmserie1 <- vector(mode="numeric", length=ntimes)
for (j in 1:ntimes){
  Sys.sleep(0.0001)
  print(j)
  load(paste("Skmeans_Serie1_ncluster_100_rep",j ,".RData", sep=""))
  skmserie1[j]<-skm$value  
} 
save(skmserie1,file="ResultadosSerie1_clusters.RData")
pos<-which.min(skmserie1)
load(paste("Skmeans_Serie1_ncluster_100_rep",pos ,".RData", sep=""))
bestskmserie1<-skm
save(bestskmserie1,file="Bestskm_Serie1_clusters.RData")

#Serie2
setwd("D:/Skmeans_Serie02")
ntimes<-1281
skmserie2 <- vector(mode="numeric", length=ntimes)
for (j in 1:ntimes){
  Sys.sleep(0.0001)
  print(j)
  load(paste("Skmeans_Serie2_ncluster_100_rep",j ,".RData", sep=""))
  skmserie2[j]<-skm$value  
} 
save(skmserie2,file="ResultadosSerie2_clusters.RData")
pos<-which.min(skmserie2)
load(paste("Skmeans_Serie2_ncluster_100_rep",pos ,".RData", sep=""))
bestskmserie2<-skm
save(bestskmserie2,file="Bestskm_Serie2_clusters.RData")


#Serie3
setwd("D:/Skmeans_Serie03")
ntimes<-1213
skmserie3 <- vector(mode="numeric", length=ntimes)
for (j in 1:ntimes){
  Sys.sleep(0.0001)
  print(j)
  load(paste("Skmeans_Serie3_ncluster_100_rep",j ,".RData", sep=""))
  skmserie3[j]<-skm$value  
} 
save(skmserie3,file="ResultadosSerie3_clusters.RData")
pos<-which.min(skmserie3)
load(paste("Skmeans_Serie3_ncluster_100_rep",pos ,".RData", sep=""))
bestskmserie3<-skm
save(bestskmserie3,file="Bestskm_Serie3_clusters.RData")


#Encuentra el mejor de todos los datos
setwd("D:/Skmeans_Serie01")
load("Bestskm_Serie1_clusters.RData")
setwd("D:/Skmeans_Serie02")
load("Bestskm_Serie2_clusters.RData")
setwd("D:/Skmeans_Serie03")
load("Bestskm_Serie3_clusters.RData")
setwd("D:/Resultats 15cores")
load("Bestskm6000rep_100clusters.RData")

valors_min<-c(bestskm[[1]]$value,bestskmserie1$value,bestskmserie2$value,bestskmserie3$value)
valors_nom<-c("Bestskm6000rep_100clusters.RData","Bestskm_Serie1_clusters.RData",
              "Bestskm_Serie2_clusters.RData","Bestskm_Serie2_clusters.RData")
which.min(valors_min)
valors_nom[which.min(valors_min)]

#Dibuixa plot de tots els resultats
setwd("D:/Resultats 15cores")
load("Resultados6000rep_100clusters.RData")
repmax<-max(resultados[,1])

setwd("D:/Skmeans_Serie01")
load("ResultadosSerie1_clusters.RData")
noulimit<-length(skmserie1)

setwd("D:/Skmeans_Serie02")
load("ResultadosSerie2_clusters.RData")
noulimit<-noulimit+length(skmserie2)

setwd("D:/Skmeans_Serie03")
load("ResultadosSerie3_clusters.RData")
noulimit<-noulimit+length(skmserie3)

vector_rep<-c(resultados$repeticion,seq(repmax+1,repmax+noulimit))
vector_resul<-c(resultados$skmvalue,skmserie1,skmserie2,skmserie3)
windows(1024,680)
par(mar=c(5,5,3,1))
plot(x=vector_rep,y=vector_resul, ylab="Valor función criterio Skmeans", xlab="Repetición", 
     pch=3, cex.main=1.5, cex.axis=1, cex.lab=1.5)

#Analisis del mejor resultado
setwd("D:/Resultats 15cores")
load("Bestskm6000rep_100clusters.RData")

windows()
datos<-data.frame(table(bestskm[[1]]$cluster))

boxplot(datos$Freq, horizontal = TRUE, xlab="Tweets por clúster",
        pars=list(outcol="red", outcex=1.5, outpch=21, outlwd=1.5, outbg="yellow", cex.lab=1.5))


