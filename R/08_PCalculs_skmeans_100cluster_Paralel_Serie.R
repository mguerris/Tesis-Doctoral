rm(list=ls())
gc()

# Paquets a instalar/carregar
pckgs<-c("skmeans","doParallel", "foreach")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
#for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Install) {install.packages(pckg, repos="https://cloud.r-project.org/")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
#setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")
setwd(".")

#Calculo de listas de seeds
seeds<-randomSequence(min=1, max=10000, col=1, check=TRUE)
save(seeds,file="Seeds.RData")
seeds<-sample(10001:50000, 10000, rep=FALSE)
save(seeds,file="Seeds2.RData")
seeds<-sample(50001:60000, 10000, rep=FALSE)
save(seeds,file="Seeds3.RData")
seeds<-sample(60001:70000, 10000, rep=FALSE)
save(seeds,file="Seeds4.RData")

#Carga de datos y calculo skmeans en paralelo
fichero<-"Dtm_f.RData"
load(fichero)
fichero<-"Seeds.RData"
load(fichero)

nclusters<-100
ncores<-15
ntimes<-6000/ncores


for (j in 1:ntimes){
  cl <- makeCluster(ncores, type="PSOCK", outfile="debug_file_provparaleltrocej2.txt")
  registerDoParallel(cl)
  skm <- foreach(i=1:ncores, .export=c('skmeans', 'j', 'seeds', 'ncores'), .errorhandling='pass', .packages=c('skmeans')) %dopar% {
    set.seed(seeds[(j-1)*ncores+i])
    skmeans(dtm_small,nclusters, method="genetic",control=list(popsize=50))  
  }
  stopCluster(cl)
  filename<-paste("Skmeans_ncluster_",nclusters, "_rep",j ,"_ncores",ncores,".RData", sep="")
  save(skm,file=filename)
  rm(skm)
  gc()
}  


#Carga de datos y calculo skmeans en serie I
fichero<-"Dtm_f.RData"
load(fichero)
fichero<-"Seeds2.RData"
load(fichero)

nclusters<-100

for (j in 1:length(seeds)){
  set.seed(seeds[j])
  skm <- skmeans(dtm_small,nclusters, method="genetic",control=list(popsize=50))  
  filename<-paste("Skmeans_Serie1_ncluster_",nclusters, "_rep",j ,".RData", sep="")
  save(skm,file=filename)
}  


#Carga de datos y calculo skmeans en serie II
fichero<-"Dtm_f.RData"
load(fichero)
fichero<-"Seeds3.RData"
load(fichero)

nclusters<-100

for (j in 1:length(seeds)){
  set.seed(seeds[j])
  skm <- skmeans(dtm_small,nclusters, method="genetic",control=list(popsize=50))  
  filename<-paste("Skmeans_Serie2_ncluster_",nclusters, "_rep",j ,".RData", sep="")
  save(skm,file=filename)
}  


#Carga de datos y calculo skmeans en serie III
fichero<-"Dtm_f.RData"
load(fichero)
fichero<-"Seeds4.RData"
load(fichero)

nclusters<-100

for (j in 1:length(seeds)){
  set.seed(seeds[j])
  skm <- skmeans(dtm_small,nclusters, method="genetic",control=list(popsize=50))  
  filename<-paste("Skmeans_Serie3_ncluster_",nclusters, "_rep",j ,".RData", sep="")
  save(skm,file=filename)
}  



