# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("tm", "skmeans", "slam", "parallel")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
#for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Install) {install.packages(pckg, repos="https://cloud.r-project.org/")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
#setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final")
setwd(".")

#Carga de datos
#fichero<-"Dtm_CyClCm_20160101to20160630_f.RData"
fichero<-"Dtm_f.RData"
load(fichero)




#Calcula elbow method
#set.seed(1234)

#no_cores <- detectCores() - 1
#cl <- makeCluster(no_cores, type="FORK", outfile="debug_file.txt")
#clusterSetRNGStream(cl,1234)

#no_cores <- detectCores()
#cl <- makeCluster(no_cores, type="FORK", outfile="debug_file.txt")

for (k in seq(2, 285, by = 1)){
    #no_cores <- detectCores() - 1
    #cl <- makeCluster(no_cores, type="FORK")
    #clusterSetRNGstream(1234)
    #clusterSetRNGStream(cl,1234)
    
    #no_cores <- detectCores()
    no_cores <- 30
    cl <- makeCluster(no_cores, type="PSOCK", outfile="debug_file.txt")
    clusterSetRNGStream(cl,1234)
    clusterExport(cl, c("skmeans", "dtm_small", "k"))
    skm<-parLapply(cl,rep(1,50), function(x) skmeans(dtm_small,k*x, method="genetic",control=list(popsize=50)))
    #skm<-parLapply(cl,seq(105,200, by=5), function(x) skmeans(dtm_small,x, method="genetic",control=list(popsize=50)))
    #skm<-append(skm,skmaux)
    #skm<-parLapply(cl,seq(5,10, by=1), function(x) (k*x))
    #k<-k+1
    stopCluster(cl)
    filename<-paste("Elbow", k, "_rep50.RData", sep="")
    #filename<-paste("Elbow500_rep", j,".RData", sep="")
    save(skm,file=filename)
    #stopCluster(cl)
    rm(skm)
    gc()
    #topCluster(cl)
    #filename<-paste("Elbow500_rep", j,".RData", sep="")
    #save(skm,file=filename)
}

#stopCluster(cl)



