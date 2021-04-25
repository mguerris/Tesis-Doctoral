# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("tm", "skmeans", "slam", "rlist")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
#path<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
#setwd(path)
setwd(".")


ncluster<-285
stats_skm<-list()
best_skm<-list()
clusters<-NULL

for (k in seq(2, ncluster, by = 1)){
  filename<-paste("Elbow", k, "_rep50.RData", sep="")
  load(filename)
  data<-unlist(lapply(skm, `[`, 'value') )
  stats<-summary(data)
  best<-skm[which.min(data)]
  stats_skm<-list.append(stats_skm,stats)
  best_skm<-list.append(best_skm,best)
  clusters<-append(clusters, k)
  Sys.sleep(0.1)
  print(k)
}

#filename<-paste("Best_skm_rep50.RData", sep="")
save(best_skm,file="Best_skm_rep50.RData")
#filename<-paste(path, "Clusters_skm_rep50.RData", sep="")
save(clusters,file="Clusters_skm_rep50.RData")
save(stats_skm,file="Stats_skm_rep50.RData")

