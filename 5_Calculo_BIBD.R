# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("ibd", "crossdes", "agricolae", "installr", "tester", "rlist")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
path<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/"
setwd(path)

#Calcul de combinacions de BIBD

combinations<-data.frame()
#clusters
b<-100
#v=raters
for(v in 2:30){
  #k=raters per cluster
  for(k in 2:v){
    r<-(k*b)/v
    lambda<-(k-1)*r/(v-1)
    if (is_integer(r) && is_integer(lambda)){
      combinations<-rbind(combinations,c(v,b,k,r,lambda))
    }
  }
}
names(combinations)<-c("v", "b", "k","r","lambda")

filas<-length(rownames(combinations))
bibd.good <- vector(mode = "logical", length = filas);
combinations<-cbind(combinations,bibd.good)

list_bibd<-list()
list_resul<-list()
for (i in 1:filas) {
  Sys.sleep(0.001)
  print(i)
  bibd <- find.BIB(combinations[i,1], combinations[i,2], combinations[i,3]);
  bibd.resul<-capture.output(isGYD(bibd))
  list_bibd<-list.append(list_bibd,bibd)
  list_resul<-list.append(list_resul,bibd.resul)
}
save(combinations, file="BIBD_Combinations.RData")
save(list_bibd, file="BIBD.RData")
save(list_resul, file="BIBD_feasibility.RData")


load("BIBD_Combinations.RData")
load("BIBD.RData")
load("BIBD_feasibility.RData")

filas<-length(rownames(combinations))
bibd.resul <- vector(mode = "character", length = filas);
bibd.resul<-(lapply(seq(1:filas), function(x) list_resul[[x]][2]))
dif_bib<-unique(bibd.resul)
dif_bib

bibd.resul[[3]]

for(i in 1:filas){
  if(bibd.resul[[i]]=="[1] The design is a balanced incomplete block design w.r.t. rows."){
    combinations[i,6]<-TRUE
  }
}

comb_true<-combinations[combinations[,6]=="TRUE",]

#BIBD escogido
set.seed(1234)
#lanzamos 1000 veces
resul2<-bibd(6,100,50,3,20, ntrial=1000,pbar=TRUE)

save(resul2,file="BIBD_finalresul.RData")
load("BIBD_finalresul.RData")

clusters_rater<-matrix(, nrow = 50, ncol = 18)
for(i in 1:6){
  clusters<-which(resul2$N[i,]==1)
  clusters_rater[,i]<-clusters
  clusters_rater[,i+6]<-clusters
  clusters_rater[,i+12]<-clusters
}

table_clusters<-t(resul2$N)

#reorder randomly clusters for every rater
set.seed(1234)
for(i in 1:18){
  orden<-sample(1:50,50,rep=FALSE)
  clusters_rater[,i]<-clusters_rater[orden,i]
}
#reprder order of raters randomly
set.seed(1234)
orden<-sample(1:18,18,rep=FALSE)
aux<-matrix(, nrow = 50, ncol = 18)
for(i in 1:18){
  aux[,i]<-clusters_rater[,orden[i]]
}

clusters_rater<-aux
save(clusters_rater,file="BIBd_clustersrater.RData")

#tabla
load("BIBd_clustersrater.RData")
t(clusters_rater[1:50,1:6])
write.csv(t(clusters_rater[1:50,1:6]), file="BIBD_design.csv")

#todos los raters
load("BIBd_clustersrater.RData")
resul_entry<-data.frame()
ev_number<-NULL
cluster<-NULL
category<-NULL
for(i in 1:18){
  ev_number<-append(ev_number,rep.int(i,50))
  cluster<-append(cluster,clusters_rater[,i])
  category<-append(category,rep.int(0,50))
}

resul_entry<-data.frame(ev_number,cluster,category)
write.csv(resul_entry, file="Template_entrada_resul_evs.csv")


