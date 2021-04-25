# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("tm", "skmeans", "slam", "rlist", "cluster", "clue", "clusterCrit", "RColorBrewer")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
path<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/"
setwd(path)

#Funcio L-method
Lmethod<-function(valores, clusters, titulo){
  crit<-valores
  cls<-clusters
  inipartition<-2
  maxpartition<-length(cls)-1
  amodels<-NULL
  bmodels<-NULL
  partition<-NULL
  while (inipartition<maxpartition){
    partition<-append(partition,inipartition)
    yadata<-crit[1:inipartition]
    ybdata<-crit[inipartition+1:maxpartition]  
    xadata<-cls[1:inipartition]  
    xbdata<-cls[inipartition+1:maxpartition] 
    amodel<-lm(yadata~xadata)
    bmodel<-lm(ybdata~xbdata)
    amodels<-append(amodels,list(amodel))
    bmodels<-append(bmodels,list(bmodel))
    #Sys.sleep(0.1)
    #print(inipartition)
    inipartition<-inipartition+1
  }
  
  amodelRMSE<-lapply(rep(1:length(amodels)), function(x) sqrt(sum(residuals(amodels[[x]])^2)/length(residuals(amodels[[x]]))))
  bmodelRMSE<-lapply(rep(1:length(bmodels)), function(x) sqrt(sum(residuals(bmodels[[x]])^2)/length(residuals(bmodels[[x]]))))
  amodelRMSE<-unlist(amodelRMSE)
  bmodelRMSE<-unlist(bmodelRMSE)
  aweight<-lapply(rep(1:length(amodelRMSE)), function(x) length(residuals(amodels[[x]]))/(maxpartition+1))
  bweight<-lapply(rep(1:length(bmodelRMSE)), function(x) length(residuals(bmodels[[x]]))/(maxpartition+1))
  aweight<-unlist(aweight)
  bweight<-unlist(bweight)
  modelRMSE<-aweight*amodelRMSE+bweight*bmodelRMSE
  
  windows(1024,680)
  par(mar=c(5,5,3,1))
  plot(x=rep(1:length(modelRMSE)),y=modelRMSE, 
       xlab="Number of clusters", ylab="Weighted RMSE", 
       main= titulo,
       pch=3, cex.main=1.5, cex.axis=1, cex.lab=1.5)
  
  partition[which.min(modelRMSE)]
  clusters[partition[which.min(modelRMSE)]]
  
  windows(1024,680)
  plot(x=cls[1:length(crit)],y=crit)
  model<-amodels[[partition[which.min(modelRMSE)]]]
  abline(coef=model$coefficients)
  model<-bmodels[[partition[which.min(modelRMSE)]]]
  abline(coef=model$coefficients)
  
  return(modelRMSE)
  
}


#Datos
filename<-"Best_skm_rep50.RData"
load(filename)
filename<-"Clusters_skm_rep50.RData"
load(filename)
filename<-"Stats_skm_rep50.RData"
load(filename)


#Grafica de Elbow method
crit<-unlist(lapply(best_skm ,
                    function(x) x[[1]]$value))
cls<-unlist(lapply(best_skm ,
                   function(x) nrow(x[[1]]$prototypes)))

#Gráfica de la función criterio Skmeans
windows(1024,680)
par(mar=c(5,5,3,1)) #set the margin (bottom, left, top , right)
plot(x=cls[1:length(crit)],y=crit, 
     xlab="Número de clústeres", ylab="Valor función criterio Skmeans", 
     main= "",
     pch=3)

#Dibuixa silouette mig
load("Resum_silouette.RData")
valores<-unlist(lapply(seq(from=1,to=length(clusters)), function(x) resum_silh[[x]]$avg.width))

windows(1024,680)
par(mar=c(5,5,3,1)) #set the margin (bottom, left, top , right)
plot(x=clusters, y=valores, 
     xlab="Número de clústeres", ylab="Valor silueta promedio",
     pch=3)



#Calculo de L method para el elbow
crit<-unlist(lapply(best_skm ,function(x) x[[1]]$value))
cls<-unlist(lapply(best_skm ,function(x) nrow(x[[1]]$prototypes)))
dat<-Lmethod(crit,cls, "L-method for Elbow graph")

ymin<-dat[which.min(dat)]
xmin<-which.min(dat)

windows(1024,680)
par(mar=c(5,5,3,1))
plot(x=rep(1:length(dat)),y=dat, 
     xlab="Number of clusters", ylab="Weighted RMSE",
     pch=3, cex.main=1.5, cex.axis=1, cex.lab=1.5)
points(xmin,ymin,pch=19,col="red",cex=1.5)
text(x=xmin,y=900, labels=paste("Min = 78 clusters"), col="red", font=2)

#Para la tesis
windows(1024,680)
par(mar=c(5,5,3,1))
plot(x=rep(1:length(dat)),y=dat, 
     xlab="Número de clústeres", ylab="RMSE Ponderado",
     pch=3)
points(xmin,ymin,pch=19,col="red",cex=1.5)
text(x=xmin,y=900, labels=paste("Min = 78 clústeres"), col="red", font=2, cex=1.25)


#Calculo de L method para silouette
load("Resum_silouette.RData")
valores<-unlist(lapply(seq(from=1,to=length(clusters)), function(x) resum_silh[[x]]$avg.width))
#Lmethod(valores[1:150],clusters[1:150], "C) L-method for silhouette")
dat<-Lmethod(valores,clusters, "L-method for silhouette graph")

ymin<-dat[which.min(dat)]
xmin<-which.min(dat)

windows(1024,680)
par(mar=c(5,5,3,1))
plot(x=rep(1:length(dat)),y=dat, 
     xlab="Number of clusters", ylab="Weighted RMSE", 
     main= "L-method for silhouette graph",
     pch=3, cex.main=1.5, cex.axis=1, cex.lab=1.5)
points(xmin,ymin,pch=19,col="red",cex=1.5)
text(x=xmin,y=0.013, labels=paste("Min = 78 clusters"), col="red", font=2)

#Para la tesis
windows(1024,680)
par(mar=c(5,5,3,1))
plot(x=rep(1:length(dat)),y=dat, 
     xlab="Número de clústeres", ylab="RMSE Ponderado", 
     pch=3)
points(xmin,ymin,pch=19,col="red",cex=1.5)
text(x=xmin,y=0.013, labels=paste("Min = 78 clústeres"), col="red", font=2, cex=1.25)


#Calculo mas preciso de curvatura con la segunda derivada y con el modulo de curvatura
valores<-unlist(lapply(best_skm ,function(x) x[[1]]$value))
curva.spl <- smooth.spline(clusters,valores, df = 3)
curvatura_aprox<-predict(curva.spl, x = clusters, deriv = 2)
windows(1024,680)
plot(x=curvatura_aprox$x,y=curvatura_aprox$y)
clusters[which.max(curvatura_aprox$y)]

deriv2<-predict(curva.spl, x = clusters, deriv = 2)
deriv1<-predict(curva.spl, x = clusters, deriv = 1)
curvatura<-deriv2$y/(1+deriv1$y^2)^(3/2)

xmax<-which.max(curvatura)+1
ymax<-curvatura[xmax]


windows(1024,680)
par(mar=c(5,5,3,1))
plot(x=curvatura_aprox$x,y=curvatura, ylab="Curvature module", xlab="Number of clusters",
     main="Curvature module for elbow graph", 
     pch=3, cex.main=1.5, cex.axis=1, cex.lab=1.5)
clusters[which.max(curvatura)]

points(xmax,ymax,pch=19,col="red",cex=1.5)
text(x=xmax,y=1.75e-6, labels=paste("Max = 193 clusters"), col="red", font=2)


#Para la tesis
windows(1024,680)
par(mar=c(5,5,3,1))
plot(x=curvatura_aprox$x,y=curvatura, ylab="Módulo de curvatura", xlab="Número de clústeres",
     pch=3)
clusters[which.max(curvatura)]

points(xmax,ymax,pch=19,col="red",cex=1.5)
text(x=xmax,y=1.75e-6, labels=paste("Max = 193 clústeres"), col="red", font=2, cex=1.1)




#Calculo mas preciso de curvatura con la segunda derivada y con el modulo de curvatura
load("Resum_silouette.RData")
valores<-unlist(lapply(seq(from=1,to=length(clusters)), function(x) resum_silh[[x]]$avg.width))
curva.spl <- smooth.spline(clusters,valores, df = 3)
windows(1024,680)
plot(x=curva.spl$x,y=curva.spl$y)

data_plot<-data.frame(cbind(clusters,valores, curva.spl$y))
colnames(data_plot)<-c("clusters","silhouette", "spline")

curvatura_aprox<-predict(curva.spl, x = clusters, deriv = 2)
windows(1024,680)
plot(x=curvatura_aprox$x,y=curvatura_aprox$y)
clusters[which.min(curvatura_aprox$y)]

deriv2<-predict(curva.spl, x = clusters, deriv = 2)
deriv1<-predict(curva.spl, x = clusters, deriv = 1)
curvatura<-abs(deriv2$y/(1+deriv1$y^2)^(3/2))

xmax<-which.max(curvatura)
ymax<-curvatura[xmax]

windows(1024,680)
par(mar=c(5,5,3,1))
plot(x=curvatura_aprox$x,y=curvatura, xlab="Number of clusters", ylab="Curvature module",
     main="Curvature module for silhouette graph", 
     pch=3, cex.main=1.5, cex.axis=1, cex.lab=1.5)
points(xmax,ymax,pch=19,col="red",cex=1.5)
text(x=xmax,y=7e-6, labels=paste("Max = 98 clusters"), col="red", font=2)
clusters[which.max(curvatura)]

#Para la tesis
windows(1024,680)
par(mar=c(5,5,3,1))
plot(x=curvatura_aprox$x,y=curvatura, xlab="Número de clústeres", ylab="Módulo de curvatura",
     pch=3)
points(xmax,ymax,pch=19,col="red",cex=1.5)
text(x=xmax,y=7e-6, labels=paste("Max = 98 clústeres"), col="red", font=2, cex=1.1)
clusters[which.max(curvatura)]

