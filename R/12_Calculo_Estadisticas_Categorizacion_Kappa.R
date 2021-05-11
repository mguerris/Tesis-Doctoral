# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("irr", "rel", "raters","readxl", "rlist","tm","skmeans", "RWeka", "psych", "lme4", "xlsx")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
path<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/"
setwd(path)

#Carga de clusters categorizados
datos<-read.csv("Entrada datos evaluadores.csv", header=TRUE, sep=";")

#Carga de resultado de skmeans para saber los tweets
load("Bestskm_100clusters.RData")
load("Corpus_CyClCm_f.RData")
load("CyClCm_ff.RData")


clusters<-100
evaluators<-18
ratings<-matrix(nrow=clusters,ncol=evaluators)


#Carrega de dades en ratings
for(i in 1:900){
  ratings[datos$cluster[i],datos$ev_number[i]]<-datos$category[i]
}

colnames(ratings)<-c("Rater 1", "Rater 2", "Rater 3","Rater 4", "Rater 5", "Rater 6",
                         "Rater 7", "Rater 8", "Rater 9","Rater 10", "Rater 11", "Rater 12",
                         "Rater 13", "Rater 14", "Rater 15","Rater 16", "Rater 17", "Rater 18")

#Creem matriu de clusters i nombre de vegades que s'ha votat cada cluster
#segon les categories
clusters<-100
categories<-6
cat_ratings<-matrix(0,nrow=clusters,ncol=categories)
for(i in 1:100){
  aux_table<-table(ratings[i,])
  val_table<-as.vector(aux_table)
  pos_table<-as.integer(names(aux_table))
  n<-length(aux_table)
  cat_ratings[i,pos_table[1:n]]<-val_table[1:n]
}

colnames(cat_ratings)<-c("Cat 1", "Cat 2", "Cat 3","Cat 4", "Cat 5", "Cat 6")

#calcul dels mes votats donant a la categoria mes votada un 1
#si varies categories son màximes, aleshores li donem un 1 a indeterminat
clusters<-100
categories<-6
cat_max<-matrix(0,nrow=clusters,ncol=categories)
for(i in 1:100){
  n<-which(cat_ratings[i,] == max(cat_ratings[i,] ))
  if (length(n)==1){
    cat_max[i,n]<-1
  }else{
    cat_max[i,6]<-1
  }
}

colnames(cat_max)<-c("Cat 1", "Cat 2", "Cat 3","Cat 4", "Cat 5", "Cat 6")

#Calcul dels tweets de cada cluster
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
km_result<-bestskm[[1]]
corpus_docs<-unlist(lapply(myCorpus,as.character))
idmyCorpus<-selec_tweets$id
names(corpus_docs)<-idmyCorpus
number_tweets<-vector(mode="integer",length = 100)
for(i in 1:100) {
  docs_cl<-names(km_result$cluster)[km_result$cluster==i]
  equal<-match(docs_cl, names(corpus_docs))
  number_tweets[i]<-length(corpus_docs[equal])
}

#Calcul de la matriu de tweets per categoria
clusters<-100
categories<-6
tweets_max<-matrix(0,nrow=clusters,ncol=categories)
for(i in 1:100){
  tweets_max[i,]<-cat_max[i,]*number_tweets[i]
}

colnames(tweets_max)<-c("Cat 1", "Cat 2", "Cat 3","Cat 4", "Cat 5", "Cat 6")

#suma de todos los clusters maximos por categorias
#suma de todos los tweets por categorias
sum_cats_max<-colSums(cat_max)
sum_tweets_max<-colSums(tweets_max)

#Calcul de les votacions totals atenent a les votacions individuals
sum_cats_ratings<-colSums(cat_ratings)

#calcul de les votacions totals ponderades segons els tweets
#normalitzem cada fila pel total de votacions per cluster (9)
#i ho ponderem segons el nombre de tweets
#Calcul de la matriu de tweets per categoria
clusters<-100
categories<-6
cat_pond<-matrix(0,nrow=clusters,ncol=categories)
cat_pond<-cat_ratings/9
for(i in 1:100){
  cat_pond[i,]<-cat_pond[i,]*number_tweets[i]
}

colnames(cat_pond)<-c("Cat 1", "Cat 2", "Cat 3","Cat 4", "Cat 5", "Cat 6")

#Calcul de la suma de votacions ponderades pel nombre de tweets
#La suma sense ponderar de votacions totals esta a sum_cat_ratings
sum_cats_ponds<-colSums(cat_pond)

#Muntem un dataframe amb els diferents resultats
resul_ratings<-data.frame(sum_cats_ratings,sum_cats_ponds,sum_cats_max,sum_tweets_max)

#Calculem el mateix data_frame amb el porcentatges de cada variable
#Resultats en tant per u
per_ratings<-resul_ratings
for(i in 1:4){
  per_ratings[,i]<-resul_ratings[,i]/sum(resul_ratings[,i])
}

#Recomposem el ordre original dels raters
#Atenent a com es van distribuir originalment
set.seed(1234)
orden<-sample(1:18,18,replace = FALSE)
aux_ratings<-matrix(nrow=clusters,ncol=evaluators)
for(i in 1:evaluators){
  aux_ratings[,orden[i]]<-ratings[,i]
}

ratings_list<-list()
for(i in 1:6){
  rater<-matrix(nrow=clusters,ncol=3)
  rater[,1]<-aux_ratings[,i]
  rater[,2]<-aux_ratings[,i+6]
  rater[,3]<-aux_ratings[,i+12]
  ratings_list<-list.append(ratings_list, rater)
}


#Chequeos
colSums(!is.na(ratings))
rowSums(!is.na(ratings))

#Extraccion de clusters segun categorias
#Segun categoria maxima
clusters_catmax<-vector(mode="integer",length = 100)
clusters_catmax<-NULL
clusters_catmax<-as.vector(cat_max%*%c(1,2,3,4,5,6))
save(clusters_catmax, file="Clusters_por_categoria.RData")

#Calculamos el ratings list quitando las celdas que no son NA
full_raterlist<-list()
for (j in 1:6){
  empty_matr <- matrix(nrow=50, ncol=3) 
  pos<-which(!is.na(ratings_list[[j]]), TRUE)
  for (i in 1:50){
    empty_matr[i,]<-ratings_list[[j]][pos[i,1],]
  }
  full_raterlist<-list.append(full_raterlist,empty_matr)
}
full_raterlist[[6]]


#Calculamos el kappa de fleiss por grupos de raters, sin paradojas, por categoria y global
#La funcion kappa de fleiss peta si se utilizan NA
#Y da diferente si se utiliza celdas vacias junto con celdas llenas o bien celdas llenas
#Los considera como ceros y por tanto aumenta el agreement
#Es necesario calcularlo con la matriz llena y por tanto seleccionar los elementos llenos


kappa_raterlist<-list()
for(i in 1:6){
  kappa_raterlist<-list.append(kappa_raterlist,kappam.fleiss(full_raterlist[[i]], exact = FALSE, detail = FALSE))
}

values<-NULL
statistic<-NULL
pvalue<-NULL
for(i in 1:6){
  values<-c(values, kappa_raterlist[[i]]$value)
  statistic<-c(statistic, kappa_raterlist[[i]]$statistic)
  pvalue<-c(pvalue, kappa_raterlist[[i]]$p.value)
}
kappa_fleiss_rater<-cbind.data.frame(values,statistic,pvalue)

#Calculem el interval de confian?a del kappa de Fleiss i el kappa, amb confidence level 95%
values<-list()
est<-NULL
se<-NULL
lb<-NULL
ub<-NULL
for(i in 1:6){
  values<-list.append(values,spi(data =full_raterlist[[i]] , weight = c("unweighted"), conf.level = 0.95))
  est<-c(est,values[[i]]$est)
  se<-c(se,values[[i]]$se)
  lb<-c(lb,values[[i]]$lb)
  ub<-c(ub,values[[i]]$ub)
}
fleiss_conf<-cbind.data.frame(est,se,lb,ub)

#Calcul de kappa sense paradoxes
fleiss_noparadox<-list()
for (j in 1:6){
  matr<-matrix(0,nrow=clusters,ncol=categories)
  for(i in 1:100){
    matr[i,ratings_list[[j]][i,1]]<-matr[i,ratings_list[[j]][i,1]]+1
    matr[i,ratings_list[[j]][i,2]]<-matr[i,ratings_list[[j]][i,2]]+1
    matr[i,ratings_list[[j]][i,3]]<-matr[i,ratings_list[[j]][i,3]]+1
  }
  zeros<-rowSums(matr)
  matr_nozeros<-matr[-which(zeros==0),]
  fleiss_noparadox<-list.append(fleiss_noparadox,
                                concordance(matr_nozeros, test = "MC", B = 1000, alpha = 0.05))
}  

est<-NULL
pe<-NULL
lb<-NULL
ub<-NULL
for(j in 1:6){
  est<-c(est,fleiss_noparadox[[j]]$Fleiss[1])
  pe<-c(pe,fleiss_noparadox[[j]]$Statistic[4])
  lb<-c(lb,fleiss_noparadox[[j]]$Statistic[2])
  ub<-c(ub,fleiss_noparadox[[j]]$Statistic[3])
}
fleiss_conf_noparadox<-cbind.data.frame(est,lb,ub,pe)


#Kappa de fleiss por categorias
#pesos para calcular la kappa de fleiss global
pj<-colSums(cat_ratings)/sum(colSums(cat_ratings))
qj<-1-pj
n<-100
m<-9
k_j<-vector(mode="double", length=6)
i<-1
for (i in 1:6){
  numerador<-sum(cat_ratings[,i]*(m-cat_ratings[,i]))
  denominador<-(n*m*(m-1)*pj[i]*qj[i])
  k_j[i]<-1-(numerador/denominador)
}
error_kj<-sqrt(2/(n*m*(m-1)))
z_kj<-k_j/error_kj
pvalue_kj<-pnorm(z_kj, lower.tail = FALSE)
er_kj<-rep(error_kj,times=6)
print(k_j)
print(pvalue_kj)
print(z_kj)
print(error_kj)

kappa_categorias<-cbind(k_j,error_kj,z_kj,pvalue_kj)

#kglobal
x2<-0
for (i in 1:n){
  x2<-x2+sum(cat_ratings[i,]*cat_ratings[i,])
}
numerador<-(n*m*m)-x2
denominador<-n*m*(m-1)*sum(pj*qj)
kglobal<-1-(numerador/denominador)
#error de kglobal
denominador<-sqrt(n*m*(m-1))*sum(pj*qj)
numerador<-sqrt(2)*sqrt(sum(pj*qj)^2-sum(pj*qj*(qj-pj)))
error_kglobal<-numerador/denominador
z<-kglobal/error_kglobal
p_value<-pnorm(z, lower.tail = FALSE)

kappa_global<-cbind(kglobal, error_kglobal, z, p_value)
print(c(kglobal, error_kglobal, z, p_value))



#Resultados a Excel
filename<-"Resultados Clasificacion.xlsx"
write.xlsx2(ratings, file=filename, sheetName="Clusters evaluados por rater",
            col.names=TRUE, row.names=TRUE, append=FALSE, password=NULL)
write.xlsx2(cat_ratings, file=filename, sheetName="Votaciones por categoria",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
write.xlsx2(cat_max, file=filename, sheetName="Votaciones maximas por categoria",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
write.xlsx2(tweets_max, file=filename, sheetName="Tweets votados maximos por categoria",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
write.xlsx2(cat_pond, file=filename, sheetName="Tweets votados ponderados por categoria",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
write.xlsx2(resul_ratings, file=filename, sheetName="Resultados vots por categoria",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
write.xlsx2(per_ratings, file=filename, sheetName="Resultados percentuales por categoria",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
write.xlsx2(fleiss_conf, file=filename, sheetName="Kappa Fleiss por grupos de raters",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
write.xlsx2(fleiss_conf_noparadox, file=filename, sheetName="Kappa Fleiss sinparadox grupos de raters",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
write.xlsx2(kappa_categorias, file=filename, sheetName="Kappa Fleiss por categorias",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)
write.xlsx2(kappa_global, file=filename, sheetName="Kappa Fleiss Global",
            col.names=TRUE, row.names=TRUE, append=TRUE, password=NULL)

