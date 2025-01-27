rm(list=ls())

################################################################################
################################ MISCELLANEOUS #################################
################################################################################

## Dependencies installation
  if("twitteR" %in% rownames(installed.packages()) == FALSE) {
    install.packages("twitteR")
  }
  if("tidyverse" %in% rownames(installed.packages()) == FALSE) {
    install.packages("tidyverse")
  }
  if("tm" %in% rownames(installed.packages()) == FALSE) {
    install.packages("tm")
  }
  if("SnowballC" %in% rownames(installed.packages()) == FALSE) {
    install.packages("SnowballC")
  }
  if("wordcloud" %in% rownames(installed.packages()) == FALSE) {
    install.packages("wordcloud")
  }
  if("textcat" %in% rownames(installed.packages()) == FALSE) {
    install.packages("textcat")
  }
  if("igraph" %in% rownames(installed.packages()) == FALSE) {
    install.packages("igraph")
  }
  if("ggraph" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggraph")
  }

if("sna" %in% rownames(installed.packages()) == FALSE) {
  install.packages("sna")
}

## Dependencies loading
  library("twitteR")
  library("tidyverse")
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("textcat")
  library("igraph")
  library("ggraph")
  library("sna")
  library("rlist")

setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")

################################################################################
############################## CREATE nodes and edges ##################################
################################################################################

  load("EnglishMentioned_VS_ScreenName_all.RData")
  links_SNA <- EnglishRelations
  oldColumns <- c(1, 2, 3)
  newColumns <- c("to", "from", "weight")
  colnames(links_SNA)[oldColumns] <- newColumns
  links_SNA <- links_SNA[, c("from", "to", "weight")]
  save(links_SNA, file="links_SNA_all.rda")

  sc <- links_SNA %>% group_by(from) %>% summarise(outI=sum(weight)) %>% 
    arrange(-outI) %>% mutate(name=from) %>% select(name,outI)
  me <- links_SNA %>% group_by(to) %>% summarise(inI=sum(weight)) %>% 
    arrange(-inI) %>% mutate(name=to) %>% select(name,inI)
  
  nodes <- merge(x=sc,y=me,by="name",all=T)
  nodes[is.na(nodes)] <- 0
  
  nodes$totI <- nodes$outI + nodes$inI
  
  nodes <- nodes[order(-nodes$totI),]
  rownames(nodes) <- (1:nrow(nodes))
  save(nodes, file="nodes_SNA_all.rda")

  topMentioned <- nodes[order(-nodes$inI),]
  rownames(topMentioned) <- (1:nrow(topMentioned))
  save(topMentioned, file="topMentioned_all.rda")
  topScreenNames <- nodes[order(-nodes$outI),]
  rownames(topScreenNames) <- (1:nrow(topScreenNames))
  save(topScreenNames, file="topScreenNames_all.rda")
  
  
  #Calcul par�metres de la xarxa global
  load("links_SNA_all.rda")
  load("nodes_SNA_all.rda")
  load("topMentioned_all.rda")
  load("topScreenNames_all.rda")
  
  #Calculamos la red total
  g <- igraph::graph_from_data_frame(links_SNA, directed=FALSE, vertices=nodes)
  g <- igraph::simplify(g, remove.multiple = F, remove.loops = T)
  
  save(g,file="Red_ALL.RData")
  
  #Parametros de la red global 
  length(V(g))
  length(E(g))
  
  eg<-igraph::degree(g)
  summary(deg)
  clo<-igraph::closeness(g)
  summary(clo)
  bet<-igraph::betweenness(g)
  summary(bet)
  eig<-igraph::eigen_centrality(g)
  summary(eig$vector)
  
  g <- igraph::simplify(g, remove.multiple = T, remove.loops = T)
  g_density<-igraph::edge_density(g)
  g_density
  g_inclusivity<-igraph::vertex.connectivity(g,checks = TRUE)
  g_inclusivity
  
  dist<-distances(g)
  mean_distance(g)
  ecc<-eccentricity(g)
  
  diam<-diameter(g)
  
  #Subredes dentro de la red
  dg<-decompose.graph(g)
  
  nodes$cluster<-0
  for(i in 1:length(dg)){
    names_vertexs<-V(dg[[i]])$name
    pos_nodes<-which(nodes$name %in% names_vertexs)
    nodes[pos_nodes,"cluster"]<-i
    Sys.sleep(0.01)
    print(i)
  }

  
  #numero de subredes
  length(dg)
  num_vertex<-sapply(1:length(dg), function(x) length(V(dg[[x]])))
  num_edge<-sapply(1:length(dg), function(x) length(E(dg[[x]])))
  menciones_1<-sapply(1:length(dg), function(x) sum(E(dg[[x]])$weight))
  menciones_2<-sapply(1:length(dg), function(x) sum(V(dg[[x]])[]$totI))
  vertex_max<-function(red){
    maxim<-(max(V(red)[]$totI))
    pos_max<-which(V(red)[]$totI==maxim)
    nom_max<-V(red)[[pos_max]]$name
    return(nom_max)
  }
  nom_maxims<-sapply(1:length(dg), function(x) vertex_max(dg[[x]]))
  menciones_por_vertice<-menciones_1/num_vertex
  menciones_por_vertice2<-menciones_2/num_vertex
  data_tree<-data.frame(nom_maxims, num_vertex,menciones_1,menciones_2,
                        menciones_por_vertice,menciones_por_vertice2, stringsAsFactors = FALSE)
  
  diameters<-sapply(1:length(dg), function(x) diameter(dg[[x]]))
  inclusivity<- sapply(1:length(dg), function(x) vertex.connectivity(dg[[x]], checks = TRUE))
  av_distance<- sapply(1:length(dg), function(x) mean_distance(dg[[x]], directed = FALSE))
  
  g <- igraph::simplify(g, remove.multiple = T, remove.loops = T)
  dg<-decompose.graph(g)
  density<- sapply(1:length(dg), function(x) edge_density(dg[[x]]))
  
  data_net<-data.frame(nom_maxims, num_vertex,num_edge, diameters,
                       density, inclusivity, av_distance, stringsAsFactors = FALSE)
  
  
  #Parametros de las subredes
  summary(data_net[,2:7])
  summary(data_tree[,2:5])
  
  stats::quantile(num_vertex, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))
  stats::quantile(num_edge, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))
  stats::quantile(diameters, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))
  density_nona<-density[!is.na(density)]
  stats::quantile(density_nona, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))
  stats::quantile(inclusivity, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))
  av_distance_nona<-av_distance[!is.na(av_distance)]
  stats::quantile(av_distance_nona, c(0.95, 0.99, 0.995, 0.996, 0.997, 0.998, 0.999))
  
  #Analisis visual de los nodos que tienen m�s menciones para detectar si est�n o no conectados
  #No miramos si est�n conectados indirectamente sino si est�n conectados entre ellos de forma directa
  df<-nodes[order(-nodes$totI),]
  selnodes<-df[1:500,"name"]
  nodes_to_view<-df[1:50,"name"]
  
  net<-igraph::induced_subgraph(g,V(g)[name %in% selnodes])
  net<-igraph::simplify(net, remove.multiple = F, remove.loops = T)
  
  windows(1024,768)
  g_ego_layout<-ggraph(net, layout = 'igraph', algorithm = 'kk') + 
    geom_edge_link(aes(width = E(net)$weight), colour="blue", alpha=0.25)+
    geom_node_point(colour="black", size=0.25) +
    geom_node_label(aes(label=ifelse(igraph::V(net)$name %in% nodes_to_view,V(net)$name,NA)), size=3, 
                    label.padding = unit(0.10, "lines"),
                    label.r = unit(0.05, "lines"),
                    label.size = 0.15, repel=T) +
    scale_edge_width(range = c(0.5, 5))+
    theme_classic() +
    theme(axis.title = element_blank(),axis.text = element_blank(),
          axis.line = element_blank(), axis.ticks = element_blank(),
          legend.position="none")
  print(g_ego_layout)
  
  #Calculamos los nodos aislados de los 500 y por tanto sin menciones
  #Son aquellos que tienen grado 0
  load("Red_ALL.RData")
  load("nodes_SNA_all.rda")
  df<-nodes[order(-nodes$totI),]
  selnodes<-df[1:500,"name"]
  
  net<-igraph::induced_subgraph(g,V(g)[name %in% selnodes])
  net<-igraph::simplify(net, remove.multiple = F, remove.loops = T)
  
  nodes_aislados<-length(which(igraph::degree(net)==0))
  porcentaje_nodes_aislados<-nodes_aislados/500
  
  
#--------------------------------------------------------------------------  
  #Calculo de redes egocentricas de los mas mencionados
  rm(list = setdiff(ls(), lsf.str()))
  setwd("C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")
  setwd("C:/Users/mguerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats popularitat")
  load("Red_ALL.RData")
  load("topMentioned_all.rda")
  mentioned<-topMentioned$name[1:5]
  
  load("topScreenNames_ALL.rda")
  mentioned<-topScreenNames[1:5,"name"]
  
  diam<-78
  
  l_ego<-list()
  for(node in mentioned){
    ego_mentioned<-ego(g,order=diam,nodes=node, mode="all")
    g_ego <- induced_subgraph(g,unlist(ego_mentioned))
    g_ego <- igraph::simplify(g_ego, remove.multiple = T, remove.loops = T)
    l_ego<-list.append(l_ego,g_ego)
  }  
  
  
  i<-1
  
  for(net in l_ego){
    if(i==1){cat("Mencion","Diametro","Densidad","Inclusividad","Distancia media","\n")}
    cat(mentioned[i],diameter(net), 
        edge_density(net), vertex.connectivity(net,checks = TRUE),
        mean_distance(net,directed = FALSE),"\n")
    i<-i+1
  }  
  
  i<-1
  for(net in l_ego){
    if(i==1){cat("Mencion","Vertices","Aristas","\n")}
    cat(mentioned[i],length(V(net)), length(E(net)), "\n") 
    i<-i+1
  } 


#Busqueda de los usuarios que m�s mencionan en las redes egoc�ntricas  
  load("topScreenNames_all.rda")
  mention<-topScreenNames[1:5,"name"]
  i<-1
  for(net in l_ego){
    pos<-which(V(net)$name %in% mention)
    Sys.sleep(0.01)
    print(V(net)[pos]$name)
    i<-i+1
  } 

  
  #############################################################################
  #Representacion visual de las redes egocenctricas
  #etiquetas de los nodos mas mencionados y que mas mencionan
  ############################################################################
  load("topMentioned_all.rda")  
  load("topScreenNames_all.rda")
  nodes_to_view<-c(topMentioned$name[1:5],topScreenNames$name[1:5])
  nodes_to_view
  
  #Tratamiento de la red mayor para simplificarla
  bignet<-l_ego[[1]]
  selecnet <- induced_subgraph(bignet,which(V(bignet)$totI>4))
  selecnet <- igraph::simplify(selecnet, remove.multiple = F, remove.loops = T)
  V(selecnet)
  E(selecnet)
  
  
    windows(1024,768)
    g_ego_layout<-ggraph(selecnet, layout = 'igraph', algorithm = 'kk') + 
      geom_edge_link(aes(width = E(selecnet)$weight), colour="blue", alpha=0.25)+
      geom_node_point(colour="black", size=0.25) +
      geom_node_label(aes(label=ifelse(igraph::V(selecnet)$name %in% nodes_to_view,V(selecnet)$name,NA)), size=3, 
                      label.padding = unit(0.10, "lines"),
                      label.r = unit(0.05, "lines"),
                      label.size = 0.15, repel=T) +
      scale_edge_width(range = c(0.5, 5))+
      theme_classic() +
      theme(axis.title = element_blank(),axis.text = element_blank(),
            axis.line = element_blank(), axis.ticks = element_blank(),
            legend.position="none")
    print(g_ego_layout)
  

  i<-1
  for(net in l_ego){
    windows(1024,768)
    g_ego_layout<-ggraph(net, layout = 'igraph', algorithm = 'kk') + 
      geom_edge_link(aes(width = E(net)$weight), colour="blue", alpha=0.25)+
      geom_node_point(colour="black", size=0.25) +
      geom_node_label(aes(label=ifelse(igraph::V(net)$name %in% nodes_to_view,V(net)$name,NA)), size=3, 
                      label.padding = unit(0.10, "lines"),
                      label.r = unit(0.05, "lines"),
                      label.size = 0.15, repel=T) +
      scale_edge_width(range = c(0.5, 5))+
      theme_classic() +
      theme(axis.title = element_blank(),axis.text = element_blank(),
            axis.line = element_blank(), axis.ticks = element_blank(),
            legend.position="none")
    print(g_ego_layout)
  }
  
  
  
##############################################################################  
#Busqueda tweets de las redes egocenctricas totales#########################
###########################################################################
rm(list = setdiff(ls(), lsf.str()))
load("All_mentions.RData")
load("Red_ALL.RData")
load("topMentioned_all.rda")
mentioned<-topMentioned$name[1:5]
load("Tweets_nostop_limpios_conduplicados.RData")

load("topScreenNames_all.rda")
mentioned<-topScreenNames$name[1:5]

diam<-78

l_ego<-list()
for(node in mentioned){
  ego_mentioned<-ego(g,order=diam,nodes=node, mode="all")
  g_ego <- induced_subgraph(g,unlist(ego_mentioned))
  g_ego <- igraph::simplify(g_ego, remove.multiple = F, remove.loops = T)
  l_ego<-list.append(l_ego,g_ego)
}  

names_node<-lapply(seq(1:5), function(x) V(l_ego[[x]])$name)
menciones$data_mentions<-str_remove(menciones$data_mentions,"@")
extrae_id<-function(menciones, nombres){
  pos1<-which(menciones$data_mentions %in% nombres)
  pos2<-which(menciones$screenName %in% nombres)
  pos3<-c(pos1,pos2)
  pos3<-unique(pos3)
  ids<-menciones[pos3,"id"]
  return(ids)
}  
tweets_ids<-lapply(seq(1:5), function(x) extrae_id(menciones,names_node[[x]]))
pos_tweets<-lapply(seq(1:5), function(x) which(df2$id %in% tweets_ids[[x]]))
tweets_unique<-lapply(seq(1:5), function(x) unique(df2[pos_tweets[[x]], "txtc"]))

tweets_unique_raw<-lapply(seq(1:5), function(x) unique(df2[pos_tweets[[x]], "text"]))

extrae_tweets_unicos<-function(texto, usuario, categoria){
  fichero=paste(categoria,"_", usuario,"_Tweets_unicos.csv",sep="")
  fichero_xlsx=paste(categoria,"_",usuario,"_Tweets_unicos.xlsx",sep="")
  write.csv(texto, file=fichero, row.names = FALSE)
  library("openxlsx")
  write.xlsx(read.csv(fichero), fichero_xlsx)
} 

lapply(seq(1:5), function(x) extrae_tweets_unicos(tweets_unique_raw[[x]], mentioned[[x]],"ALL"))

tweets_por_mencionados<-data.frame(mentioned, unlist(lapply(seq(1:5), function(x) length(tweets_unique[[x]]))), stringsAsFactors = FALSE)
colnames(tweets_por_mencionados)<-c("mencionados", "n_tweets")
words_in_tweets<-sapply(seq(1:5), function(x) length(unique(unlist(str_extract_all(tweets_unique[[x]], "[[:alpha:]]+")))))
tweets_por_mencionados$unique_words<-words_in_tweets

text_tw_joint<-lapply(seq(1:5), function(x) paste(tweets_unique[[x]], collapse = ''))

myCorpus <- lapply(seq(1:5), function(x) VCorpus(VectorSource(text_tw_joint[[x]])))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]]))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))

pinta_wc<-function(freq, titulo, usuario, unibi){
  fichero=paste("WC_",titulo, "_", usuario, "_", unibi,".png",sep="")
  png(fichero, units="mm",width=210, height=100, res=300)
  wordcloud(names(freq[1]$`1`), freq[1]$`1`,scale=c(1.5,.5),rot.per=0,fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))
  dev.off()
} 

wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"ALL_ego_network", mentioned[[x]],1))

library(RWeka)
NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- lapply(seq(1:5), function(x) TermDocumentMatrix(myCorpus[[x]], control = list(tokenize = NumbergramTokenizer)))
frequent<- lapply(seq(1:5), function(x) findMostFreqTerms(tdm[[x]], n = 100))
wc<- lapply(seq(1:5), function(x) pinta_wc(frequent[[x]],"ALL_ego_network", mentioned[[x]],2))


  
