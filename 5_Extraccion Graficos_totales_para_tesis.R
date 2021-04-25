# Fent neteja
rm(list=ls())

# Paquets a instal?lar/carregar
#pckgs<-c("tm","wordcloud","wordcloud2", "slam","RWeka","ngram",
#         "cluster","NbClust","skmeans","igraph",
#         "network","sna","ndtv", "gridExtra", "grid","gridBase", 
#         "RColorBrewer", "png", "magick", "webshot", "htmlwidgets", "multipanelfigure", "tcltk")

pckgs<-c("tm","skmeans", "RWeka","slam",
         "grid","magick","gridExtra", "wordcloud", "multipanelfigure")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

# Establir directori de treball
#Variables globales
pathorigin<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
pathsortida<-paste(pathorigin,"/Grafics separats per paper",sep="")
setwd(pathorigin)


load("Corpus_CyClCm_20160101to20160630_f.RData")
load("CyClCm_20160101to20160630_ff.RData")
setwd("D:/Resultats 15cores")
load("Bestskm6000rep_100clusters.RData")

km_result<-bestskm[[1]]
setwd(pathorigin)
idmyCorpus<-selec_tweets$id

NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
maxterms<-100
nraters<-1
i<-1

for(i in 1:nraters){
  
  pdfname<-paste("Rater_Total_", i, sep="")  
  nclu<-100
  
  cl_sizes<-table(km_result$cluster)
  ordered_clu<-order(cl_sizes,decreasing=TRUE)
  ordered_clu<-rep(1:100)
  corpus_docs<-unlist(lapply(myCorpus,as.character))
  names(corpus_docs)<-idmyCorpus
  selected_clu<-ordered_clu

  j<-1
  
  lista_fig1<-NULL
  lista_fig2<-NULL
  lista_titulo<-NULL
  
  #Genera los 2 wordclouds, los titulos y guarda los wordcclouds en png en disco duro
  
  setwd(pathsortida)
  
  for(cl in selected_clu) {
    Sys.sleep(0.001)
    print(paste("Genera Wordclouds para", pdfname, "Cluster", j))
    
    docs_cl<-names(km_result$cluster)[km_result$cluster==cl]
    equal<-match(docs_cl, names(corpus_docs))
    texts_cl<-corpus_docs[equal]

    corpus_gr<-VCorpus(VectorSource(texts_cl))
    tdm_gr<-TermDocumentMatrix(corpus_gr)
    lisWC_gr<-data.frame(terms=Terms(tdm_gr),freq=row_sums(tdm_gr))
    lisWC_gr<-lisWC_gr[order(lisWC_gr$freq,decreasing=TRUE),]
    minfreq<-lisWC_gr$freq[maxterms+1]+1
    if(is.na(minfreq)) {minfreq<-2}
    lisWC_gr<-lisWC_gr[lisWC_gr$freq>=minfreq,]
    
    titulopagina<-paste("Cluster ",cl," Number of tweets = ", nDocs(tdm_gr)," ", j, "/", nclu, sep="")
    name_fig1<-paste(pdfname,"_fig1_Cluster", cl, ".png", sep="")
    name_fig2<-paste(pdfname,"_fig2_Cluster", cl, ".png", sep="")
  
    lista_titulo<-append(lista_titulo,titulopagina)
    lista_fig1<-append(lista_fig1, name_fig1)
    lista_fig2<-append(lista_fig2, name_fig2)
    
    Sys.sleep(0.001)
    print("Genera Wordclouds unigramas")
    
    png(name_fig1, units="mm", width=210, height=130, res=300)
    wordcloud(lisWC_gr$terms,lisWC_gr$freq,scale=c(1.5,.5),rot.per=0,fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))
    dev.off()
    
    Sys.sleep(0.001)
    print("OK Wordclouds unigramas")
    
    tdm_gr <- TermDocumentMatrix(corpus_gr, control = list(tokenize = NumbergramTokenizer))
    lisWC_gr<-data.frame(terms=Terms(tdm_gr),freq=row_sums(tdm_gr))
    lisWC_gr<-lisWC_gr[order(lisWC_gr$freq,decreasing=TRUE),]
    minfreq<-lisWC_gr$freq[maxterms+1]+1
    if(is.na(minfreq)) {minfreq<-2}
    lisWC_gr<-lisWC_gr[lisWC_gr$freq>=minfreq,]
    
    Sys.sleep(0.001)
    print("Genera Wordclouds bigramas")
  
    png(name_fig2, units="mm",width=210, height=130, res=300)
    wordcloud(lisWC_gr$terms,lisWC_gr$freq,scale=c(1.5,.5),rot.per=0,fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))
    dev.off()
    
    j=j+1
  }
  
  #Junta los wordclouds y el titulo en una pagina y lo guarda
  # en disco duro en formato png cada pagina
  j<-1
  list_figure<-NULL
  for(cl in selected_clu) {
    Sys.sleep(0.001) 
    print(paste("Genera pagina", j, "para", pdfname))
    
    name_figure<-paste(pdfname,"_figure_Cluster", cl, ".png", sep="")
    list_figure<-append(list_figure,name_figure)
    
    figure1 <- multi_panel_figure(
      width = 210, height = c(10,130,130,7), columns=1, panel_label_type ="none")
    figure1<-fill_panel(figure1, textGrob(lista_titulo[j]),row=1, column=1)
    figure1<-fill_panel(figure1, lista_fig1[j],row=2, column=1)
    figure1<-fill_panel(figure1, lista_fig2[j],row=3, column=1)
    save_multi_panel_figure(figure1,name_figure)
    j<-j+1
  }
  
  #Monta el fichero pdf a partir de las paginas generadas y guardadas
  j<-1
  pdf(file=paste(pdfname,".pdf",sep=""), width=8.267, height=11.692)
  
  for(cl in selected_clu) {
    grid.newpage()
    Sys.sleep(0.001)
    print(paste("Genera pdf", pdfname, "Cluster",j))
  
    img<-image_read(list_figure[j])
    grid.raster(img)
    
    j<-j+1
  }
  dev.off()
}



