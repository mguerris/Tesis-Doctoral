# Fent neteja
rm(list=ls())

# Paquets a instal?lar/carregar
#pckgs<-c("tm","wordcloud","wordcloud2", "slam","RWeka","ngram",
#         "cluster","NbClust","skmeans","igraph",
#         "network","sna","ndtv", "gridExtra", "grid","gridBase", 
#         "RColorBrewer", "png", "magick", "webshot", "htmlwidgets", "multipanelfigure", "tcltk")

pckgs<-c("tm","skmeans", "RWeka","slam",
         "grid","magick","gridExtra", "wordcloud", "multipanelfigure",
         "staplr")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Sys.getenv()
#webshot::install_phantomjs()

# Establir directori de treball
#Variables globales
pathorigin<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
pathsortida<-paste(pathorigin,"/Grafics separats",sep="")
setwd(pathorigin)


load("Corpus_CyClCm_f.RData")
load("CyClCm_ff.RData")
load("BIBd_clustersrater.RData")
setwd("D:/Resultats 15cores")
load("Bestskm_100clusters.RData")
#load("20160720_skmeans_CyClCm_wCh_woNo_k150f30r170best.RData")
km_result<-bestskm[[1]]
setwd(pathorigin)

idmyCorpus<-selec_tweets$id

#definicio taula descriptiva
#library(gridExtra)
df = matrix(ncol=3,nrow=7)
tit1<-paste("Actividad", "Humana",sep="\n")
def1<-paste("T?rminos relacionados con la presencia de la qu?mica en la vida diaria", 
            "Implica alguna manifestaci?n relacionada con la industria o producci?n qu?mica"
            ,sep="\n")
tit2<-paste("Conocimiento","Cient?fico",sep="\n")
def3<-paste("T?rminos relacionados con la qu?mica como asignatura de clase", 
            "as? como actividades comunes de estudiantes"
            ,sep="\n")
def4<-paste("T?rminos relacionados con manifestaciones culturales:", 
            "canci?n, grupo de m?sica, concierto, pel?cula, serie de televisi?n ..."
            ,sep="\n")
tit5<-paste("Relaci?n", "Humana",sep="\n")
df[1,] = c("Categor?a","Definici?n", "Elecci?n")
df[2,] = c(tit1, def1, "")
df[3,]= c(tit2, "T?rminos relacionados con conceptos o entidades abstractas qu?micas","")
df[4,] = c("Docencia", def3,"")
df[5,]= c("Espectaculo", def4,"")
df[6,]= c(tit5, "T?rminos relacionados con la emoci?n o sentimiento entre personas
.","")
df[7,]= c("Indeterminado", "T?rminos principales mezclados de diferentes categor?as","")
df = as.data.frame(df)
#colors<-brewer.pal(7,"Pastel2")
#tt1 <- ttheme_minimal(base_size=8,core=list(bg_params = list(fill = colors[1:7], col=NA), fg_params=list(fontface=1)))
tt2 <- ttheme_default(base_size=8, 
                      core=list(fg_params=list(hjust=0, x=0.01)),
                      rowhead=list(fg_params=list(hjust=1, x=0.01)))

taula_descriptiva<-tableGrob(df,rows=NULL, cols=NULL, theme=tt2)
windows()
grid.draw(taula_descriptiva)



#definicio colors taula descriptiva
#colors<-c("#FFFFCC","#C7E9B4","#7FCDBB","#40B6C4","#2C7FB8" ,"#253494")
#display.brewer.all()

NumbergramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
maxterms<-100

nraters<-18
i<-1

for(i in 1:nraters){
  
  pdfname<-paste("Rater_", i, sep="")  
  #seleccion<-sample(1:100, 50, rep=FALSE)
  selected_clu<-clusters_rater[,i]
  nclu<-length(selected_clu)
  
  #cl_sizes<-table(km_result$cluster)
  #ordered_clu<-order(cl_sizes,decreasing=TRUE)
  #ordered_clu<-cl_sizes
  #selected_clu<-ordered_clu[which(ordered_clu %in% seleccion)]
  corpus_docs<-unlist(lapply(myCorpus,as.character))
  names(corpus_docs)<-idmyCorpus

  j<-1
  
  lista_fig1<-NULL
  lista_fig2<-NULL
  lista_titulo<-NULL
  
  #Genera los 2 wordclouds, los titulos y guarda los wordcclouds en png en disco duro
  
  setwd(pathsortida)
  
  for(cl in selected_clu) {
    #cl<-"98"
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
    
    #titulopagina<-paste("Cluster ",cl," n = ", nDocs(tdm_gr)," ", j, "/", nclu, sep="")
    titulopagina<-paste("Cluster ", j, "/", nclu, " ", "(", cl,")", sep="")
    name_fig1<-paste(pdfname,"_fig1_Cluster", cl, ".png", sep="")
    name_fig2<-paste(pdfname,"_fig2_Cluster", cl, ".png", sep="")
  
    lista_titulo<-append(lista_titulo,titulopagina)
    lista_fig1<-append(lista_fig1, name_fig1)
    lista_fig2<-append(lista_fig2, name_fig2)
    
    Sys.sleep(0.001)
    print("Genera Wordclouds unigramas")
    
    png(name_fig1, units="mm", width=210, height=100, res=300)
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
  
    png(name_fig2, units="mm",width=210, height=100, res=300)
    wordcloud(lisWC_gr$terms,lisWC_gr$freq,scale=c(1.5,.5),rot.per=0,fixed.asp=FALSE, random.order=F, colors=brewer.pal(8, "Dark2"))
    dev.off()
    
    j=j+1
  }
  
  #Junta los wordclouds, el titulo y la tabla descriptiva en una pagina y lo guarda
  # en disco duro en formato png cada pagina
  
  
  j<-1
  list_figure<-NULL
  for(cl in selected_clu) {
    Sys.sleep(0.001) 
    print(paste("Genera pagina", j, "para", pdfname))
    
    name_figure<-paste(pdfname,"_figure_Cluster", cl, ".png", sep="")
    list_figure<-append(list_figure,name_figure)
    
    figure1 <- multi_panel_figure(
      width = 210, height = c(10,100,100,67), columns=1, panel_label_type ="none")
    figure1<-fill_panel(figure1, textGrob(lista_titulo[j]),row=1, column=1)
    figure1<-fill_panel(figure1, lista_fig1[j],row=2, column=1)
    figure1<-fill_panel(figure1, lista_fig2[j],row=3, column=1)
    #figure1<-fill_panel(figure1, taula_descriptiva,row=4, column=1)
    figure1<-fill_panel(figure1, "Tabla_eleccion.png",row=4, column=1)
    save_multi_panel_figure(figure1,name_figure)
    j<-j+1
  }
  
  #Monta el fichero pdf a partir de las paginas generadas y guardadas
  
  #pb <- tkProgressBar(title = "Generating Pdf", min = 1, max = nclu, width = 300)
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

#inserta la primera pagina de instrucciones en cada uno del los documentos de los raters
setwd(pathsortida)
for (i in 1:18){
  file_1<-paste(pathsortida,"/Instrucciones_jc.pdf",sep="")
  file_2<-paste(pathsortida,"/Rater_",i,".pdf",sep="")
  file_sortida<-paste(pathsortida,"/",i,"_Rater_",i,".pdf",sep="")
  staple_pdf(input_directory=NULL,input_files=c(file_1, file_2),
             output_filepath = file_sortida)
  Sys.sleep(0.001)
  print(paste("Genera pdf ", file_sortida))
  
}



