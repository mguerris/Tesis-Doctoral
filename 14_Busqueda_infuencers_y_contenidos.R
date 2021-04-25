# Fent neteja
rm(list=ls())

# Paquets a instalar/carregar
pckgs<-c("rlist","wordcloud", "stringr", "parallel", "xlsx")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Directorio de trabajo
#pathorigin<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final"
path<-"C:/Users/Manel Guerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats influencers"
path<-"C:/Users/mguerris/Dropbox/TRABAJO/DOCTORADO/Tesis doctoral/Experimentacion/Codigo final/Resultats influencers"
setwd(path)

########

#Carga de cuentas a buscar
load("usuariosInspección.Rdata")
NombreUsuarios<-as.character(sapply(UsersToInspect,function(x) x$name))
Nombrepantalla<-as.character(sapply(UsersToInspect,function(x) x$screenName))

#tweets del dataframe
load("Tweets_nostop_limpios_conduplicados.RData")

#Busqueda de tweets en los que el screename del tweet corresponde con el 
#screenname de la lista de entidades relevantes
l_tweets<-lapply(seq(1:length(Nombrepantalla)), function(x) which(df2$screenName %in% Nombrepantalla[x]))
long_ltweets<-sapply(l_tweets, length)
names_ltweets<-which(long_ltweets!=0)

#cuentas y nombre de cuentas con tweets
nombre<-NULL
cuenta<-NULL
tweet<-NULL
cat<-NULL
id<-NULL
#nombre, nombre de cuenta, id, tweets, categoria
for(i in 1:length(Nombrepantalla))
{
  if(long_ltweets[i]!=0)
  {
    for(j in 1:long_ltweets[i]){
      nombre<-c(nombre,NombreUsuarios[i])
      cuenta<-c(cuenta,Nombrepantalla[i])
      pos<-l_tweets[[i]][j]
      tweet<-c(tweet,df2[pos,"text"])
      #cat<-c(cat,selec_tweets[pos,"categoria"])
      id<-c(id,df2[pos,"id"])
    }
  }
} 
tweets_lscreen<-data.frame(nombre,cuenta,tweet,id, stringsAsFactors = FALSE)
save(tweets_lscreen, file="Tweets_lista_usuarios.RData")
write.csv(tweets_lscreen, file="01_Tweets_Screenname_Empresas.csv", row.names = FALSE)
library("openxlsx")
write.xlsx(read.csv("01_Tweets_Screenname_Empresas.csv"), "01_Tweets_Screenname_Empresas.xlsx")

#Buscamos ahora los de la lista que esten mencionados por otros usuarios
load("usuariosInspección.Rdata")
NombreUsuarios<-as.character(sapply(UsersToInspect,function(x) x$name))
Nombrepantalla<-as.character(sapply(UsersToInspect,function(x) x$screenName))

cuentas<-paste("@",Nombrepantalla, sep="")
cuentas<-str_replace(cuentas,"@Total", "@Total ")
tweets_mencionados<-sapply(seq(1:length(cuentas)), function(x) str_match(df2$text,cuentas[x]))
cuenta<-NULL
nombre<-NULL
usuario<-NULL
tw<-NULL
id<-NULL
for(i in 1:length(cuentas)){
  pos<-which(!is.na(tweets_mencionados[,i]))
  if(length(pos)!=0){
    for(j in 1:length(pos)){
      nombre<-c(nombre,NombreUsuarios[i])
      cuenta<-c(cuenta,cuentas[i])
      usuario<-c(usuario,df2[pos[j],"screenName"])
      tw<-c(tw,df2[pos[j],"text"])
      id<-c(id,df2[pos[j],"id"])
    }
  }  
}
tweets_mention<-data.frame(nombre,cuenta,usuario,tw,id, stringsAsFactors = FALSE)
save(tweets_mention, file="02_Tweets_lista_mencionados.RData")
write.csv(tweets_mention, file="02_Tweets_lista_mencionados.csv", row.names = FALSE)
library("openxlsx")
write.xlsx(read.csv("02_Tweets_lista_mencionados.csv"), "02_Tweets_lista_mencionados.xlsx")

load("02_Tweets_lista_mencionados.RData")
tbl<-table(tweets_mention$cuenta,tweets_mention$usuario)

pp<-as.matrix(tbl)
pp

tbl <- table(x)
tbl[order(-(tbl))]
tbl

#Estadisticas
load("Tweets_lista_usuarios.RData")
load("Tweets_clasificados.RData")

load("usuariosInspección.Rdata")
NombreUsuarios<-as.character(sapply(UsersToInspect,function(x) x$name))
Nombrepantalla<-as.character(sapply(UsersToInspect,function(x) x$screenName))

#Tweets emitidos por empresas y sociedades
cuentas_unicas<-unique(tweets_lscreen$cuenta)
sociedad_unica<-unique(tweets_lscreen$nombre)
cuentas_unicas
sociedad_unica
por_cuentas_lista<-length(cuentas_unicas)/length(Nombrepantalla)
por_cuentas_lista*100
por_cuentas<-length(cuentas_unicas)/length(unique(df2$screenName))
por_cuentas*100

8/80
tweets_unicos<-unique(tweets_lscreen$tweet)
por_tweets<-length(tweets_unicos)/length(rownames(df2))
por_tweets*100

#Tweets mencionados y usuarios que mencionan
cuentas_mencionadas<-unique(tweets_mention$cuenta)
sociedad_mencionada<-unique(tweets_mention$nombre)
usuarios_mencionan<-unique(tweets_mention$usuario)
cuentas_mencionadas
sociedad_mencionada
cat(sociedad_mencionada)

por_cuentas_lista<-length(cuentas_mencionadas)/length(Nombrepantalla)
por_cuentas_lista*100
por_cuentas<-length(cuentas_mencionadas)/length(unique(df2$screenName))
por_cuentas*100

tweets_unicos<-unique(tweets_mention$tw)
por_tweets<-length(tweets_unicos)/length(rownames(df2))
por_tweets*100
98/76242

library(dplyr)
tweets_group <- tweets_mention %>%
  select(nombre, cuenta, tw) %>%
  group_by(nombre) %>%
  summarise(menciones = length(tw))

summary(tweets_group$menciones)

tweets_group<-tweets_group[order(-tweets_group$menciones),] 

users_group<-tweets_mention %>%
  select(usuario, tw) %>%
  group_by(usuario) %>%
  summarise(menciones = length(tw))

users_group<-users_group[order(-users_group$menciones),] 
summary(users_group$menciones)

openxlsx::write.xlsx(tweets_group, file="Resultado menciones cuentas.xlsx")
openxlsx::write.xlsx(users_group, file="Resultado menciones usuarios.xlsx")

