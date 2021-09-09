library(psych)
library(tm)
library(lsa)
library(dplyr)
library(tidytext)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(e1071)
library(gmodels)
library(FactoMineR)
library(ggrepel)
library(scatterplot3d)
library(ggalt)
library(smacof)
library(caret)
library(caTools)
library(mclust)
library(MASS)

# Sustituir nombres de autores por sus abreviaturas
abrevia<-function(nom)
{
  df.nom<-data.frame("NOMBRE"=c("AaronPressman","AlanCrosby","AlexanderSmith","BenjaminKangLim","BernardHickey",
                                "BradDorfman","DarrenSchuettler","DavidLawder","EdnaFernandes","EricAuchard"),
                     "ABREVIATURA"=c("AP","AC","AS","BKL","BH","BD","DS","DL","EF","EA"))
  return(df.nom[which(df.nom$NOMBRE==nom),]$ABREVIATURA)
}

# Carga del conjunto de datos objeto de estudio. 
carga_datos.t.100<-function() # carga de conjunto de datos reducido
{
  # mis_autores<-c("AlexanderSmith","BenjaminKangLim","BradDorfman","DavidLawder")
  # mis_autores.2<-c("AaronPressman","AlanCrosby","BernardHickey","DarrenSchuettler","EdnaFernandes","EricAuchard")
  mis_autores<-c("AaronPressman","AlanCrosby","AlexanderSmith","BenjaminKangLim","BernardHickey",
                 "BradDorfman","DarrenSchuettler","DavidLawder","EdnaFernandes","EricAuchard")
  dir_datos<-c("R/C50/C50train","R/C50/C50test")
  # cuantos<-ifelse(tr,40,10)
  cuantos<-50
  vacio<-TRUE
  for(dir in dir_datos)
  {
    num<-0
    ld <- list.dirs(dir, recursive = FALSE)
    for (i in ld) 
    {
      # if (num<n.aut)
      # {
      num2<-0
      clase<-strsplit(i,'/')
      nombre<-clase[[1]][length(clase[[1]])]
      if (nombre %in% mis_autores)
      {
        num<-num+1
        lf <- list.files(i)
        for (j in lf) 
        {
          nom.fichero<-substr(j,0,nchar(j)-4)
          if(nombre!=nom.fichero)
          {
            num2<-num2+1
            if(num2<=cuantos)
            {
              archivo<-paste(i,"/",j,sep="")
              con <- file(archivo, open="r") # Abrimos la conexión
              documento <- readLines(con)
              close(con)
              txt<-unsplit(documento," ")
              nom_abrev<-abrevia(nombre)
              if (vacio)
              {
                datos<-data.frame(ARTICULO=txt,AUTOR=nom_abrev,NUM_AUTOR=num,FICHERO=nom.fichero,stringsAsFactors = F)
                vacio<-FALSE
              }
              else
                datos<-rbind(datos,data.frame(ARTICULO=txt,AUTOR=nom_abrev,NUM_AUTOR=num,FICHERO=nom.fichero,stringsAsFactors = F))
            }
          }
        }
      }
    }
  }
  rm(dir_datos,vacio,ld,clase,nombre,nom_abrev,lf,dir,i,j,archivo,con,documento,txt)
  return(datos)
}

carga_datos.4.100<-function() # carga de conjunto de datos reducido
{
  mis_autores<-c("AlexanderSmith","BenjaminKangLim","BradDorfman","DavidLawder")
  mis_abreviaturas<-c("AS","BKL","BD","DL")
  # mis_autores.2<-c("AaronPressman","AlanCrosby","BernardHickey","DarrenSchuettler","EdnaFernandes","EricAuchard")
  # mis_autores<-c("AaronPressman","AlanCrosby","AlexanderSmith","BenjaminKangLim","BernardHickey",
  #                "BradDorfman","DarrenSchuettler","DavidLawder","EdnaFernandes","EricAuchard")
  dir_datos<-c("R/C50/C50train","R/C50/C50test")
  # cuantos<-ifelse(tr,40,10)
  cuantos<-50
  vacio<-TRUE
  for(dir in dir_datos)
  {
    num<-0
    ld <- list.dirs(dir, recursive = FALSE)
    for (i in ld) 
    {
      # if (num<n.aut)
      # {
      num2<-0
      clase<-strsplit(i,'/')
      nombre<-clase[[1]][length(clase[[1]])]
      if (nombre %in% mis_autores)
      {
        num<-num+1
        lf <- list.files(i)
        for (j in lf) 
        {
          nom.fichero<-substr(j,0,nchar(j)-4)
          if(nombre!=nom.fichero)
          {
            num2<-num2+1
            if(num2<=cuantos)
            {
              archivo<-paste(i,"/",j,sep="")
              con <- file(archivo, open="r") # Abrimos la conexión
              documento <- readLines(con)
              close(con)
              txt<-unsplit(documento," ")
              if (vacio)
              {
                datos<-data.frame(ARTICULO=txt,AUTOR=nombre,NUM_AUTOR=num,FICHERO=nom.fichero,stringsAsFactors = F)
                vacio<-FALSE
              }
              else
                datos<-rbind(datos,data.frame(ARTICULO=txt,AUTOR=nombre,NUM_AUTOR=num,FICHERO=nom.fichero,stringsAsFactors = F))
            }
          }
        }
      }
    }
  }
  rm(dir_datos,vacio,ld,clase,nombre,lf,dir,i,j,archivo,con,documento,txt)
  return(datos)
}

carga_datos_textual<-function(todos,n)
{
  todos<-trata_txt(todos,n)
  todos$FICHERO<-as.character(todos$FICHERO)
  todos$FICHERO<-factor(todos$FICHERO)
  todos$AUTOR<-as.character(todos$AUTOR)
  todos$AUTOR<-factor(todos$AUTOR)
  return(todos)
}

trata_txt<-function(arts,n)
{
  
  arts$ARTICULO<-tolower(as.character(arts$ARTICULO))
  
  # arts$ARTICULO<-removeWords(arts$ARTICULO, words = ))
  arts$ARTICULO<-removeNumbers(arts$ARTICULO)
  arts$ARTICULO<-removePunctuation(arts$ARTICULO)
  arts$ARTICULO<-gsub("[[:cntrl:]]", " ", arts$ARTICULO)
  arts$ARTICULO<-removeWords(arts$ARTICULO, words = c("amp",stopwords("english")))
  arts$ARTICULO<-gsub(' . | .. ', ' ', arts$ARTICULO)
  arts$ARTICULO<-stripWhitespace(arts$ARTICULO)
  # arts$ARTICULO<-stemDocument(arts$ARTICULO,language="english")
  if (n==0)
    tidy_arts<-arts
  if (n==1)
    tidy_arts <- arts %>%
    unnest_tokens(word, ARTICULO)
  if (n>1)
    tidy_arts <- arts %>%
    unnest_tokens(word, ARTICULO, token = "ngrams", n = 2)
  
  return(tidy_arts)
}

terminos_f<-function(ti,numero)
{
  nuevo<-ti %>%
    count(word, sort = TRUE) %>%
    mutate(n=n,word = reorder(word, n))
  if (numero<1)
  {
    limite<-round(nrow(ti)*numero,0)    
    return(data.frame(TODOS=as.character(nuevo[nuevo$n>=limite,]$word),N=nuevo[nuevo$n>=limite,]$n))
  }
  else
    return(data.frame(TODOS=as.character(nuevo[1:numero,]$word),N=nuevo[1:numero,]$n))
}
g_term_freq<-function(ti,txt,frec,opc,tam)
{
  if(txt=="")
    txt<-ti$AUTOR
  nuevo<-ti %>%
    count(word, sort = TRUE) %>%
    mutate(n=n,word = reorder(word, n))
  if (opc==0)
  {
    g<-ggplot(nuevo[1:tam,],aes(word,n,fill=nuevo[1:tam,]$word)) +
      geom_col() +
      ggtitle(txt) + 
      xlab(NULL) +
      theme(legend.position = "none") +
      coord_flip()
  }
  else
  {
    paleta<-brewer.pal(3, "Paired")[c(2:1,3)]
    colores<-as.character(nuevo[1:tam,]$word)
    # porc<-round(100*(length(colores[!colores %in% frec$TODOS])/tam),0)
    lista<-colores %in% frec[1:tam,]$TODOS
    colores[lista]<-as.character(paleta[1])
    colores[!lista]<-as.character(paleta[2])
    switch(opc,colores_1<-colores,colores_2<-colores,colores_3<-colores)
    g<-ggplot(nuevo[1:tam,],aes(word,n,fill=switch(opc,colores_1,colores_2,colores_3))) + 
      geom_col() +
      scale_fill_brewer(palette="Paired") +
      ggtitle(txt) +
      xlab(NULL) +
      theme(legend.position = "none") +
      coord_flip()
  }
  # ggsave(paste("term_freq_",txt,".png",sep=""),g,path="graficos/textual")
  return(g)
}
dist_freq_terminos<-function(tt)
{
  words<- tt %>%
    count(AUTOR, word, sort = TRUE)
  total_words <- words %>% 
    group_by(AUTOR) %>% 
    summarize(total = sum(n))
  words <- left_join(words, total_words)
}
numerica<-function(m)
{
  tabla<-as.matrix(m)
  colnames(tabla)<-m$dimnames$Terms
  return(tabla)
}
evalua_confusion_global<-function(CONFUSION)
{
  return(sum(diag(CONFUSION))/sum(CONFUSION))
}
evalua_x_autor<-function(CONFUSION)
{
  TP<-diag(CONFUSION)
  filas<-rowSums(CONFUSION)
  columnas<-colSums(CONFUSION)
  # return(list(Precis=(TP/(columnas)),Recup=(TP/(filas)),A=(TP/(filas+columnas-(2*TP)))))
  return(list(Precis=(TP/(filas)),Recup=(TP/(columnas))))
}
