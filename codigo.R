# RUTA, FUNCIONES, LIBRERÍAS...
# en PC TRABAJO
setwd("C:/Users/David/Google Drive/TFM/Plantilla")
# en mi mac
# setwd("~/Google Drive/TFM/Plantilla")
# source("R/librerias_aux_TFM.r") # librerias 
# source("R/funciones_TFM_0_Teoria.r") # funciones
# load("TFM_100.RData")
# load("capitulo06.RData")
# load("capitulo06_1.RData")
load("capitulo04_TFM-2.RData")
source("R/libreriasYfunciones.r")
load("capitulo04_TFM.RData")
# save.image("R/capitulo06.RData")
# save.image("capitulo06_0.RData")
# CARGA DE DATOS Y PREPROCESADO DEL CORPUS
# Creacion del dataframe que contiene los artículos y autores
articulos<-carga_datos.4.100()
table(articulos$AUTOR)
unique(articulos$AUTOR)

if (length(unique(articulos$AUTOR))==4)
{
  articulos$AUTOR<-replace(articulos$AUTOR,unique(articulos$AUTOR),c("AS","BKL","BD","DL"))
}else
{
  articulos$AUTOR<-replace(articulos$AUTOR,unique(articulos$AUTOR),c("AP","AC","AS","BKL","BH","BD","DS","DL","EF","EA"))
}
unique(articulos$AUTOR)

mi.corpus<-SimpleCorpus(VectorSource(articulos$ARTICULO))
mi.dtm<-DocumentTermMatrix(mi.corpus,control=list(language="english", tolower=TRUE, removeNumbers=TRUE, stripWhitespace=TRUE, removePunctuation = TRUE, 
                                                  stopwords = TRUE, stemming=TRUE))
mi.dtm$dimnames$Docs<-articulos$FICHERO
mi.dtm

tidy<-carga_datos_textual(articulos,1)
frecuentes<-terminos_f(tidy,.003)
df_fre<-data.frame(frecuentes$TODOS)
colnames(df_fre)<-c("TÉRMINOS")
gg1<-g_term_freq(tidy,"Palabras frecuentes",0,0,30)
gg1

gg.todos<-by(tidy,tidy$AUTOR,g_term_freq,"",frecuentes,1,30)

frecuentes<-terminos_f(tidy,.45)
autores<-unique(tidy$AUTOR)
todos_words<-dist_freq_terminos(tidy)
todos_words <- todos_words %>%
  bind_tf_idf(word, AUTOR, n)
terminos_tf_idf<-todos_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(AUTOR) %>%
  top_n(30)

for (i in 1:length(autores))
{
  terminos.tf.idf<-terminos_tf_idf[which(terminos_tf_idf$AUTOR==autores[i]),]
  gg.tfidf<-terminos.tf.idf[1:30,] %>%
    ungroup() %>%
    ggplot(aes(word, tf_idf, fill = AUTOR)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    coord_flip()

    (grid.arrange(gg.todos[[autores[i]]],gg.tfidf,ncol=2))
}

num.terminos<-length(unique(tidy$word))
todos_tf<-tidy[tidy$word %in% as.character(terminos_f(tidy,num.terminos)$TODOS),]

frequency.1 <- todos_tf %>%
  count(AUTOR, word) %>%
  group_by(AUTOR) %>%
  mutate(proportion = n / sum(n)) %>%
  dplyr::select(-n) %>%
  spread(AUTOR, proportion) %>%
  gather(AUTOR, proportion, `BenjaminKangLim`:`BradDorfman`)

frequency.2 <- todos_tf %>%
  count(AUTOR, word) %>%
  group_by(AUTOR) %>%
  mutate(proportion = n / sum(n)) %>%
  dplyr::select(-n) %>%
  spread(AUTOR, proportion) %>%
  gather(AUTOR, proportion, `AlexanderSmith`:`BenjaminKangLim`)

frequency.3 <- todos_tf %>%
  count(AUTOR, word) %>%
  group_by(AUTOR) %>%
  mutate(proportion = n / sum(n)) %>%
  dplyr::select(-n) %>%
  spread(AUTOR, proportion) %>%
  gather(AUTOR, proportion, `DavidLawder`,`BenjaminKangLim`)

gg.f1<-ggplot(frequency.1, aes(x = proportion, y = `AlexanderSmith`, color = abs(`AlexanderSmith` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.000001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~AUTOR, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "AlexanderSmith", x = NULL)

gg.f2<-ggplot(frequency.2, aes(x = proportion, y = `DavidLawder`, color = abs(`DavidLawder` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.000001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~AUTOR, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "DavidLawder", x = NULL)

gg.f3<-ggplot(frequency.3, aes(x = proportion, y = `BradDorfman`, color = abs(`BradDorfman` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.000001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~AUTOR, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "BradDorfman", x = NULL)
grid.arrange(grobs=list(gg.f1),nrow=1,ncol=1)
grid.arrange(grobs=list(gg.f2),nrow=1,ncol=1)
grid.arrange(grobs=list(gg.f3),nrow=1,ncol=1)


# ANALISIS CORRESPONDENCIAS
autores<-unique(articulos$AUTOR)
for (i in 1:length(autores))
{
  articulos.aux<-articulos[which(articulos$AUTOR==autores[i]),]$ARTICULO
  articulo<-""
  for(j in 1:length(articulos.aux))
    articulo<-paste(articulo,articulos.aux[j],sep=" ")
  if(i==1)
    articulos.agregados<-data.frame(ARTICULO=articulo,AUTOR=autores[i])
  else
    articulos.agregados<-rbind(articulos.agregados,data.frame(ARTICULO=articulo,AUTOR=autores[i]))
}
mi.corpus.agregado<-SimpleCorpus(VectorSource(articulos.agregados$ARTICULO))
mi.dtm.agregado<-DocumentTermMatrix(mi.corpus.agregado,control=list(language="english", tolower=TRUE, removeNumbers=TRUE, stripWhitespace=TRUE, removePunctuation = TRUE, 
                                                  stopwords = TRUE, stemming=TRUE))
mi.dtm.agregado$dimnames$Docs<-articulos.agregados$AUTOR
matriz.dtm.agregado<-numerica(mi.dtm.agregado)
res_CA_AUX<-CA(matriz.dtm.agregado,graph=F)

mi.dtm.agregado

res_CA_AUX$eig
res_CA_AUX$row$contrib
res_CA_AUX$row$cos2
df.row.contrib<-data.frame("AUTOR"=rep(rownames(res_CA_AUX$row$contrib),3),
                           "VALOR"=c(res_CA_AUX$row$contrib[,1],res_CA_AUX$row$contrib[,2],res_CA_AUX$row$contrib[,3]),
                           "DIM"=c(rep("D1",4),rep("D2",4),rep("D3",4)))
gg.row.contrib<-ggplot(data=df.row.contrib, 
                   aes(x=DIM, y=VALOR, fill=AUTOR)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="right") +
  labs(title="", x="", y="")


gg.row.contrib
df.row.cos2<-data.frame("AUTOR"=rep(rownames(res_CA_AUX$row$cos2),3),
                           "VALOR"=c(res_CA_AUX$row$cos2[,1],res_CA_AUX$row$cos2[,2],res_CA_AUX$row$cos2[,3]),
                           "DIM"=c(rep("D1",4),rep("D2",4),rep("D3",4)))

gg.row.cos2<-ggplot(data=df.row.cos2, 
                       aes(x=DIM, y=VALOR, fill=AUTOR)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none") +
  labs(title="", x="", y="")
gg.row.cos2

datos.filas<-data.frame(res_CA_AUX$row$coord,names(res_CA_AUX$call$marge.row))
colnames(datos.filas)<-c("D1","D2","D3","AUTOR")

with(datos.filas, {
  s3d <- scatterplot3d(D1, D2, D3,        # x y and z axis
                       color="blue", pch=19,        # filled blue circles
                       type="h",                    # vertical lines to the x-y plane
                       main="3-D Autores",
                       xlab="Eje 1",
                       ylab="Eje 2",
                       zlab="Eje 3")
  s3d.coords <- s3d$xyz.convert(D1, D2, D3) # convert 3D coords to 2D projection
  text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
       labels=AUTOR,               # text to plot
       cex=.5, pos=4)           # shrink text 50% and place to right of points)
})


CA_plot_FILAS.1_2<-plot.CA(res_CA_AUX,axes=c(1,2),invisible="col",label="row",title="Eje 1 vs Eje 2",cex=.5)
CA_plot_FILAS.2_3<-plot.CA(res_CA_AUX,axes=c(2,3),invisible="col",label="row",title="Eje 2 vs Eje 3",cex=.5)
grid.arrange(grobs=list(CA_plot_FILAS.1_2,CA_plot_FILAS.2_3),nrow=1,ncol=2)

datos.columnas<-data.frame(res_CA_AUX$col$coord,names(res_CA_AUX$call$marge.col))
colnames(datos.columnas)<-c("D1","D2","D3","Palabra")
with(datos.columnas, {
  s3d <- scatterplot3d(D1, D2, D3,        # x y and z axis
                       color="blue", pch=19,  # filled blue circles
                       cex.symbols=.1,
                       type="h",                    # vertical lines to the x-y plane
                       main="3-D Palabras",
                       xlab="Eje 1",
                       ylab="Eje 2",
                       zlab="Eje 3")
  s3d.coords <- s3d$xyz.convert(D1, D2, D3) # convert 3D coords to 2D projection
  text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
       labels=Palabra,               # text to plot
       cex=.25, pos=4)           # shrink text 25% and place to right of points)
})

col.contrib<-res_CA_AUX$col$contrib[which(res_CA_AUX$col$contrib[,1]>=.25 | res_CA_AUX$col$contrib[,2]>=.25 | res_CA_AUX$col$contrib[,3]>=.25),]
names(col.contrib[,1])
n.pal<-length(col.contrib[,1])

names(col.contrib[order(col.contrib[,1],decreasing=TRUE),1])[1:10]
names(col.contrib[order(col.contrib[,2],decreasing=TRUE),1])[1:10]
names(col.contrib[order(col.contrib[,3],decreasing=TRUE),1])[1:10]

terminos_tf_idf.100<-todos_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(AUTOR) %>%
  slice_head(n=n.pal)
for (i in 1:length(autores))
{
  palabras.tf_idf<-data.frame(PALABRA=terminos_tf_idf.100[which(terminos_tf_idf.100$AUTOR==autores[i]),][order(terminos_tf_idf.100$tf_idf,decreasing=TRUE),][1:n.pal,]$word,
                              AUTOR=autores[i])
  palabras.frecuentes<-data.frame(PALABRA=terminos_f(tidy[which(tidy$AUTOR==autores[i]),],n.pal)$TODOS,
                                  AUTOR=autores[i])
  for (j in 1:3)
  {
    if (i==1 && j==1)
    {
      palabras<-data.frame(PALABRA=names(res_CA_AUX$col$contrib[which(res_CA_AUX$col$contrib[,j]>=.25),j]),
                           CONTRIBUCION=res_CA_AUX$col$contrib[which(res_CA_AUX$col$contrib[,j]>=.25),j],
                           DIMENSION=j,
                           AUTOR=autores[i])
      
      frecuentes<-inner_join(palabras[which(palabras$AUTOR==autores[i] & palabras$DIMENSION==j),],
                             palabras.frecuentes[which(palabras.frecuentes$AUTOR==autores[i]),],
                             by=c("AUTOR","PALABRA"))
      tfIdf<-inner_join(palabras[which(palabras$AUTOR==autores[i] & palabras$DIMENSION==j),],
                        palabras.tf_idf[which(palabras.tf_idf$AUTOR==autores[i]),],
                        by=c("AUTOR","PALABRA"))
    }
    else
    {
      palabras<-rbind(palabras,data.frame(PALABRA=names(res_CA_AUX$col$contrib[which(res_CA_AUX$col$contrib[,j]>=.25),j]),
                                          CONTRIBUCION=res_CA_AUX$col$contrib[which(res_CA_AUX$col$contrib[,j]>=.25),j],
                                          DIMENSION=j,
                                          AUTOR=autores[i]))
      frecuentes<-rbind(frecuentes,inner_join(palabras[which(palabras$AUTOR==autores[i] & palabras$DIMENSION==j),],
                                              palabras.frecuentes[which(palabras.frecuentes$AUTOR==autores[i]),],
                                              by=c("AUTOR","PALABRA")))
      tfIdf<-rbind(tfIdf,inner_join(palabras[which(palabras$AUTOR==autores[i] & palabras$DIMENSION==j),],
                                    palabras.tf_idf[which(palabras.tf_idf$AUTOR==autores[i]),],
                                    by=c("AUTOR","PALABRA")))
    }
  }
}
unidos<-full_join(frecuentes,tfIdf,
                  by=c("AUTOR","DIMENSION","PALABRA"))
unidos<-cbind(unidos,
              data.frame(CONTRIBUCION=ifelse(is.na(unidos$CONTRIBUCION.x),
                                             unidos$CONTRIBUCION.y,
                                             ifelse(is.na(unidos$CONTRIBUCION.y),
                                                    unidos$CONTRIBUCION.x,
                                                    ifelse(unidos$CONTRIBUCION.x==unidos$CONTRIBUCION.y,
                                                           unidos$CONTRIBUCION.x,
                                                           max(unidos$CONTRIBUCION.x,unidos$CONTRIBUCION.y))))))

unidos<-unidos[,-c(2,5)]

# table(frecuentes$AUTOR,frecuentes$DIMENSION)
# cat("El ",round(100*sum(table(frecuentes$AUTOR,frecuentes$DIMENSION))/n.pal,2), " % (",sum(table(frecuentes$AUTOR,frecuentes$DIMENSION))," palabras) de las que mas aportan a la construcción de ","\n","los ejes se encuentra entre las 151 palabras mas frecuentes de alguno de ","\n","los cuatro autores")
# table(tfIdf$AUTOR,tfIdf$DIMENSION)
# cat("El ",round(100*sum(table(tfIdf$AUTOR,tfIdf$DIMENSION))/n.pal,2), " % (",sum(table(tfIdf$AUTOR,tfIdf$DIMENSION))," palabras) de las que mas aportan a la construcción de ","\n","los ejes se encuentra entre las 151 palabras de mayores valores tfIdf de ","\n","alguno de los cuatro autores")
table(unidos$AUTOR,unidos$DIMENSION)
# cat("El ",round(100*sum(table(unidos$AUTOR,unidos$DIMENSION))/n.pal,2), " % (",sum(table(unidos$AUTOR,unidos$DIMENSION))," palabras) de las que mas aportan a la construcción de ","\n","los ejes se encuentra entre las 151 palabras de mayor frecuencia o de mmayores ","\n","valores tfIdf de alguno de los cuatro autores")

df.cols<-inner_join(unidos,data.frame(PALABRA=names(col.contrib[,1]),col.contrib),by="PALABRA")
unicos<-unique(df.cols[order(df.cols$CONTRIBUCION,decreasing=TRUE),c("PALABRA","DIMENSION","CONTRIBUCION")])
unicos$PALABRA
unicos[1:3,]

dim.columnas=inner_join(data.frame(PALABRA=names(res_CA_AUX$col$coord[,1]),D1=res_CA_AUX$col$coord[,1],
                                   D2=res_CA_AUX$col$coord[,2],D3=res_CA_AUX$col$coord[,3]),unidos,by="PALABRA")

# gg.col1<-ggplot(data=dim.columnas[which(dim.columnas$AUTOR %in% c("BenjaminKangLim","AlexanderSmith","DavidLawder")),],
#                 aes(D1,D2))+
#   geom_encircle(aes(group=AUTOR,fill=AUTOR),alpha=0.4)+
#   geom_text(label=dim.columnas[which(dim.columnas$AUTOR %in% c("BenjaminKangLim","AlexanderSmith","DavidLawder")),]$PALABRA,
#                   size=1.5,color="blue")+
#   geom_point(data=datos.filas[which(datos.filas$AUTOR %in% c("BenjaminKangLim","AlexanderSmith","DavidLawder")),],aes(D1,D2),color="red")+
#   ggplot2::annotate(geom="text",x=datos.filas[c("BenjaminKangLim","AlexanderSmith","DavidLawder"),]$D1,
#        y=datos.filas[c("BenjaminKangLim","AlexanderSmith","DavidLawder"),]$D2,
#        label=datos.filas[c("BenjaminKangLim","AlexanderSmith","DavidLawder"),]$AUTOR,
#        color="RED",cex=2.5)+theme_bw()+
#   theme(legend.position = "top",legend.title = element_text(size = 6),
#         legend.text = element_text(size = 6))
# 
# gg.col2<-ggplot(data=dim.columnas[which(dim.columnas$AUTOR %in% c("BenjaminKangLim","BradDorfman","DavidLawder")),],aes(D1,D3))+
#   geom_encircle(aes(group=AUTOR,fill=AUTOR),alpha=0.4)+
#   geom_text(label=dim.columnas[which(dim.columnas$AUTOR %in% c("BenjaminKangLim","BradDorfman","DavidLawder")),]$PALABRA,
#             size=1.5,color="blue")+
#   geom_point(data=datos.filas[which(datos.filas$AUTOR %in% c("BenjaminKangLim","BradDorfman","DavidLawder")),],aes(D1,D3),color="red")+
#   ggplot2::annotate(geom="text",x=datos.filas[c("BenjaminKangLim","BradDorfman","DavidLawder"),]$D1,
#                     y=datos.filas[c("BenjaminKangLim","BradDorfman","DavidLawder"),]$D3,
#                     label=datos.filas[c("BenjaminKangLim","BradDorfman","DavidLawder"),]$AUTOR,
#                     color="RED",cex=2.5)+theme_bw()+
#   theme(legend.position = "top",legend.title = element_text(size = 6),
#         legend.text = element_text(size = 6))
# 
# grid.arrange(grobs=list(gg.col1,gg.col2),nrow=1,ncol=2)

with(dim.columnas, {
  s3d <- scatterplot3d(D1, D2, D3,        # x y and z axis
                       color="blue", pch=19,  # filled blue circles
                       cex.symbols=.1,
                       type="h",                    # vertical lines to the x-y plane
                       main="3-D Palabras",
                       xlab="Eje 1",
                       ylab="Eje 2",
                       zlab="Eje 3")
  s3d.coords <- s3d$xyz.convert(D1, D2, D3) # convert 3D coords to 2D projection
  text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
       labels=PALABRA,               # text to plot
       cex=.5, pos=4)           # shrink text 25% and place to right of points)
})

# gg.col1<-ggplot(data=dim.columnas,
#                 aes(D1,D2))+
#   geom_text(label=dim.columnas$PALABRA,
#             size=2.5,color="blue")

gg.col1<-ggplot() + geom_point(data = dim.columnas, mapping = aes(x = D1, y = D2), color = "red", alpha = 0.5) + 
  geom_text_repel(data =dim.columnas, color="blue", size=2.0, 
                  mapping = aes(x=D1, y=D2 , label = PALABRA),
                  max.overlaps = getOption("ggrepel.max.overlaps", default = 100))+
  theme_bw()

# gg.col2<-ggplot(data=dim.columnas,
#                 aes(D2,D3))+
#   geom_text(label=dim.columnas$PALABRA,
#             size=2.5,color="blue")

gg.col2<-ggplot() + geom_point(data = dim.columnas, mapping = aes(x = D2, y = D3), color = "red", alpha = 0.5) + 
  geom_text_repel(data =dim.columnas, color="blue", size=2.0, 
                  mapping = aes(x=D2, y=D3 , label = PALABRA),
                  max.overlaps = getOption("ggrepel.max.overlaps", default = 100))+
  theme_bw()

grid.arrange(grobs=list(gg.col1,gg.col2),nrow=1,ncol=2)

# FIN ANALISIS CORRESPONDENCIAS

# ESCALADO MULTIDIMENSIONAL

matriz.tdm.agregada<-t(matriz.dtm.agregado)
dist.cos<-1-cosine(matriz.tdm.agregada)
round(dist.cos,6)

v.propios<-cmdscale(dist.cos,eig=TRUE)
v.propios<-100*cumsum(v.propios$eig*(v.propios$eig>0))/sum(v.propios$eig*(v.propios$eig>0)) 
round(v.propios,3)

res.mds<-smacofSym(dist.cos)
res.mds.3d<-smacofSym(dist.cos,ndim=3)

datos.autores.mds<-data.frame("MET_D1"=res.mds$conf[,1],"MET_D2"=res.mds$conf[,2])
datos.autores.mds.3d<-data.frame("MET_D1"=res.mds.3d$conf[,1],
                                 "MET_D2"=res.mds.3d$conf[,2],
                                 "MET_D3"=res.mds.3d$conf[,3])

res.mds.no.met<-smacofSym(dist.cos,type="ordinal")
res.mds.no.met.3d<-smacofSym(dist.cos,type="ordinal",ndim=3)

datos.autores.mds.no.met<-data.frame("NO.MET_D1"=res.mds.no.met$conf[,1],"NO.MET_D2"=res.mds.no.met$conf[,2])
datos.autores.mds.no.met.3d<-data.frame("NO.MET_D1"=res.mds.no.met.3d$conf[,1],
                                        "NO.MET_D2"=res.mds.no.met.3d$conf[,2],
                                        "NO.MET_D3"=res.mds.no.met.3d$conf[,3])


par(mfrow=c(1,2))
with(datos.autores.mds.3d, {
  s3d <- scatterplot3d(MET_D1, MET_D2, MET_D3,        # x y and z axis
                       color="blue", pch=19,        # filled blue circles
                       type="h",                    # vertical lines to the x-y plane
                       main="3-D Métrico",
                       xlab="Eje 1",
                       ylab="Eje 2",
                       zlab="Eje 3")
  s3d.coords <- s3d$xyz.convert(MET_D1, MET_D2, MET_D3) # convert 3D coords to 2D projection
  text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
       labels=rownames(datos.autores.mds.3d),               # text to plot
       cex=.5, pos=4)           # shrink text 50% and place to right of points)
})
plot(res.mds.3d,plot.type="resplot",main="Residuos")
# plot(res.mds.3d,plot.type="Shepard",main="Shepard (MDS Métrico)")
par(mfrow=c(1,1))
# datos.autores.mds.3d
df.rtados.mds<-data.frame("TIPO"=c("MET","NO.MET"),
                          "BONDAD"=c(1-res.mds$stress,1-res.mds.no.met$stress),
                          "ITERACIONES"=c(res.mds$niter,res.mds.no.met$niter))

cat("S-Stress (MDS Métrico):",res.mds$stress,sep=" ")
cat("S-Stress (MDS No Métrico):",res.mds.no.met$stress,sep=" ")
df.rtados.mds

cbind(datos.autores.mds,datos.autores.mds.no.met)
# par(mfrow=c(1,2))
# plot(res.mds,main="Métrico",col="red",label.conf=list(pos=5,col="blue"),xlim=c(-2,1),ylim=c(-1,1))
# plot(res.mds.no.met,main="No métrico",label.conf=list(pos=5,col="blue"),xlim=c(-2,1),ylim=c(-1,1))
# par(mfrow=c(1,1))
# cbind(datos.autores.mds,datos.autores.mds.no.met)
gg.aut.mds<-ggplot(datos.autores.mds,aes(MET_D1,MET_D2))+
  geom_text(label=rownames(datos.autores.mds),size=1.5,color="blue")+
  theme_bw()
gg.aut.mds.no.met<-ggplot(datos.autores.mds.no.met,aes(NO.MET_D1,NO.MET_D2))+
  geom_text(label=rownames(datos.autores.mds.no.met),size=1.5,color="blue")+
  theme_bw()

grid.arrange(grobs=list(gg.aut.mds,gg.aut.mds.no.met),nrow=1,ncol=2)

cat("Residuos")
par(mfrow=c(1,2))
plot(res.mds,plot.type="resplot",main="Métrico")
plot(res.mds.no.met,plot.type="resplot",main="No métrico")
par(mfrow=c(1,1))

df.spp<-data.frame(rbind(sort(res.mds$spp,decreasing=TRUE),sort(res.mds.no.met$spp,decreasing=TRUE)))
rownames(df.spp)<-c("Métrico","No métrico")           
cat("Contribución al stress:")
round(df.spp,5)
df.spp.g<-data.frame("AUTOR"=rep(names(res.mds$spp),2),
                       "VALOR"=c(res.mds$spp,res.mds.no.met$spp),
                       "MODO"=c(rep("Métrico",4),rep("No Métrico",4)))

gg.df.spp.g<-ggplot(data=df.spp.g, 
                   aes(x=AUTOR, y=VALOR, fill=MODO)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") +
  labs(title="", x="", y="")
gg.df.spp.g


df.spp<-data.frame(rbind(sort(res.mds$spp,decreasing=TRUE),sort(res.mds.no.met$spp,decreasing=TRUE)))
rownames(df.spp)<-c("Métrico","No métrico")           
round(df.spp,5)

matriz.dtm.col.sel<-matriz.dtm.agregado[,names(col.contrib[,1])]
dist.cos.col<-1-cosine(matriz.dtm.col.sel)
v.propios.col<-cmdscale(dist.cos.col,eig=TRUE)
v.propios.col<-100*cumsum(v.propios.col$eig*(v.propios.col$eig>0))/sum(v.propios.col$eig*(v.propios.col$eig>0)) 
headTail(round(v.propios.col,2))

res.mds.col.3d.met<-smacofSym(dist.cos.col,ndim=3)
res.mds.col.3d.no.met<-smacofSym(dist.cos.col,type="ordinal",ndim=3)
datos.palabras.met.3d<-data.frame("MET_D1"=res.mds.col.3d.met$conf[,1],
                                 "MET_D2"=res.mds.col.3d.met$conf[,2],
                                 "MET_D3"=res.mds.col.3d.met$conf[,3])
datos.palabras.no.met.3d<-data.frame("MET_D1"=res.mds.col.3d.no.met$conf[,1],
                                  "MET_D2"=res.mds.col.3d.no.met$conf[,2],
                                  "MET_D3"=res.mds.col.3d.no.met$conf[,3])
res.mds.col.3d.met
res.mds.col.3d.no.met
df.rtados.col.3d<-data.frame("TIPO"=c("MET","NO.MET"),
                              "BONDAD"=c(1-res.mds.col.3d.met$stress,1-res.mds.col.3d.no.met$stress),
                              "ITERACIONES"=c(res.mds.col.3d.met$niter,res.mds.col.3d.no.met$niter))

cat("S-Stress (MDS Métrico):",res.mds.col.3d.met$stress,sep=" ")
cat("S-Stress (MDS No Métrico):",res.mds.col.3d.no.met$stress,sep=" ")
df.rtados.col.3d

# cat("Contribución al stress:")
# df.spp.col.3d<-data.frame(cbind(head(df.spp.col.met.3d,25),head(rownames(df.spp.col.no.met.3d),25),
#                                 head(df.spp.col.no.met.3d,25)))
# colnames(df.spp.col.3d)<-c("% (Met)","Pal","% (No met)")
# df.spp.col.3d

par(mfrow=c(1,2))
with(datos.palabras.met.3d, {
  s3d <- scatterplot3d(MET_D1, MET_D2, MET_D3,        # x y and z axis
                       color="blue", pch=19,        # filled blue circles
                       type="h",                    # vertical lines to the x-y plane
                       main="3-D Met",
                       xlab="Eje 1",
                       ylab="Eje 2",
                       zlab="Eje 3")
  s3d.coords <- s3d$xyz.convert(MET_D1, MET_D2, MET_D3) # convert 3D coords to 2D projection
  text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
       labels=rownames(datos.palabras.met.3d),               # text to plot
       cex=.5, pos=4)           # shrink text 50% and place to right of points)
})
with(datos.palabras.no.met.3d, {
  s3d <- scatterplot3d(MET_D1, MET_D2, MET_D3,        # x y and z axis
                       color="blue", pch=19,        # filled blue circles
                       type="h",                    # vertical lines to the x-y plane
                       main="3-D No Met",
                       xlab="Eje 1",
                       ylab="Eje 2",
                       zlab="Eje 3")
  s3d.coords <- s3d$xyz.convert(MET_D1, MET_D2, MET_D3) # convert 3D coords to 2D projection
  text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
       labels=rownames(datos.palabras.no.met.3d),               # text to plot
       cex=.5, pos=4)           # shrink text 50% and place to right of points)
})

plot(res.mds.col.3d.met,plot.type="resplot",main="Métrico")
plot(res.mds.col.3d.no.met,plot.type="resplot",main="No métrico")
par(mfrow=c(1,1))

res.mds.col<-smacofSym(dist.cos.col)

res.mds.col.no.met<-smacofSym(dist.cos.col,type="ordinal")

# round(sort(res.mds.col$spp,decreasing=TRUE),4)
# round(sort(res.mds.col.no.met$spp,decreasing=TRUE),4)

df.rtados.mds.col<-data.frame("TIPO"=c("MET","NO.MET"),
                              "BONDAD"=c(1-res.mds.col$stress,1-res.mds.col.no.met$stress),
                              "ITERACIONES"=c(res.mds.col$niter,res.mds.col.no.met$niter))

df.rtados.mds.col

datos.palabras.mds<-data.frame("MET_D1"=res.mds.col$conf[,1],"MET_D2"=res.mds.col$conf[,2])
dim.columnas.mds=inner_join(data.frame(PALABRA=rownames(datos.palabras.mds),MET_D1=datos.palabras.mds[,1],
                                       MET_D2=datos.palabras.mds[,2]),unidos,by="PALABRA")

datos.palabras.mds.no.met<-data.frame("NO.MET_D1"=res.mds.col.no.met$conf[,1],"NO.MET_D2"=res.mds.col.no.met$conf[,2])
dim.columnas.mds.no.met=inner_join(data.frame(PALABRA=rownames(datos.palabras.mds.no.met),NO.MET_D1=datos.palabras.mds.no.met[,1],
                                              NO.MET_D2=datos.palabras.mds.no.met[,2]),unidos,by="PALABRA")

df.palabras.unidos<-inner_join(dim.columnas.mds,dim.columnas.mds.no.met[,c("PALABRA","NO.MET_D1","NO.MET_D2","DIMENSION","AUTOR")],by=c("PALABRA","DIMENSION","AUTOR"))
df.palabras.unidos.unicos<-unique(df.palabras.unidos[,c("PALABRA","MET_D1","MET_D2","NO.MET_D1","NO.MET_D2")])

# gg.pal.mds<-ggplot(df.palabras.unidos.unicos,aes(MET_D1,MET_D2))+
#   geom_text(label=df.palabras.unidos.unicos$PALABRA,size=1.5,color="blue")+
#   theme_bw()
gg.pal.mds<-ggplot() + geom_point(data = df.palabras.unidos.unicos, 
                      mapping = aes(x = MET_D1, y = MET_D2), color = "red", alpha = 0.5) + 
  geom_text_repel(data =df.palabras.unidos.unicos, size=2, color="blue",
                  mapping = aes(x=MET_D1, y=MET_D2 , label = PALABRA),
                  max.overlaps = getOption("ggrepel.max.overlaps", default = 100))+
  theme_bw()
# gg.pal.mds.no.met<-ggplot(df.palabras.unidos.unicos,aes(NO.MET_D1,NO.MET_D2))+
#   geom_text(label=df.palabras.unidos.unicos$PALABRA,size=1.5,color="blue")+
#   theme_bw()
gg.pal.mds.no.met<-ggplot() + geom_point(data = df.palabras.unidos.unicos, 
                                  mapping = aes(x = NO.MET_D1, y = NO.MET_D2), color = "red", alpha = 0.5) + 
  geom_text_repel(data =df.palabras.unidos.unicos, size=2, color="blue",
                  mapping = aes(x=NO.MET_D1, y=NO.MET_D2 , label = PALABRA),
                  max.overlaps = getOption("ggrepel.max.overlaps", default = 100))+
  theme_bw()
grid.arrange(grobs=list(gg.pal.mds,gg.pal.mds.no.met),nrow=1,ncol=2)


# FIN  ESCALADO MULTIDIMENSIONAL

# UNFOLDING

# unique(dim.columnas$PALABRA)
# colnames(matriz.dtm.col.sel)
# names(col.contrib[order(col.contrib[,1],decreasing=TRUE),1])[1:10]
# names(col.contrib[order(col.contrib[,2],decreasing=TRUE),1])[1:10]
# names(col.contrib[order(col.contrib[,3],decreasing=TRUE),1])[1:10]

# matriz.unfolding<-prop.table(matriz.dtm.agregado,margin=1)[,colnames(matriz.dtm.col.sel)]
t(matriz.dtm.agregado[,1:10])
matriz.unfolding<-prop.table(matriz.dtm.agregado,margin=1)[,unique(dim.columnas$PALABRA)]

matriz.unfolding<-replace(matriz.unfolding,matriz.unfolding==0,0.000001)
matriz.unfolding<-1/matriz.unfolding
t(round(matriz.unfolding[,1:10],0))
# matriz.unfolding<-1/matriz.unfolding

res.unfolding.met.3d<-unfolding(matriz.unfolding,ndim=3)
# res.unfolding.met.3d

datos.autores.unfolding.met.3d<-data.frame("MET_D1"=res.unfolding.met.3d$conf.row[,1],
                                           "MET_D2"=res.unfolding.met.3d$conf.row[,2],
                                           "MET_D3"=res.unfolding.met.3d$conf.row[,3],
                                           "CLASE"="AUTOR")
datos.palabras.unfolding.met.3d<-data.frame("MET_D1"=res.unfolding.met.3d$conf.col[,1],
                                            "MET_D2"=res.unfolding.met.3d$conf.col[,2],
                                            "MET_D3"=res.unfolding.met.3d$conf.col[,3],
                                            "CLASE"="PALABRA")

# datos.autores.unfolding.no.met.3d<-data.frame("MET_D1"=res.unfolding.no.met.3d$conf.row[,1],
#                                            "MET_D2"=res.unfolding.no.met.3d$conf.row[,2],
#                                            "MET_D3"=res.unfolding.no.met.3d$conf.row[,3],
#                                            "CLASE"="AUTOR")
# datos.palabras.unfolding.no.met.3d<-data.frame("MET_D1"=res.unfolding.no.met.3d$conf.col[,1],
#                                             "MET_D2"=res.unfolding.no.met.3d$conf.col[,2],
#                                             "MET_D3"=res.unfolding.no.met.3d$conf.col[,3],
#                                             "CLASE"="PALABRA")

res.unfolding.met<-unfolding(matriz.unfolding,ndim=2)
# res.unfolding.no.met<-unfolding(matriz.unfolding,type="ordinal",ndim=2)
# res.unfolding.met

datos.autores.unfolding.met<-data.frame("MET_D1"=res.unfolding.met$conf.row[,1],
                                           "MET_D2"=res.unfolding.met$conf.row[,2],
                                           "CLASE"="AUTOR")
datos.palabras.unfolding.met<-data.frame("MET_D1"=res.unfolding.met$conf.col[,1],
                                            "MET_D2"=res.unfolding.met$conf.col[,2],
                                            "CLASE"="PALABRA")

df.rtados.unfolding<-data.frame("TIPO"=c("3D","2D"),
                                "BONDAD"=c(1-res.unfolding.met.3d$stress,1-res.unfolding.met$stress),
                                "ITERACIONES"=c(res.unfolding.met.3d$niter,res.unfolding.met$niter))

df.rtados.unfolding
# datos.autores.unfolding.no.met<-data.frame("MET_D1"=res.unfolding.no.met$conf.row[,1],
#                                         "MET_D2"=res.unfolding.no.met$conf.row[,2],
#                                         "CLASE"="AUTOR")
# datos.palabras.unfolding.no.met<-data.frame("MET_D1"=res.unfolding.no.met$conf.col[,1],
#                                          "MET_D2"=res.unfolding.no.met$conf.col[,2],
#                                          "CLASE"="PALABRA")
df.spp.row<-data.frame("AUTOR"=rep(names(res.unfolding.met.3d$spp.row),2),
                       "VALOR"=c(res.unfolding.met.3d$spp.row,res.unfolding.met$spp.row),
                       "MODO"=c(rep("3D",4),rep("2D",4)))
length(names(res.unfolding.met.3d$spp.col))
df.spp.col<-data.frame("AUTOR"=rep(names(res.unfolding.met.3d$spp.col),2),
                          "VALOR"=c(res.unfolding.met.3d$spp.col,res.unfolding.met$spp.col),
                          "MODO"=c(rep("3D",99),rep("2D",99)))

gg.spp.row<-ggplot(data=df.spp.row, 
        aes(x=AUTOR, y=VALOR, fill=MODO)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") +
    labs(title="", x="", y="")

gg.spp.col<-ggplot(data=df.spp.col, 
       aes(x=AUTOR, y=VALOR, fill=MODO)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none") +
  labs(title="", x="", y="")

gg.spp.row
gg.spp.col

# df.spp.row
# sort(res.unfolding.met.3d$spp.row ,decreasing=TRUE)
# sort(res.unfolding.met$spp.row ,decreasing=TRUE)
# names(res.unfolding.met.3d$spp.row)
# names(res.unfolding.met$spp.row)
# names(res.unfolding.met.3d$spp.col)
# 
# df.spp.col<-data.frame("AUTOR"=rep(names(res.unfolding.met.3d$spp.col),2),
#                        "VALOR"=c(res.unfolding.met.3d$spp.col,res.unfolding.met$spp.col),
#                        "MODO"=c(rep("MET",35),rep("NO.MET",35)))
# 
# par(mfrow=c(1,2))
# for (i in 1:5)
#   (ggplot(data=df.spp.col[c(((i-1)*7)+1:((i-1)*7)+7,((i-1)*7)+36:((i-1)*7)+42),], 
#           aes(x=AUTOR, y=VALOR, fill=MODO)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(title="% Stress por palabra", x="", y=""))
# par(mfrow=c(1,1))
# 
# plot(res.unfolding.met, plot.type = "confplot", what = "columns")
# 
# 
# 
# 
# gg.spp.col.2<-ggplot(data=df.spp.col[c(8:14,43:49),], aes(x=AUTOR, y=VALOR, fill=MODO)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(title="% Stress por autor", x="", y="")
# gg.spp.col.3<-ggplot(data=df.spp.col[c(16:21,50:56),], aes(x=AUTOR, y=VALOR, fill=MODO)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(title="% Stress por autor", x="", y="")
# gg.spp.col.4<-ggplot(data=df.spp.col[c(22:28,57:63),], aes(x=AUTOR, y=VALOR, fill=MODO)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(title="% Stress por autor", x="", y="")
# gg.spp.col.5<-ggplot(data=df.spp.col[c(29:35,64:70),], aes(x=AUTOR, y=VALOR, fill=MODO)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(title="% Stress por autor", x="", y="")
# 
# grid.arrange(grobs=list(gg.spp.col.1,gg.spp.col.2,gg.spp.col.3,
#                         gg.spp.col.4,gg.spp.col.5),nrow=2,ncol=3)
# 
# 
# ggplot(data=df.spp.row, aes(x=AUTOR, y=VALOR, fill=MODO)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(title="% Stress por autor", x="", y="")
#   
# 
# sort(res.unfolding.no.met.3d$spp.col ,decreasing=TRUE)
# sort(res.unfolding.no.met$spp.col ,decreasing=TRUE)




datos.unfolding.met.3d<-rbind(datos.autores.unfolding.met.3d,datos.palabras.unfolding.met.3d)
colnames(datos.unfolding.met.3d)<-c("D1","D2","D3","CLASE")
# datos.unfolding.no.met.3d<-rbind(datos.autores.unfolding.no.met.3d,datos.palabras.unfolding.no.met.3d)
# colnames(datos.unfolding.no.met.3d)<-c("D1","D2","D3","CLASE")
datos.unfolding.met<-rbind(datos.autores.unfolding.met,datos.palabras.unfolding.met)
colnames(datos.unfolding.met)<-c("D1","D2","CLASE")
# datos.unfolding.no.met<-rbind(datos.autores.unfolding.no.met,datos.palabras.unfolding.no.met)
# colnames(datos.unfolding.no.met)<-c("D1","D2","CLASE")


colores<-c("red","blue")
colores <- colores[as.numeric(factor(datos.unfolding.met$CLASE))]
formas<-c(19,18)
formas <- formas[as.numeric(factor(datos.unfolding.met$CLASE))]
tams<-c(.75,.5)
tams <- tams[as.numeric(factor(datos.unfolding.met$CLASE))]
# rownames(datos.unfolding.met)
ggplot() + geom_point(data = datos.unfolding.met, mapping = aes(x = D1, y = D2), 
                      color = colores, alpha = 0.5) + 
  geom_text_repel(data =datos.unfolding.met, color=colores, size=2.0, 
                  mapping = aes(x=D1, y=D2 , label = rownames(datos.unfolding.met)),
                  max.overlaps = getOption("ggrepel.max.overlaps", default = 50)) +
  theme_bw()
colores<-c("red","blue")
colores <- colores[as.numeric(factor(datos.unfolding.met.3d$CLASE))]
formas<-c(19,18)
formas <- formas[as.numeric(factor(datos.unfolding.met.3d$CLASE))]
tams<-c(.75,.5)
tams <- tams[as.numeric(factor(datos.unfolding.met.3d$CLASE))]
with(datos.unfolding.met.3d, {
  s3d <- scatterplot3d(D1, D2, D3,        # x y and z axis
                       color=colores, pch=formas,
                       # color="blue", pch=19,        # filled blue circles
                       type="h",                    # vertical lines to the x-y plane
                       main="",
                       xlab="D1",
                       ylab="D2",
                       zlab="D3")
  s3d.coords <- s3d$xyz.convert(D1, D2, D3) # convert 3D coords to 2D projection
  text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
       labels=rownames(datos.unfolding.met.3d),               # text to plot
       cex=tams, pos=4, col=colores)           # shrink text 50% and place to right of points)
})
# s3d <- scatterplot3d(datos.unfolding.met.3d$D1, 
#                      datos.unfolding.met.3d$D2, 
#                      datos.unfolding.met.3d$D3,        # x y and z axis
#                        color=colores, pch=formas,
#                        # color="blue", pch=19,        # filled blue circles
#                        type="h",                    # vertical lines to the x-y plane
#                        main="UNFOLDING 3-D (Métrico)",
#                        xlab="Eje 1",
#                        ylab="Eje 2",
#                        zlab="Eje 3")
# s3d.coords <- s3d$xyz.convert(datos.unfolding.met.3d$D1, 
#                                 datos.unfolding.met.3d$D2, 
#                                 datos.unfolding.met.3d$D3) # convert 3D coords to 2D projection
# text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
#        labels=rownames(datos.unfolding.met.3d),               # text to plot
#        cex=tams, pos=4, col=colores)           # shrink text 50% and place to right of points)


# gg.unfolding.no.met<-ggplot() + geom_point(data = datos.unfolding.no.met, mapping = aes(x = D1, y = D2), 
#                                         color = colores, alpha = 0.5) + 
#   geom_text_repel(data =datos.unfolding.no.met, color=colores, size=2.0, 
#                   mapping = aes(x=D1, y=D2 , label = rownames(datos.unfolding.no.met)),
#                   max.overlaps = getOption("ggrepel.max.overlaps", default = 50))+
#   theme_bw()
# 
# grid.arrange(grobs=list(gg.unfolding.met,gg.unfolding.no.met),nrow=1,ncol=2)
# colores<-c("red","blue")
# colores <- colores[as.numeric(factor(datos.unfolding.no.met.3d$CLASE))]
# formas<-c(19,18)
# formas <- formas[as.numeric(factor(datos.unfolding.no.met.3d$CLASE))]
# tams<-c(.75,.5)
# tams <- tams[as.numeric(factor(datos.unfolding.no.met.3d$CLASE))]
# with(datos.unfolding.no.met.3d, {
#   s3d <- scatterplot3d(D1, D2, D3,        # x y and z axis
#                        color=colores, pch=formas,
#                        # color="blue", pch=19,        # filled blue circles
#                        type="h",                    # vertical lines to the x-y plane
#                        main="UNFOLDING 3-D (No Métrico)",
#                        xlab="Eje 1",
#                        ylab="Eje 2",
#                        zlab="Eje 3")
#   s3d.coords <- s3d$xyz.convert(D1, D2, D3) # convert 3D coords to 2D projection
#   text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
#        labels=rownames(datos.unfolding.met.3d),               # text to plot
#        cex=tams, pos=4, col=colores)           # shrink text 50% and place to right of points)
# })

# 
# 
# 
# 
# par(mfrow=c(1,2))
# gg.unfolding.met
# gg.unfolding.no.met
# par(mfrow=c(1,2))
# 
# gg.unfolding.1.2<-ggplot(datos.unfolding,aes(MET_D1,MET_D2))+
#   geom_text(label=rownames(datos.unfolding),size=1.5,color="blue")+
#   theme_bw()
# gg.unfolding.1.2<-ggplot(datos.unfolding,aes(MET_D1,MET_D2))+
#   geom_text(label=rownames(datos.unfolding),size=1.5,color="blue")+
#   theme_bw()
# gg.unfolding.no.met<-ggplot(datos.unfolding.no.met,aes(MET_D1,MET_D2))+
#   geom_text(label=rownames(datos.unfolding.no.met),size=1.5,color="blue")+
#   theme_bw()
# 
# 
# plot(res.unfolding.met,what="both",
#      plot.dim = c(1,2), col.rows = hcl(0), col.columns = hcl(240), 
#      label.conf.rows = list(label = TRUE, pos = 3,col = hcl(0, l = 50), cex = 0.8),
#      label.conf.columns = list(label = TRUE, pos = 3,col = hcl(240, l = 50), cex = 0.4),
#      type = "p", pch = 20,
#      cex = 0.5, asp = 1, main="Unfolding 2D", xlab="Coord. 1", ylab="Coord. 2")
# 
# 
# 
# 
# # colnames(datos.autores.unfolding)<-c("D1","D2","D3")
# # colnames(datos.palabras.unfolding)<-c("D1","D2","D3")
# # s3d <- scatterplot3d(datos.autores.unfolding$D1, datos.autores.unfolding$D2, datos.autores.unfolding$D3,        # x y and z axis
# #                      color="red", pch=19,        # filled blue circles
# #                      type="h",                    # vertical lines to the x-y plane
# #                      main="3-D Autores",
# #                      xlab="Eje 1",
# #                      ylab="Eje 2",
# #                      zlab="Eje 3")
# # 
# # s3d.coords <- s3d$xyz.convert(datos.autores.unfolding$D1, datos.autores.unfolding$D2, datos.autores.unfolding$D3) # convert 3D coords to 2D projection
# # text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
# #      labels=rownames(datos.autores.unfolding),               # text to plot
# #      cex=1, pos=4,col="red")           # shrink text 50% and place to right of points)
# # s3d.coords.2 <- s3d$xyz.convert(datos.palabras.unfolding$D1, datos.palabras.unfolding$D2, datos.palabras.unfolding$D3) # convert 3D coords to 2D projection
# # text(s3d.coords.2$x, s3d.coords.2$y,             # x and y coordinates
# #      labels=rownames(datos.palabras.unfolding),               # text to plot
# #      cex=.5, pos=4, col="blue")           # shrink text 50% and place to right of points)
# 
# datos.unfolding.no.met<-rbind(datos.autores.unfolding.no.met,datos.palabras.unfolding.no.met)
# colnames(datos.unfolding.no.met)<-c("D1","D2","D3","CLASE")
# with(datos.unfolding.no.met, {
#   s3d <- scatterplot3d(D1, D2, D3,        # x y and z axis
#                        color=colores, pch=formas,
#                        # color="blue", pch=19,        # filled blue circles
#                        type="h",                    # vertical lines to the x-y plane
#                        main="UNFOLDING 3-D (No métrico)",
#                        xlab="Eje 1",
#                        ylab="Eje 2",
#                        zlab="Eje 3")
#   s3d.coords <- s3d$xyz.convert(D1, D2, D3) # convert 3D coords to 2D projection
#   text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
#        labels=rownames(datos.unfolding),               # text to plot
#        cex=tams, pos=4, col=colores)           # shrink text 50% and place to right of points)
# })
# 
# plot(res.unfolding.met,plot.type="stressplot",what="row")
# plot(res.mds ,plot.type="stressplot")
# plot(res.mds.col ,plot.type="stressplot")
# 
# res.unfolding.met$congvec
# gg.unfolding.1.2<-ggplot(datos.unfolding,aes(MET_D1,MET_D2))+
#   geom_text(label=rownames(datos.unfolding),size=1.5,color="blue")+
#   theme_bw()
# gg.unfolding.1.2<-ggplot(datos.unfolding,aes(MET_D1,MET_D2))+
#   geom_text(label=rownames(datos.unfolding),size=1.5,color="blue")+
#   theme_bw()
# gg.unfolding.no.met<-ggplot(datos.unfolding.no.met,aes(MET_D1,MET_D2))+
#   geom_text(label=rownames(datos.unfolding.no.met),size=1.5,color="blue")+
#   theme_bw()
# 
# grid.arrange(grobs=list(gg.unfolding.1.2,gg.unfolding.no.met),nrow=1,ncol=2)

# FIN UNFOLDING

source("R/libreriasYfunciones.r")

# ANÁLISIS DISCRIMINANTE
data.frame("NOMBRE"=c("AaronPressman","AlanCrosby","AlexanderSmith","BenjaminKangLim","BernardHickey",
                      "BradDorfman","DarrenSchuettler","DavidLawder","EdnaFernandes","EricAuchard"),
           "ABREVIATURA"=c("AP","AC","AS","BKL","BH","BD","DS","DL","EF","EA"))

articulos<-carga_datos.t.100()
save.image("capitulo04_TFM.RData")
save.image("capitulo04_TFM_50.RData")

# articulos<-carga_datos.todo()
split<-sample.split(articulos$AUTOR, SplitRatio = 0.50)
training.art <- subset(articulos, split == TRUE)
test.art <- subset(articulos, split == FALSE)

folds <- createFolds(training.art$AUTOR, k = 10)

cvLDA <- lapply(folds, function(x){
  
  mi.corpus.training<-SimpleCorpus(VectorSource(training.art[-x,]$ARTICULO))
  mi.dtm.training<-DocumentTermMatrix(mi.corpus.training,
                                      control=list(language="english", tolower=TRUE, removeNumbers=TRUE, stripWhitespace=TRUE, removePunctuation = TRUE, 
                                                   stopwords = TRUE, stemming=TRUE))
  mi.dtm.training$dimnames$Docs<-training.art[-x,]$FICHERO

  mi.corpus.test<-SimpleCorpus(VectorSource(training.art[x,]$ARTICULO))
  mi.dtm.test<-DocumentTermMatrix(mi.corpus.test,
                                  control=list(language="english", tolower=TRUE, removeNumbers=TRUE, stripWhitespace=TRUE, removePunctuation = TRUE, 
                                               stopwords = TRUE, stemming=TRUE))
  mi.dtm.test$dimnames$Docs<-training.art[x,]$FICHERO
  
  matriz.rel.filas.training<-prop.table(numerica(mi.dtm.training),margin=1)
  matriz.rel.filas.test<-prop.table(numerica(mi.dtm.test),margin=1)

  test.matriz<-numerica(mi.dtm.test)
  columnas.test<-colnames(test.matriz)[which(colnames(test.matriz) %in% colnames(as.data.frame(matriz.rel.filas.training)))]
  tr.df.lda.bruto<-cbind(as.data.frame(matriz.rel.filas.training[,columnas.test]),training.art[-x,]$AUTOR)
  colnames(tr.df.lda.bruto)[ncol(tr.df.lda.bruto)]<-"AUTOR"
  
  tr.df.test.bruto<-cbind(as.data.frame(matriz.rel.filas.test[,columnas.test]),training.art[x,]$AUTOR)
  colnames(tr.df.test.bruto)[ncol(tr.df.test.bruto)]<-"AUTOR"

  training_fold <- tr.df.lda.bruto
  test_fold <- tr.df.test.bruto
  clasificador<-MASS::lda(x=training_fold[,-length(training_fold[1,])],grouping=training_fold$AUTOR)
  y_pred <- predict(clasificador, test_fold[,-length(test_fold[1,])])
  # confusion<-table(test_fold$AUTOR,y_pred$class)
  confusion<-data.frame("REAL"=test_fold$AUTOR,"PRED"=y_pred$class)
  return(confusion)
})

for (i in 1:10)
{
  if (i==1)
    df.cvLDA<-cvLDA[[i]]
  else
    df.cvLDA<-rbind(df.cvLDA,cvLDA[[i]])
}
ari.lda.bruto<-adjustedRandIndex(df.cvLDA$REAL,df.cvLDA$PRED)
precision.lda.bruto<-evalua_confusion_global(table(df.cvLDA$REAL,df.cvLDA$PRED))
confusion.lda.bruto<-table(df.cvLDA$REAL,df.cvLDA$PRED)

paste("Precision: ",precision.lda.bruto,sep="")
paste("Adjusted Rand Index: ",ari.lda.bruto,sep="")
rtados.autores.lda.bruto<-round(as.data.frame(evalua_x_autor(confusion.lda.bruto)),2)
colnames(rtados.autores.lda.bruto)<-c("Precis","Recup")
t(rtados.autores.lda.bruto)

df.rtados.global<-data.frame("MODELO"="FrecRel","PRECIS"=precision.lda.bruto,"ARI"=ari.lda.bruto)
df.precis.autores<-data.frame("FrecRel"=rtados.autores.lda.bruto[,1],row.names=rownames(rtados.autores.lda.bruto))
df.recup.autores<-data.frame("FrecRel"=rtados.autores.lda.bruto[,2],
                              row.names=rownames(rtados.autores.lda.bruto))

mi.corpus.mds<-SimpleCorpus(VectorSource(articulos$ARTICULO))
mi.dtm.mds<-DocumentTermMatrix(mi.corpus.mds,
                                    control=list(language="english", tolower=TRUE, removeNumbers=TRUE, stripWhitespace=TRUE, removePunctuation = TRUE, 
                                                 stopwords = TRUE, stemming=TRUE))
mi.dtm.mds$dimnames$Docs<-articulos$FICHERO
mi.dtm.mds

matriz.tdm.frec<-t(numerica(mi.dtm.mds))

dist.cos.tdm.frec<-1-cosine(matriz.tdm.frec)

v.propios.tdm.frec<-cmdscale(dist.cos.tdm.frec,eig=TRUE)

v.propios.tdm.frec<-100*cumsum(v.propios.tdm.frec$eig*(v.propios.tdm.frec$eig>0))/sum(v.propios.tdm.frec$eig*(v.propios.tdm.frec$eig>0))

v.propios.tdm.frec[351:400]
n<-length(v.propios.tdm.frec[which(v.propios.tdm.frec<99)])+1
cat("Rtados para dims ",(n-4)," a ",(n+5)," : \n",sep="")
round(v.propios.tdm.frec[(n-4):(n+5)],3)

mds.met<-smacofSym(dist.cos.tdm.frec,ndim=n) # conserva > 99 %
mds.met

config.mds.met<-cbind(as.data.frame(mds.met$conf),articulos$AUTOR)
colnames(config.mds.met)[ncol(config.mds.met)]<-"AUTOR"

split<-sample.split(config.mds.met$AUTOR, SplitRatio = 0.90)
met.training.art <- subset(config.mds.met, split == TRUE)
met.test.art <- subset(config.mds.met, split == FALSE)

met.folds <- createFolds(met.training.art$AUTOR, k = 10)
cv.metLDA <- lapply(met.folds, function(x){
  
  training_fold <- met.training.art[-x,]
  test_fold <- met.training.art[x,]
  clasificador<-MASS::lda(x=training_fold[,-length(training_fold[1,])],grouping=training_fold$AUTOR)
  y_pred <- predict(clasificador, test_fold[,-length(test_fold[1,])])
  # confusion<-table(test_fold$AUTOR,y_pred$class)
  confusion<-data.frame("REAL"=test_fold$AUTOR,"PRED"=y_pred$class)
  return(confusion)
})

for (i in 1:10)
{
  if (i==1)
    df.cv.metLDA<-cv.metLDA[[i]]
  else
    df.cv.metLDA<-rbind(df.cv.metLDA,cv.metLDA[[i]])
}

ari.met.lda<-adjustedRandIndex(df.cv.metLDA$REAL,df.cv.metLDA$PRED)
precision.met.lda<-evalua_confusion_global(table(df.cv.metLDA$REAL,df.cv.metLDA$PRED))
confusion.met.lda<-table(df.cv.metLDA$REAL,df.cv.metLDA$PRED)

paste("Precision: ",precision.met.lda,sep="")
paste("Adjusted Rand Index: ",ari.met.lda,sep="")
rtados.autores.met.lda<-round(as.data.frame(evalua_x_autor(confusion.met.lda)),2)
colnames(rtados.autores.met.lda)<-c("Precis","Recup")
t(rtados.autores.met.lda)

df.rtados.global<-rbind(df.rtados.global,data.frame("MODELO"="MDS.Met","PRECIS"=precision.met.lda,"ARI"=ari.met.lda))
df.precis.autores<-cbind(df.precis.autores,data.frame("MDS.Met"=rtados.autores.met.lda[,1],row.names=rownames(rtados.autores.met.lda)))
df.recup.autores<-cbind(df.recup.autores,data.frame("MDS.Met"=rtados.autores.met.lda[,2],
                                                    row.names=rownames(rtados.autores.met.lda)))

mds.no.met<-smacofSym(dist.cos.tdm.frec,type="ordinal",ndim=385) # conserva > 99 %
mds.no.met

config.mds.no.met<-cbind(as.data.frame(mds.no.met$conf),articulos$AUTOR)
colnames(config.mds.no.met)[ncol(config.mds.no.met)]<-"AUTOR"

split<-sample.split(config.mds.no.met$AUTOR, SplitRatio = 0.90)
no.met.training.art <- subset(config.mds.no.met, split == TRUE)
no.met.test.art <- subset(config.mds.no.met, split == FALSE)

no.met.folds <- createFolds(no.met.training.art$AUTOR, k = 10)
cv.no.metLDA <- lapply(no.met.folds, function(x){
  
  training_fold <- no.met.training.art[-x,]
  test_fold <- no.met.training.art[x,]
  clasificador<-MASS::lda(x=training_fold[,-length(training_fold[1,])],grouping=training_fold$AUTOR)
  y_pred <- predict(clasificador, test_fold[,-length(test_fold[1,])])
  # confusion<-table(test_fold$AUTOR,y_pred$class)
  confusion<-data.frame("REAL"=test_fold$AUTOR,"PRED"=y_pred$class)
  return(confusion)
})

for (i in 1:10)
{
  if (i==1)
    df.cv.no.metLDA<-cv.no.metLDA[[i]]
  else
    df.cv.no.metLDA<-rbind(df.cv.no.metLDA,cv.no.metLDA[[i]])
}
ari.no.met.lda<-adjustedRandIndex(df.cv.no.metLDA$REAL,df.cv.no.metLDA$PRED)
precision.no.met.lda<-evalua_confusion_global(table(df.cv.no.metLDA$REAL,df.cv.no.metLDA$PRED))
confusion.no.met.lda<-table(df.cv.no.metLDA$REAL,df.cv.no.metLDA$PRED)

paste("Precision: ",precision.no.met.lda,sep="")
paste("Adjusted Rand Index: ",ari.no.met.lda,sep="")
rtados.autores.no.met.lda<-round(as.data.frame(evalua_x_autor(confusion.no.met.lda)),2)
colnames(rtados.autores.no.met.lda)<-c("Precis","Recup")
t(rtados.autores.no.met.lda)


df.rtados.global<-rbind(df.rtados.global,data.frame("MODELO"="MDS.NoMet","PRECIS"=precision.no.met.lda,"ARI"=ari.no.met.lda))
df.precis.autores<-cbind(df.precis.autores,data.frame("MDS.NoMet"=rtados.autores.no.met.lda[,1],row.names=rownames(rtados.autores.no.met.lda)))
df.recup.autores<-cbind(df.recup.autores,data.frame("MDS.NoMet"=rtados.autores.no.met.lda[,2],
                                                    row.names=rownames(rtados.autores.no.met.lda)))

df.graf.precis<-data.frame("AUTOR"=row.names(df.precis.autores),"METODO"=rep("FrecRel",10),"VALOR"=df.precis.autores[,1])

df.graf.precis<-rbind(df.graf.precis,data.frame("AUTOR"=row.names(df.precis.autores),
                                                "METODO"=rep("MDS.Met",10),
                                                "VALOR"=df.precis.autores[,2]))
df.graf.precis<-rbind(df.graf.precis,data.frame("AUTOR"=row.names(df.precis.autores),
                                                "METODO"=rep("MDS.NoMet",10),
                                                "VALOR"=df.precis.autores[,3]))

gg.precis.autores.wrap<-ggplot(df.graf.precis,xlab="") + 
  geom_col(aes(x = AUTOR, y=VALOR, fill = AUTOR), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_wrap(facets = vars(METODO),nrow=1,ncol=3)

df.precis.autores
gg.precis.autores.wrap

df.graf.recup<-data.frame("AUTOR"=row.names(df.recup.autores),"METODO"=rep("FrecRel",10),"VALOR"=df.recup.autores[,1])

df.graf.recup<-rbind(df.graf.recup,data.frame("AUTOR"=row.names(df.recup.autores),
                                              "METODO"=rep("MDS.Met",10),
                                              "VALOR"=df.recup.autores[,2]))
df.graf.recup<-rbind(df.graf.recup,data.frame("AUTOR"=row.names(df.recup.autores),
                                              "METODO"=rep("MDS.NoMet",10),
                                              "VALOR"=df.recup.autores[,3]))

gg.recup.autores.wrap<-ggplot(df.graf.recup,xlab="") + 
  geom_col(aes(x = AUTOR, y=VALOR, fill = AUTOR), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_wrap(facets = vars(METODO),nrow=1,ncol=3)

df.recup.autores
gg.recup.autores.wrap

df.graf.global<-rbind(data.frame("METODO"=df.rtados.global$MODELO,"MEDIDA"=rep("Precision",3),"VALOR"=df.rtados.global[,2]),
                      data.frame("METODO"=df.rtados.global$MODELO,"MEDIDA"=rep("ARI",3),"VALOR"=df.rtados.global[,3]))

gg.global.grid<-ggplot(df.graf.global,xlab="") + 
  geom_col(aes(x = METODO, y=VALOR, fill = METODO), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_grid(cols = vars(MEDIDA))

df.rtados.global
gg.global.grid

save.image("capitulo04_TFM.RData")

# CONTAMOS CON TODOS LOS INDIVIDUOS A CLASIFICAR EN EL MOMENTO INICIAL

clasificador.mds.met<-MASS::lda(x=met.training.art[,-ncol(met.training.art)],
                                grouping=met.training.art$AUTOR)
y_pred.mds.met <- predict(clasificador.mds.met, met.test.art[,-ncol(met.test.art)])

confusion.mds.met<-data.frame("REAL"=met.test.art$AUTOR,"PRED"=y_pred.mds.met$class)

ari.mds.met<-adjustedRandIndex(confusion.mds.met$REAL,confusion.mds.met$PRED)
precision.mds.met<-evalua_confusion_global(table(confusion.mds.met$REAL,confusion.mds.met$PRED))

paste("Precision: ",precision.mds.met,sep="")
paste("Adjusted Rand Index: ",ari.mds.met,sep="")
rtados.autores.mds.met<-round(as.data.frame(evalua_x_autor(table(confusion.mds.met$REAL,confusion.mds.met$PRED))),2)
colnames(rtados.autores.mds.met)<-c("Precis","Recup")
rtados.autores.mds.met

clasificador.mds.no.met<-MASS::lda(x=no.met.training.art[,-ncol(no.met.training.art)],
                                   grouping=no.met.training.art$AUTOR)
y_pred.mds.no.met <- predict(clasificador.mds.no.met, no.met.test.art[,-ncol(no.met.test.art)])
confusion.mds.no.met<-data.frame("REAL"=no.met.test.art$AUTOR,"PRED"=y_pred.mds.no.met$class)

ari.mds.no.met<-adjustedRandIndex(confusion.mds.no.met$REAL,confusion.mds.no.met$PRED)
precision.mds.no.met<-evalua_confusion_global(table(confusion.mds.no.met$REAL,confusion.mds.no.met$PRED))

paste("Precision: ",precision.mds.no.met,sep="")
paste("Adjusted Rand Index: ",ari.mds.no.met,sep="")
rtados.autores.mds.no.met<-round(as.data.frame(evalua_x_autor(table(confusion.mds.no.met$REAL,confusion.mds.no.met$PRED))),2)
colnames(rtados.autores.mds.no.met)<-c("Precis","Recup")
rtados.autores.mds.no.met

t(data.frame("MDS Met"=rtados.autores.mds.met[,1],"MDS No met"=rtados.autores.mds.no.met[,1],row.names=rownames(rtados.autores.mds.met)))

df.global<-rbind(data.frame("METODO"=c("Mét","No mét"),"MEDIDA"=rep("Precision",2),"VALOR"=c(precision.mds.met,precision.mds.no.met)),
                 data.frame("METODO"=c("Mét","No mét"),"MEDIDA"=rep("ARI",2),"VALOR"=c(ari.mds.met,ari.mds.no.met)))

gg.global<-ggplot(df.global,xlab="") + 
  geom_col(aes(x = METODO, y=VALOR, fill = METODO), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_grid(cols = vars(MEDIDA))

gg.global

data.frame("PRECISION"=c(round(precision.mds.met,4),round(precision.mds.no.met,4)),
           "ARI"=c(round(ari.mds.met,4),round(ari.mds.no.met,4)),row.names=c("MDS Met","MDS No met"))
table(confusion.mds.met$REAL,confusion.mds.met$PRED)
table(confusion.mds.no.met$REAL,confusion.mds.no.met$PRED)
sum(diag(table(confusion.mds.no.met$REAL,confusion.mds.no.met$PRED)))
gg.global

df.precis<-rbind(data.frame("AUTOR"=row.names(rtados.autores.mds.met),"METODO"=rep("Met",10),"VALOR"=rtados.autores.mds.met[,1]),
                 data.frame("AUTOR"=row.names(rtados.autores.mds.no.met),"METODO"=rep("No Met",10),"VALOR"=rtados.autores.mds.no.met[,1]))

gg.precis.wrap<-ggplot(df.precis,xlab="") + 
  geom_col(aes(x = AUTOR, y=VALOR, fill = AUTOR), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_wrap(facets = vars(METODO),nrow=1,ncol=3)

gg.precis.wrap

df.recup<-rbind(data.frame("AUTOR"=row.names(rtados.autores.mds.met),"METODO"=rep("Met",10),"VALOR"=rtados.autores.mds.met[,2]),
                 data.frame("AUTOR"=row.names(rtados.autores.mds.no.met),"METODO"=rep("No Met",10),"VALOR"=rtados.autores.mds.no.met[,2]))

gg.recup.wrap<-ggplot(df.recup,xlab="") + 
  geom_col(aes(x = AUTOR, y=VALOR, fill = AUTOR), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_wrap(facets = vars(METODO),nrow=1,ncol=3)

gg.recup.autores.wrap

save.image("capitulo04_TFM.RData")

# LA ALTERNATIVA (LOS INDIVIDUOS A CLASIFICAR IRÁN CONOCIÉNDOSE EN MOMENTOS POSTERIORES)

setwd("~/Google Drive/TFM/Plantilla")
source("R/libreriasYfunciones.r")
load("capitulo04_TFM.RData")

mi.corpus.mds.training<-SimpleCorpus(VectorSource(training.art$ARTICULO))
mi.dtm.mds.training<-DocumentTermMatrix(mi.corpus.mds.training,
                                      control=list(language="english", tolower=TRUE, removeNumbers=TRUE, stripWhitespace=TRUE, removePunctuation = TRUE, 
                                                   stopwords = TRUE, stemming=TRUE))
mi.dtm.mds.training$dimnames$Docs<-training.art$FICHERO
mi.dtm.mds.training  

matriz.rel.filas.mds.training<-prop.table(numerica(mi.dtm.mds.training),margin=1)

matriz.mds.met<-t(matriz.rel.filas.mds.training)

dist.cos.mds.met<-1-cosine(matriz.mds.met)

v.propios.mds.met<-cmdscale(dist.cos.mds.met,eig=TRUE)

v.propios.mds.met<-100*cumsum(v.propios.mds.met$eig*(v.propios.mds.met$eig>0))/sum(v.propios.mds.met$eig*(v.propios.mds.met$eig>0))
n.2<-length(v.propios.mds.met[which(v.propios.mds.met<99)])+1
cat("Rtados para dims ",(n.2-4)," a ",(n.2+5)," :\n",sep="")
round(v.propios.mds.met[(n.2-4):(n.2+5)],3)

mi.corpus.mds.test<-SimpleCorpus(VectorSource(test.art$ARTICULO))
mi.dtm.mds.test<-DocumentTermMatrix(mi.corpus.mds.test,
                                    control=list(language="english", tolower=TRUE, removeNumbers=TRUE, stripWhitespace=TRUE, removePunctuation = TRUE, 
                                                 stopwords = TRUE, stemming=TRUE))
mi.dtm.mds.test$dimnames$Docs<-test.art$FICHERO
mi.dtm.mds.test

matriz.rel.filas.mds.test<-prop.table(numerica(mi.dtm.mds.test),margin=1)

save.image("capitulo04_TFM-1.RData")

fila.aux<-matriz.rel.filas.mds.training[1,
                                        !colnames(-matriz.rel.filas.mds.training) %in% 
                                          colnames(matriz.rel.filas.mds.test)]
filas.aux<-matrix(0,nrow=nrow(matriz.rel.filas.mds.test),ncol=length(fila.aux),
                  dimnames=list(rownames(matriz.rel.filas.mds.test),names(fila.aux)))

matriz.aux<-cbind(matriz.rel.filas.mds.test,filas.aux)

matriz.test<-matriz.aux[,colnames(matriz.rel.filas.mds.training)]
matriz.mds.test<-t(matriz.test)

fila.cos<-matrix(0,nrow=ncol(matriz.mds.met),ncol=ncol(matriz.mds.test),
                 dimnames=list(colnames(matriz.mds.met),colnames(matriz.mds.test)))

for (i in 1:ncol(matriz.mds.met))
{
  for (j in 1:ncol(matriz.mds.test))
  {
    fila.cos[i,j]<-1-cosine(matriz.mds.test[,j],matriz.mds.met[,i])
  }
}

res.mds.met.final<-smacofSym(dist.cos.mds.met,ndim=n.2)
config.mds.met.final<-cbind(as.data.frame(res.mds.met.final$conf),"AUTOR"=training.art$AUTOR)
res.mds.no.met.final<-smacofSym(dist.cos.mds.met,type="ordinal",ndim=n.2)
config.mds.no.met.final<-cbind(as.data.frame(res.mds.no.met.final$conf),"AUTOR"=training.art$AUTOR)

save.image("capitulo04_TFM-2.RData")

# calculo de coordenadas de nuevos puntos
X<-res.mds.met.final$conf # resultado de MDS sobre los datos de entrenamiento (900 documentos)
D<-dist.cos.mds.met # matriz de disimilaridades a partir de la que se ha obtenido X
save(X,D,fila.cos,n,n.2,training.art,articulos,file="Gower.RData")
D_i. = matrix(rep(apply(D, 2, sum)/nrow(D), ncol(D)), nrow = nrow(D)) 
D_.j = matrix(rep(apply(D, 1, sum)/nrow(D), nrow(D)), nrow = nrow(D),byrow = T)
D_.. = matrix(rep(sum(D)/(ncol(D)^2), nrow(D) * ncol(D)), nrow = nrow(D)) 
B = as.matrix(-(1/2) * (D - D_i. - D_.j + D_..))
propios<-eigen(B)
lambda<-propios$values

# fila.cos tiene las distancias de los documentos de test a los documentos de entrenamiento
for (j in 1:ncol(fila.cos))
{
  nueva.conf<-c(rep(0,n.2))
  for (i in 1:n)
    nueva.conf[i]<-(1/2)*(1/lambda[i])*t(X[,i])%*%fila.cos[,j] # ¿ fórmula de Gower para obtener las nuevas configuraciones ?
  if(j==1)
    test.conf<-data.frame(t(nueva.conf))
  else
    test.conf<-rbind(test.conf,data.frame(t(nueva.conf)))
}
colnames(test.conf)<-colnames(X)
rownames(test.conf)<-colnames(fila.cos)

config.mds.met.final<-cbind(as.data.frame(X),"AUTOR"=training.art$AUTOR)
clasificador.met.final<-MASS::lda(x=config.mds.met.final[,-length(config.mds.met.final[1,])],grouping=config.mds.met.final$AUTOR)

for (i in 1:nrow(test.conf))
{
  y_pred <- predict(clasificador.met.final, test.conf[i,])
  if(i==1)
    confusion.test.final<-data.frame("REAL"=articulos[which(articulos$FICHERO==rownames(test.conf)[i]),]$AUTOR,"PRED"=y_pred$class)
  else
    confusion.test.final<-rbind(confusion.test.final,data.frame("REAL"=articulos[which(articulos$FICHERO==rownames(test.conf)[i]),]$AUTOR,"PRED"=y_pred$class))
}

confusion.final<-table(confusion.test.final$REAL,confusion.test.final$PRED)
ari.final<-adjustedRandIndex(confusion.test.final$REAL,confusion.test.final$PRED)
precision.final<-evalua_confusion_global(table(confusion.test.final$REAL,confusion.test.final$PRED))

paste("Precision: ",precision.final,sep="")
paste("Adjusted Rand Index: ",ari.final,sep="")
rtados.autores.final<-round(as.data.frame(evalua_x_autor(confusion.final)),2)
colnames(rtados.autores.final)<-c("Precis","Recup")
rtados.autores.final

save.image("capitulo04_TFM-3.RData")

# nueva.conf<-(1/2)*(1/lambda[1])*t(X[,1])%*%d
# length(nueva.conf)
# length(t(X[1,]))
# length(d)
# length(nueva.conf)
# 1/(2*lambda[1])
# nueva.conf
# X[,1]*d
# t(d)

# FIN: calculo de coordenadas de nuevos puntos

# recálculo de distancias, MDS y LDA para cada nuevo elemento
for (i in 1:ncol(fila.cos))
{
  nuevas.dist<-rbind(cbind(dist.cos.mds.met,fila.cos[,i]),t(c(fila.cos[,i],0)))
  v.propios.aux<-cmdscale(nuevas.dist,eig=TRUE)
  v.propios.aux<-100*cumsum(v.propios.aux$eig*(v.propios.aux$eig>0))/sum(v.propios.aux$eig*(v.propios.aux$eig>0))
  n.aux<-length(v.propios.aux[which(v.propios.aux<99)])+1

  res.mds.met.test<-smacofSym(nuevas.dist,ndim=n.aux) # conserva > 99 %
  config.mds.met.final<-data.frame(res.mds.met.test$conf[-nrow(res.mds.met.test$conf),],"AUTOR"=training.art$AUTOR)
  clasificador.met.test<-MASS::lda(x=config.mds.met.final[,-length(config.mds.met.final[1,])],grouping=config.mds.met.final$AUTOR)

  config.mds.met.test<-res.mds.met.test$conf[nrow(nuevas.dist),]
  y_pred.met<- predict(clasificador.met.test, config.mds.met.test)

  res.mds.no.met.test<-smacofSym(nuevas.dist,type="ordinal",ndim=n.aux) # conserva > 99 %
  config.mds.no.met.final<-data.frame(res.mds.no.met.test$conf[-nrow(res.mds.met.test$conf),],"AUTOR"=training.art$AUTOR)
  clasificador.no.met.test<-MASS::lda(x=config.mds.no.met.final[,-length(config.mds.no.met.final[1,])],grouping=config.mds.no.met.final$AUTOR)

  config.mds.no.met.test<-res.mds.no.met.test$conf[nrow(nuevas.dist),]
  y_pred.no.met<- predict(clasificador.no.met.test, config.mds.no.met.test)
  if(i==1)
  {
    confusion.test.met<-data.frame("REAL"=articulos[which(articulos$FICHERO==colnames(fila.cos)[i]),]$AUTOR,"PRED"=y_pred.met$class)
    confusion.test.no.met<-data.frame("REAL"=articulos[which(articulos$FICHERO==colnames(fila.cos)[i]),]$AUTOR,"PRED"=y_pred.no.met$class)
  }else{
    confusion.test.met<-rbind(confusion.test,data.frame("REAL"=articulos[which(articulos$FICHERO==colnames(fila.cos)[i]),]$AUTOR,"PRED"=y_pred.met$class))
    confusion.test.no.met<-rbind(confusion.test,data.frame("REAL"=articulos[which(articulos$FICHERO==colnames(fila.cos)[i]),]$AUTOR,"PRED"=y_pred.no.met$class))
  }
}

confusion.test.met.2<-table(confusion.test.met$REAL,confusion.test.met$PRED)
ari.test.met<-adjustedRandIndex(confusion.test$REAL,confusion.test.met$PRED)
precision.test.met<-evalua_confusion_global(table(confusion.test.met$REAL,confusion.test.met$PRED))

paste("Precision: ",precision.test.met,sep="")
paste("Adjusted Rand Index: ",ari.test.met,sep="")
rtados.autores.test.met<-round(as.data.frame(evalua_x_autor(confusion.test.met.2)),2)
colnames(rtados.autores.test.met)<-c("Precis","Recup")
rtados.autores.test.met

confusion.test.no.met.2<-table(confusion.test.no.met$REAL,confusion.test.no.met$PRED)
ari.test.no.met<-adjustedRandIndex(confusion.test$REAL,confusion.test.no.met$PRED)
precision.test.no.met<-evalua_confusion_global(table(confusion.test.no.met$REAL,confusion.test.no.met$PRED))

paste("Precision: ",precision.test.no.met,sep="")
paste("Adjusted Rand Index: ",ari.test.no.met,sep="")
rtados.autores.test.no.met<-round(as.data.frame(evalua_x_autor(confusion.test.no.met.2)),2)
colnames(rtados.autores.test.no.met)<-c("Precis","Recup")
rtados.autores.test.no.met

t(data.frame("MDS Met"=rtados.autores.test.met[,1],"MDS No met"=rtados.autores.test.no.met[,1],
             row.names=rownames(rtados.autores.test.met)))

df.global.test<-rbind(data.frame("METODO"=c("Mét","No mét"),"MEDIDA"=rep("Precision",2),
                            "VALOR"=c(precision.test.met,precision.test.no.met)),
                 data.frame("METODO"=c("Mét","No mét"),"MEDIDA"=rep("ARI",2),"VALOR"=c(ari.test.met,ari.test.no.met)))

gg.global.test<-ggplot(df.global.test,xlab="") + 
  geom_col(aes(x = METODO, y=VALOR, fill = METODO), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_grid(cols = vars(MEDIDA))

gg.global.test

df.precis.test<-rbind(data.frame("AUTOR"=row.names(rtados.autores.test.met),"METODO"=rep("Met",10),
                                 "VALOR"=rtados.autores.test.met[,1]),
                 data.frame("AUTOR"=row.names(rtados.autores.test.no.met),"METODO"=rep("No Met",10),
                            "VALOR"=rtados.autores.test.no.met[,1]))

gg.precis.wrap.test<-ggplot(df.precis.test,xlab="") + 
  geom_col(aes(x = AUTOR, y=VALOR, fill = AUTOR), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_wrap(facets = vars(METODO),nrow=1,ncol=3)

gg.precis.wrap.test

df.recup.test<-rbind(data.frame("AUTOR"=row.names(rtados.autores.test.met),"METODO"=rep("Met",10),
                                "VALOR"=rtados.autores.test.met[,2]),
                data.frame("AUTOR"=row.names(rtados.autores.test.no.met),"METODO"=rep("No Met",10),
                           "VALOR"=rtados.autores.test.no.met[,2]))

gg.recup.wrap.test<-ggplot(df.recup.test,xlab="") + 
  geom_col(aes(x = AUTOR, y=VALOR, fill = AUTOR), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_wrap(facets = vars(METODO),nrow=1,ncol=3)

gg.recup.autores.wrap.test


save.image("capitulo04_TFM-4.RData")

# v.propios.mds.met<-cmdscale(dist.cos.mds.met,eig=TRUE)
# 
# v.propios.mds.met<-100*cumsum(v.propios.mds.met$eig*(v.propios.mds.met$eig>0))/sum(v.propios.mds.met$eig*(v.propios.mds.met$eig>0))
# # v.propios.mds.met[201:250]
# cat("Rtados para dims 211 a 220 : \n",round(v.propios.mds.met[211:220],3))
# 
# save.image("capitulo04_TFM-2.RData")
# 
# mds.met<-smacofSym(dist.cos.mds.met,ndim=215) # conserva > 99 %
# mds.met
# 
# config.mds.met<-cbind(as.data.frame(mds.met$conf),"AUTOR"=training.art$AUTOR)
# 
# clasificador.met<-MASS::lda(x=config.mds.met[,-length(config.mds.met[1,])],grouping=config.mds.met$AUTOR)
# 
# mi.corpus.mds.test<-SimpleCorpus(VectorSource(test.art$ARTICULO))
# mi.dtm.mds.test<-DocumentTermMatrix(mi.corpus.mds.test,
#                                     control=list(language="english", tolower=TRUE, removeNumbers=TRUE, stripWhitespace=TRUE, removePunctuation = TRUE, 
#                                                  stopwords = TRUE, stemming=TRUE))
# mi.dtm.mds.test$dimnames$Docs<-test.art$FICHERO
# mi.dtm.mds.test
# 
# matriz.rel.filas.mds.test<-prop.table(numerica(mi.dtm.mds.test),margin=1)
# 
# fila.aux<-matriz.rel.filas.mds.training[1,
#                                         !colnames(-matriz.rel.filas.mds.training) %in% 
#                                           colnames(matriz.rel.filas.mds.test)]
# filas.aux<-matrix(0,nrow=nrow(matriz.rel.filas.mds.test),ncol=length(fila.aux),
#                     dimnames=list(rownames(matriz.rel.filas.mds.test),names(fila.aux)))
# 
# matriz.aux<-cbind(matriz.rel.filas.mds.test,filas.aux)
# 
# matriz.test<-matriz.aux[,colnames(matriz.rel.filas.mds.training)]
# matriz.mds.test<-t(matriz.test)
# 
# fila.cos<-matrix(0,nrow=ncol(matriz.mds.met),ncol=ncol(matriz.mds.test),
#                  dimnames=list(colnames(matriz.mds.met),colnames(matriz.mds.test)))
# 
# for (i in 1:ncol(matriz.mds.met))
# {
#   for (j in 1:ncol(matriz.mds.test))
#   {
#     fila.cos[i,j]<-1-cosine(matriz.mds.test[,j],matriz.mds.met[,i])
#   }
# }
# 
# save.image("capitulo04_TFM-2.RData")

