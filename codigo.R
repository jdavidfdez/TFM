source("R/libreriasYfunciones.r")

articulos<-carga_datos.t.100()
table(articulos$AUTOR)

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
CA_plot_FILAS.1_3<-plot.CA(res_CA_AUX,axes=c(1,3),invisible="col",label="row",title="Eje 1 vs Eje 3",cex=.5)
grid.arrange(grobs=list(CA_plot_FILAS.1_2,CA_plot_FILAS.1_3),nrow=1,ncol=2)

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
  palabras.tf_idf<-data.frame(PALABRA=terminos_tf_idf.100[which(terminos_tf_idf.100$AUTOR==autores[i]),][order(terminos_tf_idf.100$tf_idf,decreasing=TRUE),][1:100,]$word,
                              AUTOR=autores[i])
  palabras.frecuentes<-data.frame(PALABRA=terminos_f(tidy[which(tidy$AUTOR==autores[i]),],n.pal)$TODOS,
                                  AUTOR=autores[i])
  for (j in 1:3)
  {
    if (i==1 && j==1)
    {
      palabras<-data.frame(PALABRA=names(res_CA_AUX$col$contrib[which(res_CA_AUX$col$contrib[,j]>=.5),j]),
                           CONTRIBUCION=res_CA_AUX$col$contrib[which(res_CA_AUX$col$contrib[,j]>=.5),j],
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
      palabras<-rbind(palabras,data.frame(PALABRA=names(res_CA_AUX$col$contrib[which(res_CA_AUX$col$contrib[,j]>=.5),j]),
                                          CONTRIBUCION=res_CA_AUX$col$contrib[which(res_CA_AUX$col$contrib[,j]>=.5),j],
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

table(frecuentes$AUTOR,frecuentes$DIMENSION)
cat("El ",round(100*sum(table(frecuentes$AUTOR,frecuentes$DIMENSION))/n.pal,2), " % (",sum(table(frecuentes$AUTOR,frecuentes$DIMENSION))," palabras) de las que mas aportan a la construcción de ","\n","los ejes se encuentra entre las 151 palabras mas frecuentes de alguno de ","\n","los cuatro autores")
table(tfIdf$AUTOR,tfIdf$DIMENSION)
cat("El ",round(100*sum(table(tfIdf$AUTOR,tfIdf$DIMENSION))/n.pal,2), " % (",sum(table(tfIdf$AUTOR,tfIdf$DIMENSION))," palabras) de las que mas aportan a la construcción de ","\n","los ejes se encuentra entre las 151 palabras de mayores valores tfIdf de ","\n","alguno de los cuatro autores")
table(unidos$AUTOR,unidos$DIMENSION)
cat("El ",round(100*sum(table(unidos$AUTOR,unidos$DIMENSION))/n.pal,2), " % (",sum(table(unidos$AUTOR,unidos$DIMENSION))," palabras) de las que mas aportan a la construcción de ","\n","los ejes se encuentra entre las 151 palabras de mayor frecuencia o de mmayores ","\n","valores tfIdf de alguno de los cuatro autores")

df.cols<-inner_join(unidos,data.frame(PALABRA=names(col.contrib[,1]),col.contrib),by="PALABRA")
unicos<-unique(df.cols[order(df.cols$CONTRIBUCION,decreasing=TRUE),c("PALABRA","DIMENSION","CONTRIBUCION")])
unicos$PALABRA
unicos[1:3,]

dim.columnas=inner_join(data.frame(PALABRA=names(res_CA_AUX$col$coord[,1]),D1=res_CA_AUX$col$coord[,1],
                                   D2=res_CA_AUX$col$coord[,2],D3=res_CA_AUX$col$coord[,3]),unidos,by="PALABRA")

gg.col1<-ggplot(data=dim.columnas[which(dim.columnas$AUTOR %in% c("BenjaminKangLim","AlexanderSmith","DavidLawder")),],aes(D1,D2))+
  geom_encircle(aes(group=AUTOR,fill=AUTOR),alpha=0.4)+
  geom_text(label=dim.columnas[which(dim.columnas$AUTOR %in% c("BenjaminKangLim","AlexanderSmith","DavidLawder")),]$PALABRA,
            size=1.5,color="blue")+
  geom_point(data=datos.filas[which(datos.filas$AUTOR %in% c("BenjaminKangLim","AlexanderSmith","DavidLawder")),],aes(D1,D2),color="red")+
  ggplot2::annotate(geom="text",x=datos.filas[c("BenjaminKangLim","AlexanderSmith","DavidLawder"),]$D1,
                    y=datos.filas[c("BenjaminKangLim","AlexanderSmith","DavidLawder"),]$D2,
                    label=datos.filas[c("BenjaminKangLim","AlexanderSmith","DavidLawder"),]$AUTOR,
                    color="RED",cex=2.5)+theme_bw()+
  theme(legend.position = "top",legend.title = element_text(size = 6),
        legend.text = element_text(size = 6))

gg.col2<-ggplot(data=dim.columnas[which(dim.columnas$AUTOR %in% c("BenjaminKangLim","BradDorfman","DavidLawder")),],aes(D1,D3))+
  geom_encircle(aes(group=AUTOR,fill=AUTOR),alpha=0.4)+
  geom_text(label=dim.columnas[which(dim.columnas$AUTOR %in% c("BenjaminKangLim","BradDorfman","DavidLawder")),]$PALABRA,
            size=1.5,color="blue")+
  geom_point(data=datos.filas[which(datos.filas$AUTOR %in% c("BenjaminKangLim","BradDorfman","DavidLawder")),],aes(D1,D3),color="red")+
  ggplot2::annotate(geom="text",x=datos.filas[c("BenjaminKangLim","BradDorfman","DavidLawder"),]$D1,
                    y=datos.filas[c("BenjaminKangLim","BradDorfman","DavidLawder"),]$D3,
                    label=datos.filas[c("BenjaminKangLim","BradDorfman","DavidLawder"),]$AUTOR,
                    color="RED",cex=2.5)+theme_bw()+
  theme(legend.position = "top",legend.title = element_text(size = 6),
        legend.text = element_text(size = 6))

grid.arrange(grobs=list(gg.col1,gg.col2),nrow=1,ncol=2)

# ESCALADO MULTIDIMENSIONAL
matriz.tdm.agregada<-t(matriz.dtm.agregado)
dist.cos<-1-cosine(matriz.tdm.agregada)
round(dist.cos,6)

v.propios<-cmdscale(dist.cos,eig=TRUE)
v.propios<-100*cumsum(v.propios$eig*(v.propios$eig>0))/sum(v.propios$eig*(v.propios$eig>0)) 
round(v.propios,6)

res.mds<-smacofSym(dist.cos)
datos.autores.mds<-data.frame("MET_D1"=res.mds$conf[,1],"MET_D2"=res.mds$conf[,1])

res.mds.no.met<-smacofSym(dist.cos,type="ordinal")
datos.autores.mds.no.met<-data.frame("NO.MET_D1"=res.mds.no.met$conf[,1],"NO.MET_D2"=res.mds.no.met$conf[,1])

sort(res.mds$spp,decreasing=TRUE)
sort(res.mds.no.met$spp,decreasing=TRUE)

df.rtados.mds<-data.frame("TIPO"=c("MET","NO.MET"),
                          "BONDAD"=c(1-res.mds$stress,1-res.mds.no.met$stress),
                          "ITERACIONES"=c(res.mds$niter,res.mds.no.met$niter))

df.rtados.mds

cbind(datos.autores.mds,datos.autores.mds.no.met)
gg.aut.mds<-ggplot(datos.autores.mds,aes(MET_D1,MET_D2))+
  geom_text(label=rownames(datos.autores.mds),size=1.5,color="blue")+
  theme_bw()
gg.aut.mds.no.met<-ggplot(datos.autores.mds.no.met,aes(NO.MET_D1,NO.MET_D2))+
  geom_text(label=rownames(datos.autores.mds.no.met),size=1.5,color="blue")+
  theme_bw()

grid.arrange(grobs=list(gg.aut.mds,gg.aut.mds.no.met),nrow=1,ncol=2)

matriz.dtm.col.sel<-matriz.dtm.agregado[,names(col.contrib[,1])]
dist.cos.col<-1-cosine(matriz.dtm.col.sel)
v.propios.col<-cmdscale(dist.cos.col,eig=TRUE)
v.propios.col<-100*cumsum(v.propios.col$eig*(v.propios.col$eig>0))/sum(v.propios.col$eig*(v.propios.col$eig>0)) 
headTail(round(v.propios.col,6))

res.mds.col<-smacofSym(dist.cos.col)

res.mds.col.no.met<-smacofSym(dist.cos.col,type="ordinal")

round(sort(res.mds.col$spp,decreasing=TRUE),4)
round(sort(res.mds.col.no.met$spp,decreasing=TRUE),4)

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

gg.pal.mds<-ggplot(df.palabras.unidos.unicos,aes(MET_D1,MET_D2))+
  geom_text(label=df.palabras.unidos.unicos$PALABRA,size=1.5,color="blue")+
  theme_bw()
gg.pal.mds.no.met<-ggplot(df.palabras.unidos.unicos,aes(NO.MET_D1,NO.MET_D2))+
  geom_text(label=df.palabras.unidos.unicos$PALABRA,size=1.5,color="blue")+
  theme_bw()
grid.arrange(grobs=list(gg.pal.mds,gg.pal.mds.no.met),nrow=1,ncol=2)

# ANÁLISIS DISCRIMINANTE
df.lda.bruto<-cbind(as.data.frame(numerica(mi.dtm)),articulos$AUTOR)
colnames(df.lda.bruto)[ncol(df.lda.bruto)]<-"AUTOR"
res.lda.bruto<-MASS::lda(x=df.lda.bruto[,-length(df.lda.bruto[1,])],grouping=df.lda.bruto$AUTOR,CV=TRUE)
predicciones.lda.bruto<-res.lda.bruto$class
confusion.lda.bruto<-table(df.lda.bruto$AUTOR,res.lda.bruto$class)
ari.lda.bruto<-adjustedRandIndex(df.lda.bruto$AUTOR,res.lda.bruto$class)
precision.lda.bruto<-evalua_confusion_global(confusion.lda.bruto)
confusion.lda.bruto
paste("Precision: ",precision.lda.bruto,sep="")
paste("Adjusted Rand Index: ",ari.lda.bruto,sep="")
rtados.autores.lda.bruto<-round(as.data.frame(evalua_x_autor(confusion.lda.bruto)),2)
colnames(rtados.autores.lda.bruto)<-c("Precis","Recup")
rtados.autores.lda.bruto

df.rtados.global<-data.frame("MODELO"="Orig","PRECIS"=precision.lda.bruto,"ARI"=ari.lda.bruto)
df.precis.autores<-data.frame("Orig"=rtados.autores.lda.bruto[,1],row.names=rownames(rtados.autores.lda.bruto))
df.recup.autores<-data.frame("Orig"=rtados.autores.lda.bruto[,2],
                             row.names=rownames(rtados.autores.lda.bruto))

matriz.tdm.frec<-t(numerica(mi.dtm))
dist.cos.tdm.frec<-1-cosine(matriz.tdm.frec)
v.propios.tdm.frec<-cmdscale(dist.cos.tdm.frec,eig=TRUE)
v.propios.tdm.frec<-100*cumsum(v.propios.tdm.frec$eig*(v.propios.tdm.frec$eig>0))/sum(v.propios.tdm.frec$eig*(v.propios.tdm.frec$eig>0))
round(v.propios.tdm.frec,6)

mds.met<-smacofSym(dist.cos.tdm.frec,ndim=198) # conserva > 99 %
config.mds.met<-cbind(as.data.frame(mds.met$conf),articulos$AUTOR)
colnames(config.mds.met)[ncol(config.mds.met)]<-"AUTOR"
res.mds.met<-MASS::lda(x=config.mds.met[,-length(config.mds.met[1,])],grouping=config.mds.met$AUTOR,CV=TRUE)
confusion.mds.met<-table(config.mds.met$AUTOR,res.mds.met$class)
precision.mds.met<-evalua_confusion_global(confusion.mds.met)
ari.mds.met<-adjustedRandIndex(config.mds.met$AUTOR,res.mds.met$class)
confusion.mds.met
paste("Precision: ",precision.mds.met,sep="")
paste("Adjusted Rand Index: ",ari.mds.met,sep="")
rtados.autores.mds.met<-round(as.data.frame(evalua_x_autor(confusion.mds.met)),2)
colnames(rtados.autores.mds.met)<-c("Precis","Recup")
rtados.autores.mds.met

df.rtados.global<-rbind(df.rtados.global,data.frame("MODELO"="MDS.Met","PRECIS"=precision.mds.met,"ARI"=ari.mds.met))
df.precis.autores<-cbind(df.precis.autores,
                         data.frame("MDS.Met"=rtados.autores.mds.met[,1],row.names=rownames(rtados.autores.mds.met)))
df.recup.autores<-cbind(df.recup.autores,
                        data.frame("MDS.Met"=rtados.autores.mds.met[,2],row.names=rownames(rtados.autores.mds.met)))


mds.no.met<-smacofSym(dist.cos.tdm.frec,ndim=198,type="ordinal")

config.mds.no.met<-cbind(as.data.frame(mds.no.met$conf),articulos$AUTOR)
colnames(config.mds.no.met)[ncol(config.mds.no.met)]<-"AUTOR"
res.mds.no.met<-MASS::lda(x=config.mds.no.met[,-length(config.mds.no.met[1,])],grouping=config.mds.no.met$AUTOR,CV=TRUE)
confusion.mds.no.met<-table(config.mds.no.met$AUTOR,res.mds.no.met$class)
precision.mds.no.met<-evalua_confusion_global(confusion.mds.no.met)
ari.mds.no.met<-adjustedRandIndex(config.mds.no.met$AUTOR,res.mds.no.met$class)
confusion.mds.no.met
paste("Precision: ",precision.mds.no.met,sep="")
paste("Adjusted Rand Index: ",ari.mds.no.met,sep="")
rtados.autores.mds.no.met<-round(as.data.frame(evalua_x_autor(confusion.mds.no.met)),2)
colnames(rtados.autores.mds.no.met)<-c("Precis","Recup")
rtados.autores.mds.no.met

df.rtados.global<-rbind(df.rtados.global,data.frame("MODELO"="MDS.No.Met","PRECIS"=precision.mds.no.met,"ARI"=ari.mds.no.met))
df.precis.autores<-cbind(df.precis.autores,
                         data.frame("MDS.No.Met"=rtados.autores.mds.no.met[,1],row.names=rownames(rtados.autores.mds.no.met)))
df.recup.autores<-cbind(df.recup.autores,
                        data.frame("RECUP.MDS.No.Met"=rtados.autores.mds.no.met[,2],row.names=rownames(rtados.autores.mds.no.met)))
df.rtados.global
df.precis.autores
df.recup.autores

df.graf.precis<-data.frame("AUTOR"=row.names(df.precis.autores),"METODO"=rep("Orig",4),"VALOR"=df.precis.autores[,1])

df.graf.precis<-rbind(df.graf.precis,data.frame("AUTOR"=row.names(df.precis.autores),
                                                "METODO"=rep("MDS.Met",4),
                                                "VALOR"=df.precis.autores[,2]))
df.graf.precis<-rbind(df.graf.precis,data.frame("AUTOR"=row.names(df.precis.autores),
                                                "METODO"=rep("MDS.NoMet",4),
                                                "VALOR"=df.precis.autores[,3]))

gg.precis.autores.wrap<-ggplot(df.graf.precis,xlab="") + 
  geom_col(aes(x = AUTOR, y=VALOR, fill = AUTOR), colour = NA) + 
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "",y="") + 
  theme(legend.position="bottom",legend.title = element_text(size = 6),legend.text = element_text(size = 6)) + 
  facet_wrap(facets = vars(METODO),nrow=1,ncol=3)

df.precis.autores
gg.precis.autores.wrap

df.graf.recup<-data.frame("AUTOR"=row.names(df.recup.autores),"METODO"=rep("Orig",4),"VALOR"=df.recup.autores[,1])

df.graf.recup<-rbind(df.graf.recup,data.frame("AUTOR"=row.names(df.recup.autores),
                                              "METODO"=rep("MDS.Met",4),
                                              "VALOR"=df.recup.autores[,2]))
df.graf.recup<-rbind(df.graf.recup,data.frame("AUTOR"=row.names(df.recup.autores),
                                              "METODO"=rep("MDS.NoMet",4),
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
