# R code complete - Telerilevamento Geo-Ecologico.

# Summary:
# 1. Remote sensing first code
# 2. R code time series 
# 3. R code Copernicus data
# 4. R code knitr
# 5. R code multivariate analysis
# 6. R code classification
# 7. R code ggplot2
# 8. R code vegetation indices
# 9. R code land cover
# 10. R code variability

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

# 1. My first code in R for remote sensing.

# Installare il pacchetto raster di R (esterno a R quindi argomento tra virgolette), funzione per installare è install.packages.
install.packages("raster")
# Utilizzo del pacchetto raster con le funzioni annesse possibile caricandolo con la funzione library (loading, attaching and listing of packages).
library(raster) # NO virgolette perché raster già dentro R con install.packages

# Creazione della cartella di lavoro (set working directory) attraverso la funzione 'setwd':
setwd("C:/lab/") # for windows
# Importare dati raster da lab dentro a R associandoli direttamente ad una variabile(nome), si utilizza la funzione brick all'intenro del pacchetto raster.
p224r63_2011 <- brick("p224r63_2011_masked")
# Per avere le informazioni (dimensioni, tipo, ...) del dato appena caricato basta scriverlo
p224r63_2011
# Graficare l'immagine appena caricata in p224r63_2011
plot(p224r63_2011)
# Cambio la scala dei colori (scala di grigi per tutte le bande)
# funzione colorRampPalette (vettore colori) e scala (n.livelli)=100 fuori dalla parentesi funzione.
cl1 <- colorRampPalette(c("black","gray","light gray"))(200)
# Rigrafichiamo l'immagine con i colori cl, inserendo in plot col(colore)=cl
plot(p224r63_2011, col=cl1)
# Scala colori diversa 2
cl2 <- colorRampPalette(c("black","purple","light blue","green","yellow","orange","red","white")) (200)
plot(p224r63_2011, col=cl2)
p224r63_2011

# Bande informaizone:
# B1: blu
# B2: verde
# B3: rosso
# B4: infrarosso vicino
# B5: infrarosso medio1
# B6: infrarosso termico
# B7: infrarosso medio2

# Plot di una sola banda, legando l'immagine al nome della banda (info) 
# prima pulisco R da tutti i grafici con la funzione dev.off()
dev.off()
plot(p224r63_2011$B1_sre, col=cl2)
# Visualizzare e plottare insieme alcune bande che vogliamo con la funzione par
# Par ha bisogno di "multi frame" che spezza la finestra in più frame
# i frame sono righe e colonne, se si scrive row il primo numero è indicato per le righe,
# se mfcol il primo numero nel vettore è indicativo delle colonne.
par(mfrow=c(2,2))
plot(p224r63_2011$B1_sre)
plot(p224r63_2011$B2_sre)
plot(p224r63_2011$B3_sre)
plot(p224r63_2011$B4_sre)

# bande con scala colori diverse nella stessa finestra visualizzazione
clb <- colorRampPalette(c("dark blue","blue","light blue")) (200)
clg <- colorRampPalette(c("dark green","green","light green")) (200)
clr <- colorRampPalette(c("dark red","red","pink")) (200)
clnir <- colorRampPalette(c("red","orange","yellow")) (200)

par(mfrow=c(2,2))
plot(p224r63_2011$B1_sre, col=clb)
plot(p224r63_2011$B2_sre, col=clg)
plot(p224r63_2011$B3_sre, col=clr)
plot(p224r63_2011$B4_sre, col=clnir)

# visuallizzazione immagini in RGB ossia colori naturali visibili
# utilizza la funzione plotRGB che utilizza immagini multi-layer
# associa ogni banda (1,2,3) allo schema rgb, più meccanismo di stretch="Lineare" 
# che "stende" "allunga" il range colori di ogni banda così da aver una sfumatura maggiore.
par(mfrow=c(2,2))
plotRGB(p224r63_2011,r=3,g=2,b=1,stretch="Lin") # colori naturali
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin") # nir nel rosso
plotRGB(p224r63_2011,r=3,g=4,b=1,stretch="Lin") # nir nel verde
plotRGB(p224r63_2011,r=3,g=2,b=4,stretch="Lin") # nir nel blu

par(mfrow=c(2,2))
plotRGB(p224r63_2011,r=3,g=2,b=1,stretch="Lin") # colori naturali stretch lineare
plotRGB(p224r63_2011,r=3,g=2,b=1,stretch="Hist") # colori naturali stretch histogram
plotRGB(p224r63_2011,r=3,g=4,b=1,stretch="Lin") # nir nel verde stretch lineare
plotRGB(p224r63_2011,r=3,g=4,b=1,stretch="Hist") # nir nel verde stretch histogram

#importo l'immagine della stezza zona dell'anno 1988.
p224r63_1988 <- brick("p224r63_1988_masked.grd")
p224r63_1988
plot(p224r63_1988)
# visualizzo l'immgine a colori naturali:
plotRGB(p224r63_1988,r=3,g=2,b=1,stretch="Lin")
# Associo infrarosso al colore red
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")

#Confronto foto 1988 con 2011 nella stessa finestra con immagini con NIR nella componente red
par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
#uguale con stretch Histogram a confronto con lineare:
par(mfrow=c(2,2))
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(p224r63_1988,r=4,g=3,b=2,stretch="Hist")
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Hist")

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

#2. R_code_time_series
#Time series analysis of Greenland increase temperature
#Data and code from Emanuela Cosma

#installiamo e attiviamo i pacchetti che ci servono (raster e rasterVis):
install.packages("raster")
install.packages("rasterVis") #metodi per visualizzare i dati raster
library(raster)
library(rasterVis)

#settiamo la cartella di lavoro:
setwd("C:/lab/Greenland")

#importiamo i 4 dataset e lo plottiamo per verifica
lst_2000 <- raster("lst_2000.tif")
plot(lst_2000)
lst_2005 <- raster("lst_2005.tif")
plot(lst_2005)
lst_2010 <- raster("lst_2010.tif")
plot(lst_2010)
lst_2015 <- raster("lst_2015.tif")
plot(lst_2015)

#visualizziamo tutte insieme le immagini per confrontarle
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)

# metodo più veloce per importare più immagini contemporaneamente
# creazione di una lista di file che ha qualcosa in comune nel nome (pattern)
# il pattern è il percorso di riconoscimento dei file ossia il fatto che hanno un pezzo di nome comune "lst"
# associo la lista al nome rlist
rlist <- list.files(pattern="lst")
rlist
# ora applico la funzione raster alla lista con la funzione lapply
# lapplay necessita della lista di dati su cui vogliamo applicare la funzione, che scriviamo dopo la virgola (listadati,funzione) 
# nomino le immmagini insieme con import
import <- lapply(rlist,raster)
import
# adesso abbiamo i singoli file importati e per facilitare le future operazioni 
# unisco le immagini in un unico file come se fossero più livelli (bande) all'interno di un unico file
# salvo tutto con un nome file TGr
TGr <- stack(import)
TGr
plot(TGr)
plotRGB(TGr,1,3,4,stretch="LIn")

# funzione levelplot: plotta tutte le immagini e inserisce un unica legenda per tutte al contrario di par
levelplot(TGr)
# levelplot singola mappa:
levelplot(TGr$lst_2000)

# creo una scala colori con la funz colorRamppalette e la salvo come cl
cl <- colorRampPalette(c("blue","light blue","pink","red"))(100)
#plotto con scala colori appena scelta (utilizzare col.regions come argomento per indicare la scala colore)
levelplot(TGr, col.regions=cl,main="LST variation in time", names.attr=c("July 2000","July 2005","July 2010","July 2015"))

# LAVORO SU I DATI "MELT" SCIOGLIEMNTO GHIACCI

# creazione lista con pattern comune melt e la associo al nome variabile "meltlist"
meltlist <- list.files(pattern="melt")
# importo i file raggruppatiti nella lista e li salvo con il nome "melt_import"
melt_import <- lapply(meltlist,raster)
# unisco i file importati in unico file "melt" con più layer 
melt <- stack(melt_import)
melt
# plotto il file con la funzione levelplot 
# valori di scioglimento dei ghiacci
levelplot(melt)

# OPERAZIONI FRA MATRICI semplicemente con -/+ fra i layer
# nomi file da inserire con "nome file generale $ nome attributo layer"
# salviamo in melt_amount
melt_amount <- melt$X2007annual_melt-melt$X1979annual_melt

# plotto il risultato della differenza di ghiacciaio sciolto nell'arco di tempo
# più valori sono alti pù lo scioglimento è grande
levelplot(melt_amount, col.regions=cl)


#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

# 3. R code Copernucus data
# Codice per estrazioni dati da copernicus
# visualizing Coperniscus data

install.packages("raster")
library(raster)
# Pacchetto necessario per la estrazione dei dati copernicus
install.packages("ncdf4")
library(ncdf4)

setwd("C:/lab/")

# Estrarre il dato e caricarlo sulla variebile "albedo" con la funzione raster
albedo2019_12_03 <- raster("c_gls_ALBH_201912030000_GLOBE_PROBAV_V1.5.1.nc")
albedo2019_12_03

# visualizzare immagine dataset con una nuova scala colori
cl <- colorRampPalette(c("blue","light blue","green","pink","red","yellow"))(100)
plot(albedo2019_12_03,col=cl,main="Albedo mondiale dicembre 2019")

# Ricampiono i dati per diminuire la pesantezza dell'immagine di una scala di diecimila
# per ciò si inserisce un fattore 100 (aggrega 100x100 di pixel originali per lato)
# Nuovi pixel più grandi con una media di valori dei pixel originali
# salvo il file aggregato in albedores
albedores <- aggregate(albedo2019_12_03,fact=100)
plot(albedores, col=cl, main="Albedo mondiale ridotto")

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

#4. R_code_knitr
#Codice che utilizza il pacchetto knitr per creare un report dei risultati

# installo il pacchetto knitr per poter pescare il codice (file testo) esterno 
install.packages("knitr")
library(knitr)

setwd("C:/lab/Greenland")

#Funzione per generare il report: stitch
# servono 3 argomenti:
# 1-nome file di testo
# 2-template utilizzato, nel nostrocaso misc
# 3-pacchetto utilizzato, knitr

stitch("C:/lab/Greenland/R_code_greenland.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

# 5. R_code_multivariate_analysis.r

#librerie necessarie:
library(raster)
library(RStoolbox)
# Fisso la cartella di lavoro
setwd("C:/lab/")

# Funzione brick per caricare un set di dati:
p224r63_2011 <- brick("p224r63_2011_masked.grd")
p224r63_2011 #info immagine

# Grafichiamo i valori della banda 1 contro i valori della banda 2,
# leghiamo il nome della banda alla immagine dove sono contenuti con il dollaro,
# personaliziamo con colore, forma segnale punta, dimensioni segnale.
plot(p224r63_2011$B1_sre,p224r63_2011$B2_sre,col="red",pch=19, cex=2)

#Mettiamo in correlazione a due a due tutte le bande(variabili) contenute nell'immagine
pairs(p224r63_2011)

#ANALISI PCA
# Siccome l'analisi pixel per pixel, conitene molte operazioni e tempo
# Quindi meglio prima ridimensionare i pixel aggregandoli, quindi diminuendo la risoluzione.
# passaggio da pixel piccoli a più grandi con un fattore di x10 per lato di pixel.
p224r63_2011res <- aggregate(p224r63_2011, fact=10)

# Verifica che abbia funzionato (abbassamento risoluzione) 
# Plottando comparando le due immagini prima e dopo aggregate.
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="lin") # Dettaglio pixel 30x30 m
plotRGB(p224r63_2011res, r=4, g=3, b=2, stretch="lin") # Dettaglio pixel 300x300 m

# Effettuare l'analisi PCA (principal component analisys)
# ossia trovare una immagine con MAPPA, MODELLO, chiamata generate con rasterPCA.
# Si creano i principal component ossia le bande, dove la prima contiene la maggiore variabilità.
p224r63_2011res_pca <- rasterPCA(p224r63_2011res)
# Per vedere la variabilità spiegata delle singole bande appena create si usa un summary:
summary(p224r63_2011res_pca$model)
# la prima banda spiega il 99%, ma per spiegare il 100% ovviamente bisogna avere un numero 
# di bande (principal component) che sia uguale al numero di bande iniziali quindi 7.
plot(p224r63_2011res_pca$map) # mappe diverse per componente con massimi valori nella prima e poi a scalare
p224r63_2011res_pca$call # codice/riga che ha generato l'immagine 
plotRGB(p224r63_2011res_pca$map, r=4,g=3,b=2,stretch="lin")

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

# 6. R code classification 

library(raster)
install.packages("RStoolbox")
library(RStoolbox)

# settare la working directory 
setwd("C:/lab/")

#importo dati esterni con funzione brick e salvo immagine in "so"
so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
#informazione immagine:
so
#visualizzazione livelli con schema RGB colori naturali utilizzando tutti i pixel con stretch lineare
plotRGB(so, 1,2,3, stretch="lin")

# classificazione non supervisionata dall'utente, ossia il software effettua la classificazione da solo, basandosi sul numero di classi scelto da noi.
# Il software, partendo da un numero random di pixel di partenza a cui associa una classe, contiunua poi ad associare gli altri pixel alle classi da lui scelte
# in base alla somiglianza con la riflettanza nei pixel di partenza. 
# Per associare un pixel ad una certa classe, che ha certi valori basati sui pixel di settaggio iniziali, utilizza una funzione di massima verosomiglianza.
soc3 <- unsuperClass(so, nClasses = 3)
# visualizzare la mappa creata ma esplicitando di plottare la parte "map" di soc,
# perchè in soc sono contenute più informazione; mappa e il modello da cui si è costruito le classi
# quindi esplicitare di plottare la mappa legando con il dollaro.
plot(soc3$map)
#cassificazione non supervisionata con 20 classi
soc20 <- unsuperClass(so, nClasses = 20)
plot(soc20$map)

#Carico altra immagine

sun <- brick("sun.jpg")
plotRGB(sun, 1,2,3, stretch="lin")

sunc <- unsuperClass(sun, nClasses = 20)
plot(sunc$map)

# CLASSIFICAZIONE GRAND CANION
# carico l'immagine con brick
gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")
gc # info immogine
# graficare la foto in colori naturali 
plotRGB(gc, r=1,g=2,b=3, stretch="lin")
plotRGB(gc, r=1,g=2,b=3, stretch="hist")
# uno stretch differente produce colori diversi che sono utili per risaltare certe informazioni,
# lo stretch Histogram produce una mappa che sembrain 3D, si intuiscono meglio le morfologie.

#Classificazione non supervisionata in 2 classi:
gcc <- unsuperClass(gc, nClasses = 4)
plot(gcc$map)
# Visualizzo immagine a colori naturali con immagine classificata per conffronto diretto
par(mfrow=c(2,1))
plotRGB(gc, r=1,g=2,b=3, stretch="hist")
plot(gcc$map)

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-



#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
