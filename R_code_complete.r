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
# 11. R code spectral signature

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


# 7. R code ggplot2

# carichiamo i pacchetti necessari, per i plot di ggplot2 serve il pacchetto ggplot2, e per farne di più assieme serve gridExtra.
library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)

#setto la cartella di lavoro
setwd("~/lab/")

#carico la foto
p224r63 <- brick("p224r63_2011_masked.grd")

# Effettuo i plot con ggRGB per visualizzare con le bande (numero) nei colori successivi red, green, blue.
ggRGB(p224r63,3,2,1, stretch="lin")
ggRGB(p224r63,4,3,2, stretch="lin")

p1 <- ggRGB(p224r63,3,2,1, stretch="lin")
p2 <- ggRGB(p224r63,4,3,2, stretch="lin")

grid.arrange(p1, p2, nrow = 2) # this needs gridExtra


#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-


# 8. R_code_vegetation_indices.r

# serve il pacchetto raster
library(raster)
library(RStoolbox)
library(rasterdiv)
library(rasterVis)

# impostare la cartella di lavoro
setwd("C:/lab/")

# Caricare la immagini
defor1 <- brick("defor1.jpg")
defor2 <- brick("defor2.jpg")

# b1 = NIR, b2 = red, b3 = green

# Visualizziamo le immagini con i colori con coi sono stati importati
par(mfrow=c(2,1))
plotRGB(defor1, r=1,b=2,g=3, stretch="lin")
plotRGB(defor2, r=1,b=2,g=3, stretch="lin")
# La zona è la stessa perchè:
# 1. il fiume è uguale (anche se i fiumi possono cambiare nel tempo)
# 2. si vede una tessera di territorio al centro che rimane uguale
defor1
# Difference Vegetation Index, differenza pixe per pixel 
#della banda NIR (alta riflettanza veg) - Banda red(bassa relf veg)
dvi1 <- defor1$defor1.1 - defor1$defor1.2
plot(dvi1)

#plottiamo con colori più indicati
cl<- colorRampPalette(c("darkblue","yellow","red","black"))(100)
plot(dvi1, col=cl)
#Stessa cosa per defor2 calcoliamo il dvi:
defor2
# Difference Vegetation Index, differenza pixe per pixel 
#della banda NIR (alta riflettanza veg) - Banda red(bassa relf veg)
dvi2 <- defor2$defor2.1 - defor2$defor2.2
plot(dvi2)

#plottiamo con colori più indicati
cl<- colorRampPalette(c("darkblue","yellow","red","black"))(100)
plot(dvi2, col=cl)

# Paragone pre e post:
par(mfrow=c(2,1))
plot(dvi1, col=cl, main="dvi at time 1")
plot(dvi2, col=cl, main="dvi at time 2")

#EVIDENTE CHE LA PARTE GIALLA è MOLTO MAGGIORE, RAPPRESENTANDO UNA RIDUZIONE DEL DVi, QUINDI UNA RIDUZIONE DELLA VEG SANA. 
#Differenza del dvi fra i due tempi delle immagine (pixel per pixel)
difdvi <- dvi1 - dvi2
cld <- colorRampPalette(c("blue","white","red"))(100)
plot(difdvi, col=cld)

# Per poter paragonare immagini a diversi bit bisogna normalizzare il dvi in ndvi
# ndvi = (nir-red)/(nir+red) così tutte le immagini hanno un range da -1 a 1
ndvi1 <- (defor1$defor1.1 - defor1$defor1.2)/(defor1$defor1.1 + defor1$defor1.2)
plot(ndvi1, col=cl)
# Per seconda immagine uguale:
ndvi2 <- (defor2$defor2.1 - defor2$defor2.2)/(defor2$defor2.1 + defor2$defor2.2)
plot(ndvi2, col=cl)
# Differenza di ndvi
difndvi = ndvi1 - ndvi2
plot(difndvi, col=cld)

# Possibile calcolare più indici simultaneamente compreso dvi e ndvi
# con la funzione "spectralIndices" all'interno del pacchetto RStoolbox
# Consulta il manuale su internet per utilizzarla
# Calcolo tutti gli indici considerando le bande tranne quella del blu:
vi1 <- spectralIndices(defor1, green = 3, red = 2, nir = 1)
plot(vi1, col=cl)
vi2 <- spectralIndices(defor2, green = 3, red = 2, nir = 1)
plot(vi1, col=cl)

# RASTERDIV: worldwide ndvi
plot(copNDVI)
#Togliere l'acqua, pixel con valore 253,254,255 li impostiamo come NA.
#perhe riflettanza massima è data dall'acqua, eliminarla per migliorare la visualizzazione del resto.
copNDVI <- reclassify(copNDVI, cbind(253:255,NA))
plot(copNDVI)

# serve rasterVis:
levelplot(copNDVI)


#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-


# 9. R_code_land_cover.r

library(raster)
library(RStoolbox)
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
#library(rasterdiv)
#library(rasterVis)

setwd("C:/lab/")

defor1 <- brick("defor1.jpg")
defor2 <- brick("defor2.jpg")
# Informazione immagini NIR=banda 1, Red=banda 2, green=banda 3
par(mfrow=c(2,1))
plotRGB(defor1, r=1,g=2,b=3, stretch="lin")
plotRGB(defor2, r=1,g=2,b=3, stretch="lin")
# Con il pacchetto ggplot si ottengono delle visualizzazioni plot
# esteticamente migliori, basta usare la funzione che si chiama ggRGB al posto di plotRGB
# per visualizzarli insieme(multiframe), al posto di par, bisogna installare un pacchetto "gridExtra"
# dopo poi si utilizza la funzione "grid.arrange"
p1 <- ggRGB(defor1, r=1,g=2,b=3, stretch="lin")
p2 <- ggRGB(defor2, r=1,g=2,b=3, stretch="lin")
grid.arrange(p1,p2, nrow=2) # plotto insieme due immagini RGB (p1 e p2) lungo 2 righe (nrow=2) e una colonna.
# Classificazione non supervisionata, scelta dei punti per impostare le classi in maniera automatica randomica da parte sel software
# classificazione con 2 classi delle due immagini
set.seed(2) #imposta lo stesso risultato perchè stesso set originale
d1c <- unsuperClass(defor1, nClasses = 2)
d2c <- unsuperClass(defor2, nClasses = 2)
d1c
d2c
plot(d1c$map)
plot(d2c$map)
# Divide la parte di foresta(classe 2) e la parte coltivata(classe 1)
# il fiume è in parte nel agricolo e parte foresta in base alla riflettanza
# per migliorare la classificazione si possono fare più classi
d1c3 <- unsuperClass(defor1, nClasses = 3)
d2c3 <- unsuperClass(defor2, nClasses = 3)
plot(d1c3$map)
plot(d2c3$map)
# Ha diviso la parte di agricoltura in due classi

# Distribuzione dei pixel nelle varie classi: frequenza prima immagine anno 1992
f1 <- freq(d1c$map)
# classe 1 foresta = 306167 pixel
# classe 2 agricolo = 35125 pixel
s1 <- 306167+35125 #Pixel totali
#proporzione di pixel nell'immagine facendo la divisione del risultato della function freq e i pixel totali
prop1 <- f1/s1
prop1 # 89,7% foresta 10,3 % agricolo
# Frequenza immagine 2006
f2 <- freq(d2c$map)
# classe 1 foresta = 306167 pixel
# classe 2 agricolo = 35125 pixel
s2 <- 342726 #ATTENZIONE i pixel totali non sono uguali a prima
prop1 <- f2/s2
prop1 #52 % foresta 48 % agricoltura

# Creazione di un data frame per visualizzare i dati di percentuale di copertura del suolo
# Si utilizza la funzipone data.frame(nomi colonne)
# Creo le colonne a cui associo i componenti:
Cover <- c("Forest","Agriculture") 
Percent_1992 <- c(89.7,10.3)
Percent_2006 <- c(52,48)
# Creo il data set a tabella:
percentages <- data.frame(Cover,Percent_1992,Percent_2006)
percentages

# Grafichiamo il dataset utilizzando ggplot specificando le caratterisctiche estetiche
ggp1 <- ggplot(percentages, aes(x=Cover,y=Percent_1992, color=Cover)) + geom_bar(stat = "identity",fill="white")
# aes: inserimento degli assi da cui prednere i dati, color=Cover costruisce la legenda in base al colore
# + geom_bar:g geometria del grafico a barre, stat="identity": i dati li prende così come sono grezzi, fill="white": colore riempimento barre
ggp2 <-ggplot(percentages, aes(x=Cover,y=Percent_2006, color=Cover)) + geom_bar(stat = "identity",fill="white")

#Visualizziamo insieme i due grafici con grid.arrange
grid.arrange(ggp1, ggp2, nrow=1)


#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

# 10. R_code_variability.r

#Pacchetti richiesti:
library(raster)
library(RStoolbox)
install.packages("viridis") # for ggplot colouring
library(ggplot2) # for plotting ggplot
library(gridExtra) # for plotting ggplot toghether
library(viridis)
#Imposto la cartella di lavoro
setwd("C:/lab/")

#importiamo l'immagine sentinel
sent <- brick("sentinel.png")

# NIR 1, RED 2, GREEN 3
# r=1, g=2, b=3
plotRGB(sent) 
#funziona lo stesso perchè mette l'opzione standard 
# con red sul 1, green sul 2, blu sul 3, e stretch lineare

#Cambiamo i colori con il NIR sul verde, vegetazione verde fluo
plotRGB(sent, r=2, g=1, b=3, stretch="lin")

#nome bande:
sent
#assoccio le componenti nir e red ad un oggetto per facilitare i calcoli
nir <- sent$sentinel.1
red <- sent$sentinel.2
#calcolo del NDVI SU CUI POI CALCOLARE DEV. E MEAN PERCHè APPLICABILI SOLO AD UNA BANDA/SINGOLO STRATO
ndvi <- (nir-red)/(nir+red)
#visualizzo NCVI con una scala colori creata da noi
cl <- colorRampPalette(c("black","white","red","magenta","green"))(200)
plot(ndvi, col=cl)

# Calcolo della deviazione standard con il metodo "moving window" attraverso la funz focal
# "w=..." =geometria finestra (attenzione conviene forma isotropa), "fun=.." funzione da calcolare, sd=standard deviation
ndvisd3 <- focal(ndvi, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
clsd <- colorRampPalette(c("blue","green","pink","magenta","orange","brown","red","yellow"))(100)
plot(ndvisd3, col=clsd)

# Rifacciamo il calcolo con la funzione media
ndvimean3 <- focal(ndvi, w=matrix(1/9, nrow=3, ncol=3), fun=mean)
plot(ndvimean3, col=clsd)

# Aumentiamo la grandezza della finestra mobile, avendo una risoluzione della deviazione standard minore
ndvisd9 <- focal(ndvi, w=matrix(1/81, nrow=9, ncol=9), fun=sd)
plot(ndvisd9, col=clsd)

# SE NON VOGLIAMO CALCOLARE LA DEV.ST. E MEAN SUL INDICE NDVI,
# FACCIAMO UN ANALISI MULTIVARIATA (PCA) DA OTTENERE UNA BANDA PRINCIPALE CHE CONTIENE LA MAGGIOR PARTE DELLA VARIABILITà
sentpca <- rasterPCA(sent)
plot(sentpca$map)
sentpca #informazioni
summary(sentpca$model) # quanta variabilità spiegnao le componenti (assi)
# pc1=77.33 %, pc2=53.51 %, pc3=5.77 %, pc4=0 %.
pc1 <- sentpca$map$PC1
pc1sd <- focal(pc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
clsd <- colorRampPalette(c("blue","green","pink","magenta","orange","brown","red","yellow"))(100)
plot(pc1sd, col=clsd)
# Colore omogeneo blu rappresenta le valli o praterie di alta quota, in cui si ha appunto una certa omogeneità nel terreno (meno variabilità)
# Metre i colori rossi rappresentano le zone con più variabilità, ossia crepacci e sone in roccia diversificate.
# Macchia blu in alto a sinistra è una nuvola, per quello così omogeneo

# Come richiamare pezzi di codice già scritti senza dover copiare e incollare.
source("source_test_lezione.r") #dev.st. 7x7 di pc1

# Plot con ggplot, utilizzando una sorgente
source("source_ggplot.r")

# Contenuto del source applicato ai miei dati:
pviridis <- ggplot() + #crea una nuova finestra vuota
  geom_raster(pc1sd, mapping = aes(x=x, y=y, fill=layer)) + # crea la geometria a griglia di pixel(raster) e la mappa con le aesthetics inserite da noi
  scale_fill_viridis() + #Utilizza la legenda (color palette) già preparata, di default utilizza quella "viridis".
  ggtitle("Dev. St. of pc1 by viridis colour scale") #Titolo immagine

# Utiliziamo un altra legenda (colour scale)
pturbo <- ggplot() + 
  geom_raster(pc1sd, mapping = aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis(option = "turbo") +
  ggtitle("Dev. St. of pc1 by turbo colour scale") #ATTENZIONE trae in inganno il colore giallo che spicca ma in realtà è a metà dei vaolori in legenda.

pplasma <- ggplot() + 
  geom_raster(pc1sd, mapping = aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis(option = "plasma") +
  ggtitle("Dev. St. of pc1 by plasma colour scale")

pmagma <- ggplot() + 
  geom_raster(pc1sd, mapping = aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis(option = "magma") +
  ggtitle("Dev. St. of pc1 by magma colour scale")


#Plottiamo insieme tutte le possibili legende che vogliamo, con grid.arrange utilizzando gli oggetti a cui abbiamo associato le mappe precedenti di viridis
grid.arrange(pviridis, pplasma, pmagma, pturbo, nrow=2, ncol=2)

#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

# 11. R_code_spectral_signatures

#Pcchetti richiesti da caricare
library(raster)
library(rgdal)  #libreria 
library(ggplot2)

#Impostare la cartella di lavoro
setwd("C:/lab/") # windows

#Carichiamo tutte le bande della immagine defor2, già utilizzata. immagine della deforestazione(post deforestazione)
defor2 <- brick("defor2.jpg")

# defor2.1, defor2.2, defor2.3 
# NIR, red, green
# plottiamo l'immagine con il nir sul rosso così da evidenziare la presenza di vegetazione
plotRGB(defor2, r=1, g=2, b=3, stretch="lin") #stretch lineare
plotRGB(defor2, r=1, g=2, b=3, stretch="hist") #stetch Histograma enfatizza le differenze

#funzione click, per visualizzare i valori di riflettanza delle singole bande per ogni pixel. 
#Bisogna inserire l'immagine su cui si opera, dare un identificativo (numero) per ogni click, coordinate xy per punto premuto, numero di cella pixel premuto, tipo=punto, pch tipo segnapunto, cex esagerazione, colore segnapunto.
click(defor2, id=T, xy=T, cell=T, type="p", pch=16, cex=4, col="yellow")

# results points of vegetation (forest):
#      x      y      cell   defor2.1(NIR)    defor2.2(red)   defor2.3(green)
# 1  90.5   439.5   27337            210                12               27

# 1  159.5  241.5  169372            182                 2               13
  
# 1  347.5  225.5  181032            215                 9               21

# 1  668.5  369.5   78105            190                 3               10

# 1  364.5  256.5  158822            217                 5               19

# 1  383.5  385.5   66348            231                20               27

# results points of water (river):
#      x     y      cell    defor2.1(NIR)  defor2.2(red)  defor2.3(green)
# 1  87.5  234.5  174319             56              36               85

# 1 395.5  85.5   281460             59              75             134

# 1 636.5  262.5  154792             37              70             113

# Creazione di un dataset dove classifichiamo in base ai valori di riflettanza visti punto per punto
# define the columns of the dataset:
band <- c(1,2,3) # creazione 3 colonne
forest <- c(210,12,27) # valori primo punto nella foresta delle tre bande 
water <- c(59,75,134) # valori secondo punto nel fiume delle tre bande

# create the dataframe, creazioen di una tabella con i veri nomi delle tabelle
spectrals <- data.frame(band, forest, water)

#PLOTTAGGIIO DELLE FIMRE SPETTRALI DEI PUNTI SCELTI, attraverso la creazione di uno spazio grafico con ggplot
# con x=numero di banda e y=valore riflettanza, poi creazione delle linee con la funzione geom_line
# plot the sepctral signatures
ggplot(spectrals, aes(x=band)) +
  geom_line(aes(y=forest), color="green") +
  geom_line(aes(y=water), color="blue") +
  labs(x="band",y="reflectance")

############### Multitemporal

defor1 <- brick("defor1.jpg")

plotRGB(defor1, r=1, g=2, b=3, stretch="lin")

# spectral signatures defor1, andiamo a cliccare nelle zone dove c'è stato una più forte modifcia del paesaggio così da evidenziare la deforestazione
click(defor1, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

# results points of vegetation (forest) time1:
#      x      y      cell   defor2.1(NIR)    defor2.2(red)   defor2.3(green)
# 1 107.5  396.5    57942            217               27                53
# 2 324.5  261.5   154549            219               11                27
# 3 408.5   45.5   308857            214               22                37
# results points of water (river) time1:
#      x      y      cell   defor2.1(NIR)    defor2.2(red)   defor2.3(green)
# 4 610.5  277.5   143411            247              255               255
# 5 101.5  218.5   185028            173              229               226

# time t2, rifacciamo la stessa cosa segnando i punti nello stesso punto/zona così da verificare il cambiamento delle firma spettrale.
plotRGB(defor2, r=1, g=2, b=3, stretch="lin")
click(defor2, id=T, xy=T, cell=T, type="p", pch=16, col="yellow")

# results points of vegetation (forest) time1:
#      x      y      cell   defor2.1(NIR)    defor2.2(red)   defor2.3(green)
# 1 110.5  404.5    52452            197               30               38
# 2 316.5  278.5   143000            201              126              131
# 3 446.5   53.5   304455            189              171              159
# results points of water (river) time1:
#      x      y      cell   defor2.1(NIR)    defor2.2(red)   defor2.3(green)
# 4 617.5  286.5   137565             65               84              153
# 5 102.5  219.5   185089             11               44               79

# SAREBBE PIù CORRETTO GENERARE DEI PUNTI RANDOM E UTILIZZARE QUELLI SIA PER DEFOR1 DEFOR2, NON CLICCANDO NOI CHE SIAMO SOGGETTI AD ERRORI.

# define the columns of the dataset nella foresta:
band <- c(1,2,3)
time1p1 <- c(217,27,53)
time1p2 <- c(219,11,27)
time2p1 <- c(197,30,38)
time2p2 <- c(201,126,131)

spectralst <- data.frame(band, time1p1, time2p1, time1p2, time2p2)


# plot the sepctral signatures
ggplot(spectralst, aes(x=band)) +
  geom_line(aes(y=time1p1), color="red", linetype="dotted") +
  geom_line(aes(y=time1p2), color="red", linetype="dotted") +
  geom_line(aes(y=time2p1), linetype="dotted") +
  geom_line(aes(y=time2p2), linetype="dotted") +
  labs(x="band",y="reflectance")

#CARICIAMO UNA IMMAGINE DA EARTH OBSERVATORY DELLA NASA ED EFFETTUAMO LA FIRMA SPETTRALE UNA ZONA SCONOSCIUTA
# image from Earth Observatory

eo <- brick("june_puzzler.jpg")
plotRGB(eo, 1,2,3, stretch="hist")
click(eo, id=T, xy=T, cell=T, type="p", pch=16, cex=4, col="yellow")

# output
#     x     y  cell june_puzzler.1 june_puzzler.2 june_puzzler.3    
# 1 93.5 373.5 76414            187            163             11
#      x     y   cell june_puzzler.1 june_puzzler.2 june_puzzler.3
# 1 219.5 285.5 139900             11            140              0
#     x     y   cell june_puzzler.1 june_puzzler.2 june_puzzler.3
# 1 184.5 315.5 118265             41             40             20

# define the columns of the dataset:
band <- c(1,2,3)
stratum1 <- c(187,163,11)
stratum2 <- c(11,140,0)
stratum3 <- c(41,40,20)

spectralsg <- data.frame(band, stratum1, stratum2, stratum3)

# plot the sepctral signatures
ggplot(spectralsg, aes(x=band)) +
  geom_line(aes(y=stratum1), color="yellow") +
  geom_line(aes(y=stratum2), color="green") +
  geom_line(aes(y=stratum3), color="blue") +
  labs(x="band",y="reflectance")


#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
