# Prova esame variabilità calanchi--->> IMPOSSIBILE DISTINGUERE CON IMMAGINI COSì ESTESE, QUIDNI ALTRO OGGETTO DI ANALISI??

library(raster)
library(rasterVis)
library(RStoolbox)
library(ggplot2) 
library(gridExtra)
library(viridis)

setwd("C:/lab/Esame")

# creazione di una lista di file che ha qualcosa in comune nel nome (pattern)
# il pattern è il percorso di riconoscimento dei file ossia il fatto che hanno un pezzo di nome comune "lst"
# associo la lista al nome rlist
list10m2019 <- list.files(pattern="T32TPQ_20190228T101029")
list10m2019
# ora applico la funzione raster alla lista con la funzione lapply
# lapplay necessita della lista di dati su cui vogliamo applicare la funzione, che scriviamo dopo la virgola (listadati,funzione) 
# nomino le immmagini insieme con import
import10m2019 <- lapply(list10m2019,raster)
import10m2019
# adesso abbiamo i singoli file importati e per facilitare le future operazioni 
# unisco le immagini in un unico file come se fossero più livelli (bande) all'interno di un unico file
# salvo tutto con un nome file TGr
R10m2019 <- stack(import10m2019)
R10m2019
plot(R10m2019$T32TPQ_20190228T101029_B02_10m)
plotRGB(R10m2019, 1,2,3, stretch="Lin")
