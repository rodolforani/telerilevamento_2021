# Prova esame influenza catena appenninica e suolo -->> Toscana vs Emilia dati Sentinel 2

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
listemilia <- list.files(pattern="eT32TPQ_20210528T100559")
listemilia
listtoscana <- list.files(pattern="r_")
listtoscana
# ora applico la funzione raster alla lista con la funzione lapply
# lapplay necessita della lista di dati su cui vogliamo applicare la funzione, che scriviamo dopo la virgola (listadati,funzione) 
# nomino le immmagini insieme con import
importemilia <- lapply(listemilia,raster)
importemilia
importtoscana <- lapply(listtoscana,raster)
importtoscana
# adesso abbiamo i singoli file importati e per facilitare le future operazioni 
# unisco le immagini in un unico file come se fossero più livelli (bande) all'interno di un unico file
# salvo tutto con un nome file TGr
emilia2021 <- stack(importemilia)
emilia2021
plot(emilia2021$eT32TPQ_20210528T100559_WVP_10m)
plotRGB(emilia2021, 4, 3, 2, stretch="lin") # colori naturali
toscana2021 <- stack(importtoscana)
toscana2021
plot(toscana2021$r_TCI_10m)
plotRGB(toscana2021, 4, 3, 2, stretch="lin") # colori naturali

# BANDE:
# B2=blu
# B3=green
# B4=red
# B8=near infrared
# AOT=aerosol optical thickness
# WVP=water colour images


