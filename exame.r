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
listemilia <- list.files(pattern="emilia20m")
listemilia
listtoscana <- list.files(pattern="tosc20m")
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
plot(emilia2021$emilia20m_TCI)
plotRGB(emilia2021, 4, 3, 2, stretch="lin") # colori naturali
toscana2021 <- stack(importtoscana)
toscana2021
plot(toscana2021)
plotRGB(toscana2021,4, 3, 2, stretch="lin") # colori naturali

# BANDE:
# B2 = blu
# B3 = green
# B4 = red
# B8 = near infrared
# B11 = short wave infrared
# AOT = aerosol optical thickness
# WVP = water colour images

# Analisi multivariata (PCA)

emiliarid <- aggregate(emilia2021, fact=3) # Aggrego i pixel di un fattore 5, quindi diventeranno pixel di 100x100 metri, per velocizzare i calcoli.
emiliarid
plotRGB(emiliarid, 4, 3, 2, stretch="Lin")
emiliapca <- rasterPCA(emiliarid)
plot(emiliapca$map)
emiliapca #informazioni
summary(emiliapca$model)
# pc1=76,5% pc2=18,4% pc3=0,04% ... pc13

toscanarid <- aggregate(toscana2021, fact=3)
toscanarid
plotRGB(toscanarid, 4, 3, 2, stretch="Lin")
toscanapca <- rasterPCA(toscanarid)
plot(toscanapca$map)
toscanapca #informazioni
summary(toscanapca$model)
# pc1=82,7% pc2=13,7% pc3=0,03% ... pc13

# SERVE LA PULIZIA DALLE NUVOLEEEEE GRAZIEEEEE

# Variabilità spaziale sulla componente principale della pca (deviazione standard)

emiliapc1 <- emiliapca$map$PC1
emiliasd <- focal(emiliapc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)

emiliaplasma <- ggplot() + 
  geom_raster(emiliasd, mapping = aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis(option = "plasma") +
  ggtitle("Dev. St. of pc1 by plasma colour scale")
plot(emiliaplasma)

toscanapc1 <- toscanapca$map$PC1
toscanasd <- focal(toscanapc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)

toscanaplasma <- ggplot() + 
  geom_raster(toscanasd, mapping = aes(x=x, y=y, fill=layer)) + 
  scale_fill_viridis(option = "plasma") +
  ggtitle("Dev. St. of pc1 by plasma colour scale")
plot(toscanaplasma)


