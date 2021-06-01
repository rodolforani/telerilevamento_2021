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
plotRGB(emilia2021, 4, 3, 2, stretch="Lin") # colori naturali
toscana2021 <- stack(importtoscana)
toscana2021
plotRGB(toscana2021,4, 3, 2, stretch="lin") # colori naturali

# BANDE:
# B2 = blu
# B3 = green
# B4 = red
# B8 = near infrared
# B11 = short wave infrared
# AOT = aerosol optical thickness
# WVP = water vapour map

# Pulizia delle nuvole con la soglia inferiore del 25% di brillantezza nella banda 4 (red).
# Le nuvole riflettono molto di più di tutto il il resto nella banda 4, per cui si è visto che tutto ciò che ha un valore nella banda 4
# superiore al 25% circa dei valori più alti, si può attribuire con adeguata sicurezza ad una nuvola.
# Di conseguenza è possibile eliminarlo attraverso la funzione "mask"
# Algoritmo di analisi trovato sulla pagina: "https://sentinels.copernicus.eu/web/sentinel/technical-guides/sentinel-2-msi/level-2a/algorithm".

emiliaNA <- emilia2021$emilia20m_B04 > 3000
plot(emiliaNA)
emiliamask <- mask(emilia2021, emiliaNA, maskvalue=TRUE)
plot(emiliamask$emilia20m_TCI)

toscanaNA <- toscana2021$tosc20m_B04 > 3000
plot(toscanaNA)
toscanamask <- mask(toscana2021, toscanaNA, maskvalue=TRUE)
plot(toscanamask$tosc20m_B04)

# NDSI (normalised difference snow index)  ?? LO TNEGO ?? A COSA MI PUò SERVIRE ????

#emiliaNDSI <- ((emiliamask$emilia20m_B03-emiliamask$emilia20m_B11)/(emiliamask$emilia20m_B03+emiliamask$emilia20m_B11))
#par(mfrow=c(1,2))
#plot(emiliaNDSI)

#emiliacloudy <- emiliaNDSI*emiliamask$emilia20m_B04
#plot(emiliacloudy)

# Analisi multivariata (PCA)

emiliarid <- aggregate(emiliamask, fact=3) # Aggrego i pixel di un fattore 3, quindi diventeranno pixel di 60x60 metri, per velocizzare i calcoli.
emiliarid
plotRGB(emiliarid, 4, 3, 2, stretch="Lin")
emiliapca <- rasterPCA(emiliarid)
emiliapca #informazioni
summary(emiliapca$model)
# pc1=75.6%, pc2=20.8%, pc3=0.016%, pc4=0.014%, ... , pc13

toscanarid <- aggregate(toscanamask, fact=3)
toscanarid
plotRGB(toscanarid, 4, 3, 2, stretch="Lin")
toscanapca <- rasterPCA(toscanarid)
toscanapca #informazioni
summary(toscanapca$model)
# pc1=69.4%, pc2=26.3%, pc3=0.027%, pc4=0.0098%, ..., pc13

# Variabilità spaziale sulla componente principale della pca (deviazione standard)

emiliapc1 <- emiliapca$map$PC1
emiliasd <- focal(emiliapc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)

#emiliaplasma <- ggplot() + 
#  geom_raster(emiliasd, mapping = aes(x=x, y=y, fill=layer)) + 
#  scale_fill_viridis(option = "plasma") +
#  ggtitle("Dev. St. of pc1 by plasma colour scale")
#plot(emiliaplasma)

toscanapc1 <- toscanapca$map$PC1
toscanasd <- focal(toscanapc1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)

#toscanaplasma <- ggplot() + 
#  geom_raster(toscanasd, mapping = aes(x=x, y=y, fill=layer)) + 
#  scale_fill_viridis(option = "plasma") +
#  ggtitle("Dev. St. of pc1 by plasma colour scale")
#plot(toscanaplasma)

levelplot(emiliasd)
levelplot(toscanasd)

# NDVI (Normalised difference vegetation index)

emiliaNDVI <- ((emiliamask$emilia20m_B8A-emiliamask$emilia20m_B04)/(emiliamask$emilia20m_B8A+emiliamask$emilia20m_B04))
levelplot(emiliaNDVI)

toscanaNDVI <- ((toscanamask$tosc20m_B8A-toscanamask$tosc20m_B04)/(toscanamask$tosc20m_B8A+toscanamask$tosc20m_B04))
levelplot(toscanaNDVI)

# Senescing Vegetation normalised (vegetazione invecchiata o malata, poco produttiva di clorofilla)
# Banda 3 (green) riflette meno nella vegetazione vecchia o malata, per cui valori più alti di Senescing Vegetation indicano una pianta più vecchia.

emiliaSENVEG <- ((emiliamask$emilia20m_B8A-emiliamask$emilia20m_B03)/(emiliamask$emilia20m_B8A+emiliamask$emilia20m_B03))
levelplot(emiliaSENVEG)

toscanaSENVEG <- ((toscanamask$tosc20m_B8A-toscanamask$tosc20m_B03)/(toscanamask$tosc20m_B8A+toscanamask$tosc20m_B03))
levelplot(toscanaSENVEG)

# differenza vegetazione, considerando il dato NDVI come "contenuto di vegetazione in generale", si può stimare quanta è giovane e sana
# facendo la differenza fra NDVI e Senescing Vegetation, così da ottenere un risultato in cui valori bassi, ma positivi, indicano una vegetazione meno 
# "verde" e quindi più vecchia, valori alti, indicano una vegetazione sana.
# Valori negativi sono attribuibili a zone non vegetate (città, campi terreni).

emiliadif_veg <- (emiliaNDVI - emiliaSENVEG)
plot(emiliadif_veg)

toscanadif_veg <- (toscanaNDVI - toscanaSENVEG)
levelplot(toscanadif_veg)

# Water Bodies normalised

emiliaW <- ((emiliamask$emilia20m_B02-emiliamask$emilia20m_B11)/(emiliamask$emilia20m_B02+emiliamask$emilia20m_B11))
plot(emiliaW)

toscanaW <- ((toscanamask$tosc20m_B02-toscanamask$tosc20m_B11)/(toscanamask$tosc20m_B02+toscanamask$tosc20m_B11))
plot(toscanaW)

# CLASSIFICAZIONE 
set.seed(1)
emilia_cla <- unsuperClass(emiliamask, nClasses = 4)
plot(emilia_cla$map)
set.seed(1)
toscana_cla <- unsuperClass(toscanamask, nClasses = 4)
plot(toscana_cla$map)
