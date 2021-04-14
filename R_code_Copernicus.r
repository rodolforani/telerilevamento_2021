# Codice per estrazioni dati da copernicus
# visualizing Coperniscus data

install.packages("raster")
library(raster)
# 
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
