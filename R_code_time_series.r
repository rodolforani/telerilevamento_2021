Time series analysis of Greenland increase temperature
Data and code from Emanuela Cosma

#installiamo e attiviamo i pacchetti che ci servono (raster e rasterVis):
#install.packages("raster")
install.packages("rasterVis")
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

