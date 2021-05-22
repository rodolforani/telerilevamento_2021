#R_code_variability.r

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
