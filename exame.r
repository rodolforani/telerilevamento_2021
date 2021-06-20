# Prova esame influenza catena appenninica e suolo -->> Toscana vs Emilia dati Sentinel 2

library(raster)
library(rasterVis)
library(RStoolbox)
library(ggplot2) 
library(gridExtra)
library(viridis)
library(ncdf4)

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
toscana2021 <- stack(importtoscana)
toscana2021

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
emiliamask <- mask(emilia2021, emiliaNA, maskvalue=TRUE)
ggRGB(emiliamask, 4, 3, 2, stretch="Lin") # colori naturali

toscanaNA <- toscana2021$tosc20m_B04 > 3000
toscanamask <- mask(toscana2021, toscanaNA, maskvalue=TRUE)
ggRGB(toscanamask,4, 3, 2, stretch="Lin") # colori naturali

# Analisi multivariata (PCA)

emiliarid <- aggregate(emiliamask, fact=2) # Aggrego i pixel di un fattore 2, quindi diventeranno pixel di 40x40 metri, per velocizzare i calcoli.
emiliarid
plotRGB(emiliarid, 4, 3, 2, stretch="Lin")
emiliapca <- rasterPCA(emiliarid)
emiliapca #informazioni
summary(emiliapca$model)
# pc1=75.6%, pc2=20.8%, pc3=0.016%, pc4=0.014%, ... , pc13

toscanarid <- aggregate(toscanamask, fact=2)
toscanarid
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
cl1 <- colorRampPalette(c("white","pink","orange","red","purple", "blue", "black"))(200)
par(mfrow=c(1,2))
plot(emiliasd, col=cl1, ylab = "latitudine", xlab = "longitudine", main = "Dev. St. Emilia")
plot(toscanasd, col=cl1, ylab = "latitudine", xlab = "longitudine", main = "Dev. St. Toscana")
#levelplot(emiliasd, main=list("Dev. St. Emilia"))
#levelplot(toscanasd, main=list("Dev. St. Toscana"))

# Maschero i corpi d'acqua perchè anche riflettono nel verde e alterano la mappa della senescing vegetation
# Water Bodies normalised

emiliaW <- ((emiliamask$emilia20m_B02-emiliamask$emilia20m_B11)/(emiliamask$emilia20m_B02+emiliamask$emilia20m_B11))
toscanaW <- ((toscanamask$tosc20m_B02-toscanamask$tosc20m_B11)/(toscanamask$tosc20m_B02+toscanamask$tosc20m_B11))

emiliaWviridis <- ggplot() + 
  geom_raster(emiliaW, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() + 
  ggtitle("Corpi d'acqua emilia by viridis colour scale") 
toscanaWviridis <- ggplot() +
  geom_raster(toscanaW, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() + 
  ggtitle("Corpi d'acqua toscana by viridis colour scale")
grid.arrange(emiliaWviridis, toscanaWviridis, nrow=1, ncol=2)

emiliaWNA <- emiliaW > 0.3 # valore limite sopra il quale la mappa figura un corpo d'acqua (verificando con la figura a colori naturali)
plot(emiliaWNA)
emiliaWmask <- mask(emiliamask, emiliaWNA, maskvalue=TRUE)

toscanaWNA <- toscanaW > 0.3
plot(toscanaWNA)
toscanaWmask <- mask(toscanamask, toscanaWNA, maskvalue=TRUE)
plot(toscanaWmask)

# NDVI (Normalised difference vegetation index)

emiliaNDVI <- ((emiliaWmask$emilia20m_B8A-emiliaWmask$emilia20m_B04)/(emiliaWmask$emilia20m_B8A+emiliaWmask$emilia20m_B04))
toscanaNDVI <- ((toscanaWmask$tosc20m_B8A-toscanaWmask$tosc20m_B04)/(toscanaWmask$tosc20m_B8A+toscanaWmask$tosc20m_B04))

emiliaNDVIviridis <- ggplot() + #crea una nuova finestra vuota
  geom_raster(emiliaNDVI, mapping = aes(x=x, y=y, fill=layer)) + # crea la geometria a griglia di pixel(raster) e la mappa con le aesthetics inserite da noi
  scale_fill_viridis() + #Utilizza la legenda (color palette) già preparata, di default utilizza quella "viridis".
  ggtitle("NDVI emilia by viridis colour scale") #Titolo immagine
toscanaNDVIviridis <- ggplot() + 
  geom_raster(toscanaNDVI, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() + 
  ggtitle("NDVI toscana by viridis colour scale")
grid.arrange(emiliaNDVIviridis, toscanaNDVIviridis, nrow=1, ncol=2)

emiliaNDVIsd <- focal(emiliaNDVI, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
toscanaNDVIsd <- focal(toscanaNDVI, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
par(mfrow=c(1,2))
plot(emiliaNDVIsd, col=cl1, main="Variabilità NDVI Emilia", ylab="Latitudine", xlab="Longitudine")
plot(toscanaNDVIsd, col=cl1, main="Variabilità NDVI Toscana", ylab="Latitudine", xlab="Longitudine")

# Tolgo la parte di mappa con valori di NDVI inferiori a 0.8 così da ottenere solo la parte più vegetata

emiliaNDVI [emiliaNDVI < +0.8] <- NA
toscanaNDVI [toscanaNDVI < +0.8] <- NA
plot(emiliaNDVI)
plot(toscanaNDVI)

# Senescing Vegetation normalised (vegetazione invecchiata o malata, poco produttiva di clorofilla)
# Banda 3 (green) riflette meno nella vegetazione vecchia o malata, per cui valori più alti di Senescing Vegetation indicano una pianta più vecchia.

emiliaSENVEG <- ((emiliaWmask$emilia20m_B8A-emiliaWmask$emilia20m_B03)/(emiliaWmask$emilia20m_B8A+emiliaWmask$emilia20m_B03))
toscanaSENVEG <- ((toscanaWmask$tosc20m_B8A-toscanaWmask$tosc20m_B03)/(toscanaWmask$tosc20m_B8A+toscanaWmask$tosc20m_B03))
emiliaSENVEG [emiliaSENVEG < +0.7] <- NA
toscanaSENVEG [toscanaSENVEG < +0.7] <- NA
emiliaSENVEG [emiliaSENVEG > 0.85] <- NA
toscanaSENVEG [toscanaSENVEG > 0.85] <- NA  # Esprimo solo la vegetazione vera e propria (compreso anche campi verdi data la risoluzione di 20x20m)

emiliaSENVEGviridis <- ggplot() +                                      
  geom_raster(emiliaSENVEG, mapping = aes(x=x, y=y, fill=layer)) +   
  scale_fill_viridis() + 
  ggtitle("Senescing vegetation emilia by viridis colour scale") 
toscanaSENVEGviridis <- ggplot() +
  geom_raster(toscanaSENVEG, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() + 
  ggtitle("Senesscing vegetation toscana by viridis colour scale")
grid.arrange(emiliaSENVEGviridis, toscanaSENVEGviridis, nrow=1, ncol=2)

# differenza vegetazione, considerando il dato NDVI come "contenuto di vegetazione in generale", si può stimare quanta è giovane e sana
# facendo la differenza fra NDVI e Senescing Vegetation, così da ottenere un risultato in cui valori bassi, ma positivi, indicano una vegetazione meno 
# "verde" e quindi più vecchia. Valori alti, indicano una vegetazione sana.
# Valori negativi sono attribuibili a zone non vegetate (città, campi terreni).

emiliadif_veg <- (emiliaNDVI - emiliaSENVEG)
toscanadif_veg <- (toscanaNDVI - toscanaSENVEG)
emiliadif_veg [emiliadif_veg < 0] <- NA # associo un limite inferiore ad entrambi sapendo che comunque la vegetazione ha un NDVI (banda NIR-banda red) 
                                        # sempre superiore al senscing indix (banda NIR-banda green), per cui tutti i valori devono essere positivi.
emiliaDIFviridis <- ggplot() + 
  geom_raster(emiliadif_veg, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() + 
  ggtitle("Vegetazione sana emilia by viridis colour scale") 
toscanaDIFviridis <- ggplot() +
  geom_raster(toscanadif_veg, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis() + 
  ggtitle("Vegetazione sana toscana by viridis colour scale")
grid.arrange(emiliaDIFviridis, toscanaDIFviridis, nrow=1, ncol=2)

# RISULTATO: LA VEGETAZIONE TOSCANA è GENERALMENTE PIù SANA E IN ENTRAMBI I CASI PIù SANA SUGLI APPENNINI.

# Vapore Acqueo WVP 

par(mfrow=c(1,2))
plot(emiliamask$emilia20m_WVP)
plot(toscanamask$tosc20m_WVP)

# CLASSIFICAZIONE USU DEL SUOLO

set.seed(4)
emilia_cla <- unsuperClass(emiliamask, nClasses = 3)
set.seed(2)
toscana_cla <- unsuperClass(toscanamask, nClasses = 3)

par(mfrow=c(1,2))
plot(emilia_cla$map, ylab = "latitudine", xlab = "longitudine", main = "Emilia")
plot(toscana_cla$map, ylab = "latitudine", xlab = "longitudine", main = "Toscana")

# Distribuzione dei pixel nelle varie classi: frequenza prima immagine emilia:
femilia <- freq(emilia_cla$map)
femilia
# classe 1 mare/acque/ombre = 3691070 pixel
# classe 2 Antropico (campi arati/città) = 10145473 pixel
# Classe 3 Vegetazione (bosco/prato/campi verdi) = 15296609 pixel
# Classe 4 NA (nuvole mascherate) = 1006948 pixel 
terrenoemilia  <- 10145473 + 15296609 #Pixel di suolo di terra classe 2 + classe3
#proporzione di pixel nell'immagine facendo la divisione del risultato della function freq e i pixel terreno
propemilia <- femilia/terrenoemilia
propemilia # 60.12% Vegetazione 39.87 % Antropizzato

# Frequenza toscana
ftoscana <- freq(toscana_cla$map)
ftoscana
# classe 1 mare/acque/ombre = 2253279 pixel
# classe 2 Antropico (campi arati/città) =  6472019 pixel
# Classe 3 Vegetazione (bosco/prato/campi verdi) = 20114553 pixel
# Classe 4 NA (nuvole mascherate) = 1300249 pixel 
terrenotoscana <- 6472019 + 20114553
proptoscana <- ftoscana/terrenotoscana
proptoscana # 75.66 % Vegetazione 24.34 % Antropizzato

# Creazione di un data frame per visualizzare i dati di percentuale di copertura del suolo
# Si utilizza la funzipone data.frame(nomi colonne)
# Creo le colonne a cui associo i componenti:
Cover <- c("Vegetato","Antropizzato") 
Percentuale_terreno_coperto <- c(60.12,39.87)
Percentuale_terreno_coperto_<- c(75.66,24.34)
# Creo il data set a tabella:
percentages <- data.frame(Cover,Percentuale_E,Percentuale_T)
percentages

col <- c("blue","green")

# Grafichiamo il dataset utilizzando ggplot specificando le caratterisctiche estetiche
ggp1 <- ggplot(percentages, aes(x=Cover,y=Percentuale_terreno_coperto, color=Cover)) + 
  geom_bar(stat = "identity", fill="white") +
  ggtitle("Emilia")
# aes: inserimento degli assi da cui prednere i dati, color=Cover costruisce la legenda in base al colore
# + geom_bar:g geometria del grafico a barre, stat="identity": i dati li prende così come sono grezzi, fill="white": colore riempimento barre
ggp2 <- ggplot(percentages, aes(x=Cover,y=Percentuale_terreno_coperto_, color=Cover)) + 
  geom_bar(stat = "identity",fill = "white") +
  ggtitle("Toscana")

#Visualizziamo insieme i due grafici con grid.arrange
grid.arrange(ggp1, ggp2, nrow=1)

