# R_code_land_cover.r

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
