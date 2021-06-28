# R_code_spectral_signatures.r

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

