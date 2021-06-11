# R code classification 

library(raster)
install.packages("RStoolbox")
library(RStoolbox)

# settare la working directory 
setwd("C:/lab/")

#importo dati esterni con funzione brick e salvo immagine in "so"
so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
#informazione immagine:
so
#visualizzazione livelli con schema RGB colori naturali utilizzando tutti i pixel con stretch lineare
plotRGB(so, 1,2,3, stretch="lin")

# classificazione non supervisionata dall'utente, ossia il software effettua la classificazione da solo, basandosi sul numero di classi scelto da noi.
# Il software, partendo da un numero random di pixel di partenza a cui associa una classe, contiunua poi ad associare gli altri pixel alle classi da lui scelte
# in base alla somiglianza con la riflettanza nei pixel di partenza. 
# Per associare un pixel ad una certa classe, che ha certi valori basati sui pixel di settaggio iniziali, utilizza una funzione di massima verosomiglianza.
soc3 <- unsuperClass(so, nClasses = 3)
# visualizzare la mappa creata ma esplicitando di plottare la parte "map" di soc,
# perchè in soc sono contenute più informazione; mappa e il modello da cui si è costruito le classi
# quindi esplicitare di plottare la mappa legando con il dollaro.
plot(soc3$map)
#cassificazione non supervisionata con 20 classi
soc20 <- unsuperClass(so, nClasses = 20)
plot(soc20$map)

#Carico altra immagine

sun <- brick("sun.jpg")
plotRGB(sun, 1,2,3, stretch="lin")

sunc <- unsuperClass(sun, nClasses = 20)
plot(sunc$map)

# CLASSIFICAZIONE GRAND CANION
# carico l'immagine con brick
gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")
gc # info immogine
# graficare la foto in colori naturali 
plotRGB(gc, r=1,g=2,b=3, stretch="lin")
plotRGB(gc, r=1,g=2,b=3, stretch="hist")
# uno stretch differente produce colori diversi che sono utili per risaltare certe informazioni,
# lo stretch Histogram produce una mappa che sembrain 3D, si intuiscono meglio le morfologie.

#Classificazione non supervisionata in 2 classi:
gcc <- unsuperClass(gc, nClasses = 4)
plot(gcc$map)
# Visualizzo immagine a colori naturali con immagine classificata per conffronto diretto
par(mfrow=c(2,1))
plotRGB(gc, r=1,g=2,b=3, stretch="hist")
plot(gcc$map)
