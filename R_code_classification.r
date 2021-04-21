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

set.seed(3)
# classificazione non supervisionata dall'utente, ossia il software effettua il training set da solo basandosi sul numero di classi scelto da noi.
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
