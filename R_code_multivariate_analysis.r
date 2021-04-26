# R_code_multivariate_analysis.r

#librerie necessarie:
library(raster)
library(RStoolbox)
# Fisso la cartella di lavoro
setwd("C:/lab/")

# Funzione brick per caricare un set di dati:
p224r63_2011 <- brick("p224r63_2011_masked.grd")
p224r63_2011 #info immagine

# Grafichiamo i valori della banda 1 contro i valori della banda 2,
# leghiamo il nome della banda alla immagine dove sono contenuti con il dollaro,
# personaliziamo con colore, forma segnale punta, dimensioni segnale.
plot(p224r63_2011$B1_sre,p224r63_2011$B2_sre,col="red",pch=19, cex=2)

#Mettiamo in correlazione a due a due tutte le bande(variabili) contenute nell'immagine
pairs(p224r63_2011)
