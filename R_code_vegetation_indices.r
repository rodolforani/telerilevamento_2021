# R_code_vegetation_indices.r

# serve il pacchetto raster
library(raster)

# impostare la cartella di lavoro
setwd("C:/lab/")

# Caricare la immagini
defor1 <- brick("defor1.jpg")
defor2 <- brick("defor2.jpg")

# b1 = NIR, b2 = red, b3 = green

# Visualizziamo le immagini con i colori con coi sono stati importati

par(mfrow=c(2,1))
plotRGB(defor1, r=1,b=2,g=3, stretch="lin")
plotRGB(defor2, r=1,b=2,g=3, stretch="lin")
# La zona è la stessa perchè 1. il fiume è uguale (anche se i fiumi possono cambiare nel tempo)
# 2. si vede una tessera di territorio al centro che rimane uguale
