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

