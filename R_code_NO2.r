R_code_NO2.r

#Librerie necessarie:
library(raster) # per importare una singola banda con la funzione rester
library(RStoolbox)
library()
# impostiamo la worcking directory sulla cartella EN
setwd("C:/lab/EN")

#Importiamo una singola immagine di gennaio(1) per velocizzare i colacoli successivi, potrei fare anche brick essendo più immagini.
#essendo un elemento esterno ad R, bisogna inserirlo fra virgolette. 
EN01 <- raster("EN_0001.png")

# plot della immmagine con una scala colori inventata(funzione colorRampPalette)
col1 <- colorRampPalette(c("black","blue","light blue","yellow","orange","red")) (100)
plot(EN01, col=col1)

#Importiamo l'ultima immagine temporale di marzo (13)
EN13 <- raster("EN_0013.png")
plot(EN13, col=col1)

#Differenza fra prima e dopo di contenuto di NO2 nell'area:
ENdif <- EN01- EN13
plot(ENdif, col=col1)

#Plot di tutte le immagini insieme:
par(mfrow=c(1,3))
plot(EN01, col=col1, main="NO2 in january")
plot(EN13, col=col1, main="NO2 in march")
plot(ENdif, col=col1, main="Difference of NO2 january-march")

# 7. Import the whole set

# list of files:
rlist <- list.files(pattern="EN")
rlist

import <- lapply(rlist,raster)
import

EN <- stack(import)
plot(EN, col=col1)

# 8. Replicate the plot of images 1 and 13 using the stack
par(mfrow=c(1,2))
plot(EN$EN_0001, col=col1)
plot(EN$EN_0013, col=col1)

# 9. Compute a PCA over the 13 images, prima bigona richiamare il pacchetto Rstoolbox per la funzione PCA

ENpca <- rasterPCA(EN)

summary(ENpca$model)

plotRGB(ENpca$map, r=1, g=2, b=3, stretch="lin")

# 10. Compute the local variability (local standard deviation) of the first PC
PC1sd <- focal(ENpca$map$PC1, w=matrix(1/9, nrow=3, ncol=3), fun=sd)
plot(PC1sd, col=col1)
