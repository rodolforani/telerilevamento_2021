Inizio codice per esame

install.packages("ncdt4")
library(raster)
library(rasterdiv)
library(rasterVis)
library(ncdf4)

setwd("c:/lab/Esame")

neve12_05_2021 <- raster("c_gls_SCE500_202105120000_CEURO_MODIS_V1.0.1.nc")
neve12_05_2021
neve12_05_2021ok <- reclassify(neve12_05_2021, cbind(0:20,NA))
plot(neve12_05_2021)
