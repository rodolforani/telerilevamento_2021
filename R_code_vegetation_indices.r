# R_code_vegetation_indices.r

# serve il pacchetto raster
library(raster)
library(RStoolbox)

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
defor1
# Difference Vegetation Index, differenza pixe per pixel 
#della banda NIR (alta riflettanza veg) - Banda red(bassa relf veg)
dvi1 <- defor1$defor1.1 - defor1$defor1.2
plot(dvi1)

#plottiamo con colori più indicati
cl<- colorRampPalette(c("darkblue","yellow","red","black"))(100)
plot(dvi1, col=cl)
#Stessa cosa per defor2 calcoliamo il dvi:
defor2
# Difference Vegetation Index, differenza pixe per pixel 
#della banda NIR (alta riflettanza veg) - Banda red(bassa relf veg)
dvi2 <- defor2$defor2.1 - defor2$defor2.2
plot(dvi2)

#plottiamo con colori più indicati
cl<- colorRampPalette(c("darkblue","yellow","red","black"))(100)
plot(dvi2, col=cl)

# Paragone pre e post:
par(mfrow=c(2,1))
plot(dvi1, col=cl, main="dvi at time 1")
plot(dvi2, col=cl, main="dvi at time 2")
#EVIDENTE CHE LA PARTE GIALLA è MOLTO MAGGIORE, RAPPRESENTANDO UNA RIDUZIONE DEL DVi, QUINDI UNA RIDUZIONE DELLA VEG SANA. 

#Differenza del dvi fra i due tempi delle immagine (pixel per pixel)
difdvi <- dvi1 - dvi2
cld <- colorRampPalette(c("blue","white","red"))(100)
plot(difdvi, col=cld)

# Per poter paragonare immagini a diversi bit bisogna normalizzare il dvi in ndvi
# ndvi = (nir-red)/(nir+red) così tutte le immagini hanno un range da -1 a 1
ndvi1 <- (defor1$defor1.1 - defor1$defor1.2)/(defor1$defor1.1 + defor1$defor1.2)
plot(ndvi1, col=cl)
# Per seconda immagine uguale:
ndvi2 <- (defor2$defor2.1 - defor2$defor2.2)/(defor2$defor2.1 + defor2$defor2.2)
plot(ndvi2, col=cl)
# Differenza di ndvi
difndvi = ndvi1 - ndvi2
plot(difndvi, col=cld)

# Possibile calcolare più indici simultaneamente compreso dvi e ndvi
# con la funzione "spectralIndices" all'interno del pacchetto RStoolbox
# Consulta il manuale su internet per utilizzarla
# Calcolo tutti gli indici considerando le bande tranne quella del blu:
vi1 <- spectralIndices(defor1, green = 3, red = 2, nir = 1)
plot(vi1, col=cl)
vi2 <- spectralIndices(defor2, green = 3, red = 2, nir = 1)
plot(vi1, col=cl)
