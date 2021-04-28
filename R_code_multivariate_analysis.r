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

#ANALISI PCA
# Siccome l'analisi pixel per pixel, conitene molte operazioni e tempo
# Quindi meglio prima ridimensionare i pixel aggregandoli, quindi diminuendo la risoluzione.
# passaggio da pixel piccoli a più grandi con un fattore di x10 per lato di pixel.
p224r63_2011res <- aggregate(p224r63_2011, fact=10)

# Verifica che abbia funzionato (abbassamento risoluzione) 
# Plottando comparando le due immagini prima e dopo aggregate.
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="lin") # Dettaglio pixel 30x30 m
plotRGB(p224r63_2011res, r=4, g=3, b=2, stretch="lin") # Dettaglio pixel 300x300 m

# Effettuare l'analisi PCA (principal component analisys)
# ossia trovare una immagine con MAPPA, MODELLO, chiamata  e altre info generate con rasterPCA.
# Si creano i principal component ossia le bande, dove la prima contiene la maggiore variabilità.
p224r63_2011res_pca <- rasterPCA(p224r63_2011res)
# Per vedere la variabilità spiegata delle singole bande appena create si usa un summary:
summary(p224r63_2011res_pca$model)
# la prima banda spiega il 99%, ma per spiegare il 100% ovviamente bisogna avere un numero 
# di bande (principal component) che sia uguale al numero di bande iniziali quindi 7.
plot(p224r63_2011res_pca$map) # mappe diverse per componente con massimi valori nella prima e poi a scalare
p224r63_2011res_pca$call # codice/riga che ha generato l'immagine 
plotRGB(p224r63_2011res_pca$map, r=4,g=3,b=2,stretch="lin")
