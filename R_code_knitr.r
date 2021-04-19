#R_code_knitr
#Codice che utilizza il pacchetto knitr per creare un report dei risultati

# installo il pacchetto knitr per poter pescare il codice (file testo) esterno 
install.packages("knitr")
library(knitr)

setwd("C:/lab/Greenland")

#Funzione per generare il report: stitch
# servono 3 argomenti:
# 1-nome file di testo
# 2-template utilizzato, nel nostrocaso misc
# 3-pacchetto utilizzato, knitr

stitch("C:/lab/Greenland/R_code_greenland.txt", template=system.file("misc", "knitr-template.Rnw", package="knitr"))

