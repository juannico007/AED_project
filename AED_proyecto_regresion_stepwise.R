library(readxl)
idh <- read_excel("Documents/idh.xlsx")
View(idh)    

library(tidyverse)
library(caret)
library(leaps)

#Regresión backwards
Data = idh[2:8]

#modelo unicamente con el intercepto
intercept = lm(Data$IDH ~ 1)

#Modelo con todas las predictoras
full = lm(Data$IDH ~ ., data=Data)
summary(full)

#Regresion backward stepwise 
back = step(full, direction='backward', scope=formula(full), trace=0)
summary(back)

new = list(`esperanza de vida`=67.11, `Tasa Natalidad` =21, `PIB per capita`=3414.94, `Indice Primaria`=65.58)
predict(back, newdata = new, interval = "prediction")

#Vemos como el modelo que mejor se adata y explica la relacion de los datos excluye a las variables
#de indice de fecundidad y tasa de mortalidad. 
#Note como en este modelo todas las variables resultan ser significativas
