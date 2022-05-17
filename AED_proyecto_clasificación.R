library("readxl")
library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)
library(tidyverse)
require(ggplot2)
require(scales)
require(gridExtra)

idh <- read_excel("./Documents/idh.xlsx")
idh <- idh %>% remove_rownames() %>% column_to_rownames(var = 'País')
View(idh)

#Datos
Data = idh[1:7]

#Se escalan las variables para lograr la igualdad de varianzas
Data= as.data.frame(scale(Data))

CData = cbind(Data, Continente = idh$Continente, Tipo = idh$Tipo)

#Las varianzas son las mismas y son 1
apply(CData, 2, sd)

#Se escogen de forma aleatoria grupos de entrenamiento y prueba para nuestro modelo
sample <- sample(c(TRUE, FALSE), nrow(CData), replace=TRUE, prob=c(0.8,0.2))
train <- as.data.frame(CData[sample, ])
test <- as.data.frame(CData[!sample, ])

#Definicion del modelo con las de entrenamiento
model <- lda(train[1:7],grouping =train[,8] , data=train)

#output del modello
model

predicted_con <- predict(model, newdata = test[1:7])

predicted_con$class


CData = cbind(Data, Tipo = idh$Tipo)


#Se escogen de forma aleatoria grupos de entrenamiento y prueba para nuestro modelo

model2 <- lda(train[1:7],grouping =train[,9] , data=train)

predicted_des <- predict(model2, newdata = test[1:7])

predicted_des$class


#MODELO CON  MENOS VARIABLES

Data2 = Data[,-3]
Data2 = Data2[,-4]
Data2 =cbind(Data2, Tipo = idh$Tipo)


#Se escogen de forma aleatoria grupos de entrenamiento y prueba para nuestro modelo
train2 <- as.data.frame(Data2[sample, ])
test2 <- as.data.frame(Data2[!sample, ])


modeld2 <- lda(train2[-6],grouping =train2[,6] , data=train2)

#output del modelo 
modeld2

predicted_d2 <- predict(modeld2, newdata = test2[-6])

predicted_d2$class


#datos a plotear
lda_plot <- cbind(train2, predict(modeld2)$x)

#Plot
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = Tipo))

mean(predicted_d2$class==test2$Tipo)


#Clasificaion de Djibouti
dtest = idh[1:7]
dtest = dtest[-3]
dtest = dtest[-4]

newd = c(0.524, 67.11, 21,3414.94,65.58)

dtest<- rbind(dtest, newd)

dtest<-as.data.frame(scale(dtest))

scaled.new <- scale(newd, center = colMeans(Data2[-6]), scale = cov(Data2[-6]))

dj = dtest[125,]

predicted_dj <- predict(modeld2, newdata = dj)
predicted_dj$class
