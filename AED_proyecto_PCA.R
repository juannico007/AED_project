library("readxl")
library(factoextra)
library(tidyverse)
library("corrplot")
idh <- read_excel("Documents/idh.xlsx")
idh <- idh %>% remove_rownames() %>% column_to_rownames(var = 'País')
View(idh)

#PCA
Data = idh[1:7]
head(Data[, 1:7])

#Valores y vecotres propios
S = cov(Data)
S = cov2cor(S)
cov_eigen <- eigen(S)
vecs = cov_eigen$vectors
vals = cov_eigen$values

#Estandarizamos debido a la diferencia en las escalas y unidades de variables
#Sino, toda la variabilidad recaeria sobre el PIB
PCA <-princomp(scale(Data))
summary(PCA)
var <- get_pca_var(PCA)
cont = var$contrib
view(cont)

#La variable con mayor peso en la primera componente principal es el IDH
#Variable con mayor peso en la 2 y en la 3 es Tasa de mortalidad

#Con 3 componentes principales explicamos un 81% de la variabilidad

#Plots de correlación de las variables con cada componente principal junto con su contribución a las mismas
fviz_pca_var(PCA, axes = c(1,2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)+labs(title ="Correlación de variables con componentes principales", x = "PC2", y = "PC1")
fviz_pca_var(PCA, axes = c(1,3), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)+labs(title ="Correlación de variables con componentes principales", x = "PC3", y = "PC1")
fviz_pca_var(PCA, axes = c(2,3), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)+labs(title ="Correlación de variables con componentes principales", x = "PC3", y = "PC2")

#DE LA PRIMERA GRAFICA
#Sobre la primera componente podemos ver que el IDH, la tasa de natalidad y el índice de fecundidad tienen una magnitud
#De correlación similar para la primera componente principal
#Mientras que para la segunda componente principal la que mayor correlacion tiene es la tasa de mortalidad
#Tambien podemos ver que la tasa de natalidad y el índice de fecundidad estan inversamente correlacionados con la 
#Tasa de mortalidad
#El IDH, l indice de finalización de primaria, el PIB per cápita y la esperanza de vida estan positivamente correlacionadas
#DE LA SGUNDA GRAFICA
#Sobre la tercera componente la variable con mayor correlación es la tasa de mortalidad
#La tasa de natalidad y el índice de fecundidad están inversamente correlacionados con la esperanza de vida

corrplot(var$contrib, is.corr=FALSE)

fviz_pca_ind(PCA, axes = c(1,2), geom.ind = "point", col.ind = idh$Tipo, palette = c("#00AFBB", "#E7B800", "#FC4E07"), addEllipses = TRUE, legend.title = "Grupos")
fviz_pca_ind(PCA, axes = c(1,3), geom.ind = "point", col.ind = idh$Tipo, palette = c("#00AFBB", "#E7B800", "#FC4E07"), addEllipses = TRUE, legend.title = "Groups")
fviz_pca_ind(PCA, axes = c(2,3), geom.ind = "point", col.ind = idh$Tipo, palette = c("#00AFBB", "#E7B800", "#FC4E07"), addEllipses = TRUE, legend.title = "Groups") 

fviz_pca_ind(PCA, axes = c(1,2), geom.ind = "point", col.ind = idh$Continente, palette = c("#00AFBB", "#E7B800", "#FC4E07","#FB4EE7", "#AC4ED7", "#FC8E0F" ), addEllipses = TRUE, legend.title = "Grupos")
