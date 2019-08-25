# Universidad del Valle de Guatemala
# Ivette Cardona - 16020
# Andrea María Cordón - 16076




# Set del directorio donde estan los datos
# -----------------------------------------
# Andrea
setwd("~/2019/UVG/Segundo Semestre/DataScience/Laboratorios/Laboratorio2/DSLaboratorio2")
# Ivette
setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/DSLaboratorio2")



# --------------------
# Librerias a utilizar
# --------------------

library(dplyr)
library(ggplot2)
library(corrplot)
library(gmodels)
library(Hmisc)
library(ggthemes)
library(class)
library(caret)



# --------------------
# Lectura de los datos
# --------------------
datos <- read.csv("train.csv")
datos_relevantes <- datos[, c("LotArea", "YearRemodAdd", "YearBuilt", "GarageCars", "PoolArea", "YrSold", "SalePrice")]
datos_relevantes <- na.omit(datos_relevantes)

#Analisis exploratorio con nuevas variables
summary(datos_relevantes)

#LotArea
hist(datos_relevantes$LotArea, col="lightcyan", main="Histograma de LotArea")
qqnorm(datos_relevantes$LotArea, main="LotArea")
qqline(datos_relevantes$LotArea, col = "red")
ks.test(datos_relevantes$LotArea,rnorm(length(datos_relevantes$LotArea)))

#YearBuilt
hist(datos_relevantes$YearBuilt, col="lightcyan", main="Histograma de YearBuilt")
qqnorm(datos_relevantes$YearBuilt, main="YearBuilt")
qqline(datos_relevantes$YearBuilt, col = "red")
ks.test(datos_relevantes$YearBuilt,rnorm(length(datos_relevantes$YearBuilt)))

#YearRemoAdd
hist(datos_relevantes$YearRemodAdd, col="lightcyan", main="Histograma de YearRemodAdd")
qqnorm(datos_relevantes$YearRemodAdd, main="YearRemodAdd")
qqline(datos_relevantes$YearRemodAdd, col = "red")
ks.test(datos_relevantes$YearRemodAdd,rnorm(length(datos_relevantes$YearRemodAdd)))

#GarageCars
hist(datos_relevantes$GarageCars, col="lightcyan", main="Histograma de GarageCars")
qqnorm(datos_relevantes$GarageCars, main="GarageCars")
qqline(datos_relevantes$GarageCars, col = "red")
ks.test(datos_relevantes$GarageCars,rnorm(length(datos_relevantes$GarageCars)))

#PoolArea
hist(datos_relevantes$PoolArea, col="lightcyan", main="Histograma de PoolArea")
qqnorm(datos_relevantes$PoolArea, main="PoolArea")
qqline(datos_relevantes$PoolArea, col = "red")
ks.test(datos_relevantes$PoolArea,rnorm(length(datos_relevantes$PoolArea)))

#YrSold
hist(datos_relevantes$YrSold, col="lightcyan", main="Histograma de YrSold")
qqnorm(datos_relevantes$YrSold, main="YrSold")
qqline(datos_relevantes$YrSold, col = "red")
ks.test(datos_relevantes$YrSold,rnorm(length(datos_relevantes$YrSold)))

#SalePrice
hist(datos_relevantes$SalePrice, col="lightcyan", main="Histograma de SalePrice")
qqnorm(datos_relevantes$SalePrice, main="SalePrice")
qqline(datos_relevantes$SalePrice, col = "red")
ks.test(datos_relevantes$SalePrice,rnorm(length(datos_relevantes$SalePrice)))

# Analisis de correlacion
corrcuant <- cor(datos_relevantes, use="complete.obs", method="pearson")
corrcuant
corrplot(corrcuant, method = "square")

res2<-rcorr(as.matrix(cuantitativas))

# ----------------------------------------------
# Separación de datos en entrenamiento y prueba
# ----------------------------------------------
set.seed(123)

porciento <- 60/100 # 60% para training y 40% para pruebas
muestra<-sample(1:nrow(datos_relevantes),porciento*nrow(datos_relevantes))# Seleccion de muestras aleatorias

trainSet<-datos_relevantes[muestra,] # Grupo de entrenamiento
testSet<-datos_relevantes[-muestra,] # Grupo de pruebas



# Regresión lineal
# ----------------
modeloRL <- lm(SalePrice~., data = trainSet)
summary(modeloRL)

prediccionRL <- predict(modeloRL, newdata = testSet)
testSet$rlprediccion <- prediccionRL
prediccionRL

diferencia <- abs(testSet$rlprediccion-testSet$SalePrice)
diferencia

cfmrl <- confusionMatrix(prediccionRL, testSet)


#Generación del modelo
modeloLinealSimple<-lm(mpg~wt, data = trainSet)
summary(modeloLinealSimple)

#predicción
prediccion<-predict(modeloLinealSimple,newdata = testSet[,2:ncol(testSet)])
# Se agrega la predicción al conjunto de entrenamiento
testSet$mpgPred<-prediccion
#Ver la diferencia entre lo real y lo predicho
dif<-abs(testSet$mpgPred-testSet$mpg)

#Hay que establecer la diferencia mínima para saber quienes son los mejores clasificados

testSet$mpgPred <-NULL

############
#KNN
############

#Now creating seperate dataframe for 'Creditability' feature which is our target.
trainSet.gc_labels <- trainSet[muestra,1]
testSet.gc_labels  <- testSet[-muestra,1]   

#trainSet.gc_labels <- na.omit(trainSet.gc_labels)

trainSet.gc_labels
NROW(trainSet.gc_labels)

knn.26 <-  knn(train=trainSet, test=testSet, cl=trainSet.gc_labels, k=29)
knn.27 <-  knn(train=trainSet, test=test.gc, cl=testSet.gc_labels, k=30)









