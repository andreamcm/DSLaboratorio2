# Universidad del Valle de Guatemala
# Ivette Cardona - 16020
# Andrea María Cordón - 16076

# Set del directorio donde estan los datos
# Andrea
setwd("~/2019/UVG/Segundo Semestre/DataScience/Laboratorios/Laboratorio2/DSLaboratorio2")
# Ivette
setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/DSLaboratorio2")

# Librerias a utilizar
library(dplyr)
library(ggplot2) 
library(gmodels)
library(Hmisc)
library(ggthemes)
library(class)
library(caret)


# Lectura de los datos
# --------------------
datos <- read.csv("train.csv")
datos_relevantes <- datos[, c("LotFrontage", "YearRemodAdd", "YearBuilt", "GarageCars", "PoolArea", "YrSold", "SalePrice")]

#Analisis exploratorio con nuevas variables
summary(datos_relevantes)

#LotFrontage
hist(datos_relevantes$LotFrontage, col="lightcyan", main="Histograma de LotFrontage")
qqnorm(datos_relevantes$LotFrontage, main="LotFrontage")
qqline(datos_relevantes$LotFrontage, col = "red")
ks.test(datos_relevantes$LotFrontage,rnorm(length(datos_relevantes$LotFrontage)))

#YearRemoAdd
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


# Separación de datos en entrenamiento y prueba
# ----------------------------------------------
set.seed(123)

porciento <- 60/100 #Porciento en el que se partirÃ¡n los datos
muestra<-sample(1:nrow(datos_relevantes),porciento*nrow(datos_relevantes))#Muestra aleatoria de numeros de un vector

trainSet<-datos_relevantes[muestra,] #Obtengo las filas de los elementos que estan en el sector de muestra
testSet<-datos_relevantes[-muestra,] #Obtengo las filas de los elementos que no estÃ¡n en el vector de muestra


# Regresión lineal
# ----------------

