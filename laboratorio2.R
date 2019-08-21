# Universidad del Valle de Guatemala
# Ivette Cardona - 16020
# Andrea María Cordón - 16076

# Set del directorio donde estan los datos
# Andrea
setwd("~/2019/UVG/Segundo Semestre/DataScience/Laboratorios/Laboratorio2/DSLaboratorio2")
# Ivette
setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/Laboratorio1DataScience")

# Librerias a utilizar
library(class)
library(caret)


# Lectura de los datos
# --------------------
datos <- read.csv("train.csv")
datos_relevantes <- datos[, c("LotFrontage", "YearRemodAdd", "YearBuilt", "GarageCars", "PoolArea", "YrSold", "SalePrice")]


# Separación de datos en entrenamiento y prueba
# ----------------------------------------------
set.seed(123)

porciento <- 60/100 #Porciento en el que se partirÃ¡n los datos
muestra<-sample(1:nrow(datos_relevantes),porciento*nrow(datos_relevantes))#Muestra aleatoria de numeros de un vector

trainSet<-datos_relevantes[muestra,] #Obtengo las filas de los elementos que estan en el sector de muestra
testSet<-datos_relevantes[-muestra,] #Obtengo las filas de los elementos que no estÃ¡n en el vector de muestra


# Regresión lineal
# ----------------

