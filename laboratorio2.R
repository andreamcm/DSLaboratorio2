# Universidad del Valle de Guatemala
# Ivette Cardona - 16020
# Andrea María Cordón - 16076




# Set del directorio donde estan los datos
# -----------------------------------------
# Andrea
setwd("~/2019/UVG/Segundo Semestre/DataScience/Laboratorios/Laboratorio2/DSLaboratorio2")
# Ivette
setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/Laboratorio1DataScience")



# --------------------
# Librerias a utilizar
# --------------------
library(class)
library(caret)



# --------------------
# Lectura de los datos
# --------------------
datos <- read.csv("train.csv")
datos_relevantes <- datos[, c("LotArea", "YearRemodAdd", "YearBuilt", "GarageCars", "PoolArea", "YrSold", "SalePrice")]



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











