# Universidad del Valle de Guatemala
# Ivette Cardona - 16020
# Andrea Mar�a Cord�n - 16076




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
datos_test <- read.csv("sample_submission.csv")
#datos_relevantes <- na.omit(datos_relevantes)

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
# Separaci�n de datos en entrenamiento y prueba
# ----------------------------------------------
set.seed(123)

porciento <- 60/100 # 60% para training y 40% para pruebas
muestra<-sample(1:nrow(datos_relevantes),porciento*nrow(datos_relevantes))# Seleccion de muestras aleatorias

trainSet<-datos_relevantes[muestra,] # Grupo de entrenamiento
testSet<-datos_relevantes[-muestra,] # Grupo de pruebas
testKaggle <- testSet



# Regresi�n lineal
# ----------------
modeloRL <- lm(SalePrice~., data = trainSet)
summary(modeloRL)

prediccionRL <- predict(modeloRL, newdata = testSet)
testSet$rlprediccion <- prediccionRL
prediccionRL

diferencia <- abs(testSet$rlprediccion-testSet$SalePrice)
diferencia


cfmrl <- confusionMatrix(prediccionRL, testSet)

# Con los datos de kaggle
complete_train <- datos_relevantes
completeModeloRL <- lm(SalePrice~., data = complete_train)
summary(completeModeloRL)

prediccionRL <- predict(modeloRL, newdata = testKaggle)
testKaggle$rlprediccion <- prediccionRL

diferencia_kaggle <- abs(testKaggle$rlprediccion-testKaggle$SalePrice)
diferencia_kaggle

cfmrl <- confusionMatrix(testKaggle$prediccionRL, testKaggle$SalePrice)
testKaggle$rlprediccion
count(testKaggle$rlprediccion)
testKaggle$SalePrice
count(testKaggle$SalePrice)

# KNN
# ---
#Generaci�n del modelo
modeloLinealSimple<-lm(mpg~wt, data = trainSet)
summary(modeloLinealSimple)

#predicci�n
prediccion<-predict(modeloLinealSimple,newdata = testSet[,2:ncol(testSet)])
# Se agrega la predicci�n al conjunto de entrenamiento
testSet$mpgPred<-prediccion
#Ver la diferencia entre lo real y lo predicho
dif<-abs(testSet$mpgPred-testSet$mpg)

#Hay que establecer la diferencia m�nima para saber quienes son los mejores clasificados

testSet$mpgPred <-NULL

############
#Variable limite 
############
testSet$compar <- cut(testSet$SalePrice, breaks = c(-Inf, 129975, 214000, Inf), 
              labels = c(1, 2, 3))

testSet

trainSet$compar <- cut(trainSet$SalePrice, breaks = c(-Inf, 129975, 214000, Inf), 
                      labels = c(1, 2, 3))

trainSet
############
#KNN
############

#Caret

#29
trainSet
predKnn<-knn(trainSet,testSet,as.factor(trainSet$compar), k = 29)

cfm<-confusionMatrix(as.factor(testSet$compar),predKnn)
cfm


#30
predKnn<-knn(trainSet,testSet,as.factor(trainSet$compar), k = 30)

cfm<-confusionMatrix(as.factor(testSet$compar),predKnn)
cfm


#Con caret usando validaci�n cruzada
set.seed(123)
trctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 29)

trainSet$compar<-as.factor(trainSet$compar)
testSet$compar<-as.factor(testSet$compar)

knnTrain <- train(compar ~., data = trainSet, method = "knn",
                  trControl=trctrl,
                  preProcess = c("center", "scale"), tuneLength=10)
predknn3<-predict(knnTrain,newdata = testSet)
summary(knnTrain)
cfm<-confusionMatrix(as.factor(testSet$compar),predKnn3)
cfm








