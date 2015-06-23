##################################################################################
# Trabajo 3
# Aprendizaje Automático
# Grado en Ingenierí­a Infomrática
# Granada, 31 de Mayo de 2015.
##################################################################################
# Datos del estudiante:
# Nombre: Fernández Bosch, Pedro
# NIF: 76422233-H
##################################################################################
# Ejecutar el script en R: source("ruta/fichero.R")

##################################################################################
## EJERCICIO 1
##################################################################################

# Cargar las librerias:
library(ISLR)
library(e1071)
library(ROCR)

# Cargar OJ:
data(OJ)
attach(OJ)

## EJERCICIO 1.1

# Información de OJ:
?OJ

readline(prompt="Presione [enter] para continuar")

# Crear un conjunto de entrenamiento conteniendo una muestra aleatoria de 800 observaciones, y un conjunto de test conteniendo el resto de las observaciones.
train = sample(nrow(OJ), 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

readline(prompt="Presione [enter] para continuar")

# Ajustar un clasificador SVM (con núcleo lineal) a los datos de entrenamiento usando cost=0.01, con "Purchase" como la respuesta y las otras variables como predictores. 
set.seed(1)
svmfit = svm(Purchase ~ ., data = OJ.train, kernel = "linear", cost = 0.01)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.2

# Usar la función summary() para producir un resumen estadístico, y describir los resultados obtenidos.
psummary<-summary(svmfit)
print(psummary)

readline(prompt="Presione [enter] para continuar")

# ¿Cuáles son las tasas de error de "training" y "test"?.
svmfit = svm(Purchase ~ ., data = OJ.train, kernel = "linear", cost = 0.01)
train.pred = predict(svmfit, OJ.train)
ptable<-table(OJ.train$Purchase, train.pred)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(train.pred!= OJ.train$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

test.pred = predict(svmfit, OJ.test)
ptable<-table(OJ.test$Purchase, test.pred)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(test.pred!= OJ.test$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.3

# Usar las función tune() para seleccionar un coste óptimo. Considerar los valores de "cost" del vector: [ 0.001, 0.01, 0.1, 1, 10].
set.seed (3)
tune.out=tune(svm ,Purchase~.,data=OJ.train ,kernel = "linear", ranges =list(cost=c(0.001 , 0.01, 0.1, 1,10) ))
psummary<-summary(tune.out)
print(psummary)

readline(prompt="Presione [enter] para continuar")

# Dibujar las curvas ROC para los diferentes valores del "cost".

rocplot = function(pred, truth, ...){
  predob = prediction(pred,truth)
  perf = performance(predob,"tpr","fpr")
  plot(perf,...)
}

# Para cost = 0,001
svmfit=svm(Purchase~., data=OJ.train, kernel ="linear", cost=0.001, decision.values =T)
fitted =attributes (predict (svmfit ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=0.001",col ="red ")

readline(prompt="Presione [enter] para continuar")

# Para cost = 0,01
svmfit=svm(Purchase~., data=OJ.train, kernel ="linear", cost=0.01, decision.values =T)
fitted =attributes (predict (svmfit ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=0.01",col ="red ")

readline(prompt="Presione [enter] para continuar")

# Para cost = 0,1
svmfit=svm(Purchase~., data=OJ.train, kernel ="linear", cost=0.1, decision.values =T)
fitted =attributes (predict (svmfit ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=0.1",col ="red ")

readline(prompt="Presione [enter] para continuar")

# Para cost = 1
svmfit=svm(Purchase~., data=OJ.train, kernel ="linear", cost=1, decision.values =T)
fitted =attributes (predict (svmfit ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=1",col ="red ")

readline(prompt="Presione [enter] para continuar")

# Para cost = 10
svmfit=svm(Purchase~., data=OJ.train, kernel ="linear", cost=10, decision.values =T)
fitted =attributes (predict (svmfit ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=10",col ="red ")

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.4

# Calcular las tasas de error de "training" y "test" usando el nuevo valor de coste óptimo. 
svmfit = svm(Purchase ~ ., data = OJ.train, kernel = "linear", cost = 0.1)
train.pred = predict(svmfit, OJ.train)
ptable<-table(OJ.train$Purchase, train.pred)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(train.pred!= OJ.train$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

test.pred = predict(svmfit, OJ.test)
ptable<-table(OJ.test$Purchase, test.pred)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(test.pred!= OJ.test$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.5

# Repetir apartados (2) a (4) usando un SVM con núcleo radial. Usar valores de gamma en el rango [10, 1, 0.1, 0.01, 0.001].
svmfit.radial=svm(Purchase~., data=OJ.train, kernel ="radial", gamma=c(10, 1, 0.1, 0.01, 0.001), cost=0.01, decision.values=T)
psummary<-summary(svmfit.radial)
print(psummary)

readline(prompt="Presione [enter] para continuar")

set.seed (3)
tune.out=tune(svm ,Purchase~.,data=OJ.train ,kernel = "radial",  gamma=c(10, 1, 0.1, 0.01, 0.001), ranges =list(cost=c(0.001, 0.01, 0.1, 1,10)))
psummary<-summary(tune.out)
print(psummary)

readline(prompt="Presione [enter] para continuar")

# Para cost = 0,001
svmfit.radial=svm(Purchase~., data=OJ.train, kernel ="radial", gamma=c(10, 1, 0.1, 0.01, 0.001), cost=0.001, decision.values=T)
fitted =attributes (predict (svmfit.radial ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=0.001",col ="red")

readline(prompt="Presione [enter] para continuar")

# Para cost = 0,01
svmfit.radial=svm(Purchase~., data=OJ.train, kernel ="radial", gamma=c(10, 1, 0.1, 0.01, 0.001), cost=0.01, decision.values=T)
fitted =attributes (predict (svmfit.radial ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=0.01",col ="red")

readline(prompt="Presione [enter] para continuar")

# Para cost = 0,1
svmfit.radial=svm(Purchase~., data=OJ.train, kernel ="radial", gamma=c(10, 1, 0.1, 0.01, 0.001), cost=0.1, decision.values=T)
fitted =attributes (predict (svmfit.radial ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=0.1",col ="red")

readline(prompt="Presione [enter] para continuar")

# Para cost = 1
svmfit.radial=svm(Purchase~., data=OJ.train, kernel ="radial", gamma=c(10, 1, 0.1, 0.01, 0.001), cost=1, decision.values=T)
fitted =attributes (predict (svmfit.radial ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=1",col ="red")

readline(prompt="Presione [enter] para continuar")

# Para cost = 10
svmfit.radial=svm(Purchase~., data=OJ.train, kernel ="radial", gamma=c(10, 1, 0.1, 0.01, 0.001), cost=10, decision.values=T)
fitted =attributes (predict (svmfit.radial ,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=10",col ="red")

readline(prompt="Presione [enter] para continuar")

svmfit.radial=svm(Purchase~., data=OJ.train, kernel ="radial", gamma=c(10, 1, 0.1, 0.01, 0.001), cost=10, decision.values=T)
train.pred = predict(svmfit.radial, OJ.train)
ptable<-table(OJ.train$Purchase, train.pred)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(train.pred!= OJ.train$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

test.pred = predict(svmfit.radial, OJ.test)
ptable<-table(OJ.test$Purchase, test.pred)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(test.pred!= OJ.test$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.6

# Repetir apartados (2) a (4) usando un SVM con un núcleo polinómico. Usar degree con valores 2,3,4,5,6.
svmfit.polynomial=svm(Purchase~., data=OJ.train, kernel ="polynomial", degree=c(2,3,4,5,6), cost=0.01, decision.values=T)
psummary<-summary(svmfit.polynomial)
print(psummary)

readline(prompt="Presione [enter] para continuar")

set.seed (3)
tune.out=tune(svm ,Purchase~.,data=OJ.train ,kernel = "polynomial", degree=c(2,3,4,5,6), ranges=list(cost=c(0.001, 0.01, 0.1, 1,10)))
psummary<-summary(tune.out)
print(psummary)

readline(prompt="Presione [enter] para continuar")

# Para cost = 0,001
svmfit.polynomial=svm(Purchase~., data=OJ.train, kernel ="polynomial", degree=c(2,3,4,5,6), cost=0.001, decision.values=T)
fitted =attributes (predict (svmfit.polynomial,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=0.001",col ="red")

readline(prompt="Presione [enter] para continuar")

# Para cost = 0,01
svmfit.polynomial=svm(Purchase~., data=OJ.train, kernel ="polynomial", degree=c(2,3,4,5,6), cost=0.01, decision.values=T)
fitted =attributes (predict (svmfit.polynomial,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=0.01",col ="red")

readline(prompt="Presione [enter] para continuar")

# Para cost = 0,1
svmfit.polynomial=svm(Purchase~., data=OJ.train, kernel ="polynomial", degree=c(2,3,4,5,6), cost=0.1, decision.values=T)
fitted =attributes (predict (svmfit.polynomial,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=0.1",col ="red")

readline(prompt="Presione [enter] para continuar")

# Para cost = 1
svmfit.polynomial=svm(Purchase~., data=OJ.train, kernel ="polynomial", degree=c(2,3,4,5,6), cost=1, decision.values=T)
fitted =attributes (predict (svmfit.polynomial,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=1",col ="red")

readline(prompt="Presione [enter] para continuar")

# Para cost = 10
svmfit.polynomial=svm(Purchase~., data=OJ.train, kernel ="polynomial", degree=c(2,3,4,5,6), cost=10, decision.values=T)
fitted =attributes (predict (svmfit.polynomial,OJ.train, decision.values =T))$decision.values
rocplot(fitted ,OJ.train[,"Purchase"], main="cost=10",col ="red")

readline(prompt="Presione [enter] para continuar")

svmfit.polynomial=svm(Purchase~., data=OJ.train, kernel ="polynomial", degree=c(2,3,4,5,6), cost=10, decision.values=T)
train.pred = predict(svmfit.polynomial, OJ.train)
ptable<-table(OJ.train$Purchase, train.pred)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(train.pred!= OJ.train$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

test.pred = predict(svmfit.polynomial, OJ.test)
ptable<-table(OJ.test$Purchase, test.pred)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(test.pred!= OJ.test$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

##################################################################################
## EJERCICIO 2
##################################################################################

# Cargar las librerias:
library(tree)

## EJERCICIO 2.1

# Información de OJ:
?OJ

# Crear un conjunto de entrenamiento conteniendo una muestra aleatoria de 800 observaciones, y un conjunto de test conteniendo el resto de las observaciones.
set.seed(1)
train=sample(1:nrow(OJ), 800)
OJ.train=OJ[train, ]
OJ.test=OJ[-train, ]

readline(prompt="Presione [enter] para continuar")

# Ajustar un árbol a los datos de "training", usando "Purchase" como la respuesta y las otras variables excepto "Buy" como predictores.
arbol.OJ=tree(Purchase~., data=OJ.train)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 2.2

# Usar la función summary() para generar un resumen estadístico acerca del árbol.
psummary<-summary(arbol.OJ)
print(psummary)

readline(prompt="Presione [enter] para continuar")

parbol<-arbol.OJ
print(parbol)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 2.3

# Crear un dibujo del árbol. Extraiga las reglas de clasificación más relevantes definidas por el árbol
plot(arbol.OJ)
text(arbol.OJ, pretty=0)

readline(prompt="Presione [enter] para continuar")

parbol<-arbol.OJ
print(parbol)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 2.4

# Predecir la respuesta de los datos de test, y generar e interpretar la matriz de confusión de los datos de test. ¿Cuál es la tasa de error del test? ¿Cuál es la precisión del test?.
tree.pred=predict(arbol.OJ, OJ.test, type = "class")
ptable<-table(tree.pred, OJ.test$Purchase)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(tree.pred!=OJ.test$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(tree.pred==OJ.test$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 2.5

# Aplicar la función cv.tree() al conjunto de "training" para determinar el tamaño óptimo del árbol. 
cv.OJ=cv.tree(arbol.OJ, FUN = prune.misclass)
pcv<-cv.OJ
print(pcv)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 2.6

# Generar un gráfico con el tamaño del árbol en el eje x y la tasa de error de validación cruzada en el eje y. 
plot(cv.OJ$size, cv.OJ$dev, type="b", xlab = "Tamaño del arbol", ylab = " Desviacion")

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 2.7

# Ajustar el árbol podado correspondiente al valor óptimo obtenido en 6. Comparar los errores sobre el conjunto de training y test de los árboles ajustados en 6 con el árbol podado. 
prune.OJ=prune.misclass(arbol.OJ, best = 2)
plot(prune.OJ)
text(prune.OJ, pretty = 0)

readline(prompt="Presione [enter] para continuar")

# Conjunto de Training:
tree.pred=predict(prune.OJ, OJ.train, type = "class")
ptable<-table(tree.pred, OJ.train$Purchase)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(tree.pred!=OJ.train$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Conjunto de Test:
tree.pred=predict(prune.OJ, OJ.test, type = "class")
ptable<-table(tree.pred, OJ.test$Purchase)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(tree.pred!=OJ.test$Purchase)
print(pmean)

readline(prompt="Presione [enter] para continuar")

##################################################################################
## EJERCICIO 3
##################################################################################

# Cargar las librerias:
library(ISLR)
library(gbm)

# Cargar OJ:
data(Hitters)
attach(Hitters)

## EJERCICIO 3.1

# Información de OJ:
?Hitters

readline(prompt="Presione [enter] para continuar")

# Eliminar las observaciones para las que la información del salario es desconocido y aplicar una transformación logarítmica al resto de valores de salario. 
Hitters=na.omit(Hitters)
Hitters

readline(prompt="Presione [enter] para continuar")

Hitters$Salary=log(Hitters$Salary)
Hitters$Salary

readline(prompt="Presione [enter] para continuar")

# Crear un conjunto de "training" con 200 observaciones y un conjunto de "test" con el resto 

train=1:200
Hitters.train=Hitters[train, ]
Hitters.test=Hitters[-train, ]

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 3.2

# Realizar boosting sobre el conjunto de entrenamiento con 1,000 árboles para un rango de valores del parámetro de ponderación ??. Realizar un gráfico con el eje x mostrando diferentes valores de ?? y los correspondientes valores de MSE de "training" sobre el eje y.
set.seed(1)
landa <- c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.02, 0.04, 0.06, 0.08, 0.1)
train.error <- rep(NA, 11)
for (i in 1:11) {
  boost.hitters=gbm(Salary~., data = Hitters.train, distribution ="gaussian", n.trees=1000, shrinkage=landa[i])
  yhat.boost=predict(boost.hitters, Hitters.train, n.trees=1000)
  train.error[i]=mean((yhat.boost-Hitters.train$Salary)^2)
}
plot(landa, train.error, type="b", xlab="Valores de Landa", ylab="MSE de Training")

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 3.3

# Realizar el mismo gráfico del punto anterior pero usando los valores de MSE del conjunto de test.

set.seed(1)
landa <- c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.02, 0.04, 0.06, 0.08, 0.1)
train.error <- rep(NA, 11)
for (i in 1:11) {
  boost.hitters=gbm(Salary~., data = Hitters.train, distribution ="gaussian", n.trees=1000, shrinkage=landa[i])
  yhat.boost=predict(boost.hitters, Hitters.test, n.trees=1000)
  train.error[i]=mean((yhat.boost-Hitters.test$Salary)^2)
}
plot(landa, train.error, type="b", xlab="Valores de Landa", ylab="MSE de Test")

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 3.4

# ¿Qué variables aparecen como las más importantes en el modelo de "boosting"? 

boost.04=gbm(Salary~.,data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage=0.04)
psummary<-summary(boost.04)
print(psummary)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 3.5

# Aplicar bagging al conjunto de "training" y volver a estimar el modelo. ¿Cuál es el valor de MSE para el conjunto de test en este caso? 
library (randomForest)
set.seed (1)
bag.Hitters=randomForest(Salary~., data=Hitters.train, mtry=19, importace=TRUE)
pbag<-bag.Hitters
print(pbag)

readline(prompt="Presione [enter] para continuar")

yhat.bag=predict(bag.Hitters, newdata=Hitters.test)
pmean<-mean((yhat.bag-Hitters.test$Salary)^2)
print(pmean)

readline(prompt="Presione [enter] para continuar")

##################################################################################
print("FIN DEL PROGRAMA")
##################################################################################

##################################################################################
## FIN DEL PROGRAMA
##################################################################################
