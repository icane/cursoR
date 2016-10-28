#Minimos cuadrados ordinarios
attach(cars) 
summary(cars) 
fm1 <- lm (dist ~ speed, data = cars)
fm1
summary(fm1)
plot(cars, xlab = "Velocidad (mph)", ylab = "Distancia de frenado (ft)",las = 1,main="Grafica nº1")
lines(cars$speed,fm1$fitted,col="red")
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)
library(gvlma)
gvmodelo <- gvlma(fm1)
summary(gvmodelo)
plot(gvmodelo)
# predicción de un modelo
nuevosdatos <- data.frame(speed=c(20,25,30))
predict(fm1,nuevosdatos,interval = "prediction")
# Seleccion de modelos
library(leaps)
regfit.fwd=regsubsets(mpg~.,data=mtcars,method="forward")
plot(regfit.fwd)
summary(regfit.fwd)
regfit.bwd=regsubsets(mpg~.,data=mtcars,method="backward") 
plot(regfit.bwd)
summary(regfit.bwd)
regfit.exh=regsubsets(mpg~.,data=mtcars,method="exhaustive") 
plot(regfit.exh)
summary(regfit.exh)
coef(regfit.exh,8)
# Modelo GLM
fm2 <- glm(mpg ~ disp + hp +  drat +  wt + qsec + am + gear + carb, data=mtcars,family=gaussian(link="identity")) 
summary(fm2)
fm3 <- glm(mpg ~ disp + hp +  drat +  wt + qsec + am + gear + carb, data=mtcars,family=gaussian(link="log")) 
summary(fm3)
fm4 <- glm(mpg ~ disp + hp +  drat +  wt + qsec + am + gear + carb, data=mtcars,family=gaussian(link="inverse"))
summary(fm4)
fm5 <- glm(mpg ~ disp + hp +  drat +  wt + qsec + am + gear + carb, data=mtcars,family=Gamma(link="identity"))
summary(fm5)
# Modelo probabilistico lineal
library(ISLR) 
attach(Default) 
fit=glm(default~.,data=Default,family=binomial) 
summary(fit) 
summary(fit$fitted.values)
fit.pred=ifelse(fit$fitted.values>0.0333,1,0)
table(fit.pred,Default$default)
fit2=glm(default~.,data=Default,family=binomial (link=probit))  
summary(fit2) 
fit2.probs=predict(fit2,type="response")
summary(fit2.probs)
fit2.pred=ifelse(fit2.probs>0.03348,1,0)
table(fit2.pred,Default$default)
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)
# división de la muestra en entrenamiento y validacion
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit) 
summary(mylogit$fitted.values)
mylogit.probs=predict(mylogit,type="response")
summary(mylogit.probs)
fit.pred=ifelse(mylogit.probs>0.5,1,0)
table(fit.pred,mydata$admit)
library(ROCR)
predict.rocr  <- prediction (mylogit.probs,mydata$admit)
perf.rocr     <- performance(predict.rocr,"tpr","fpr") #True y Tasa de falsos positivos
auc <- as.numeric(performance(predict.rocr ,"auc")@y.values)
plot(perf.rocr,type='o', main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)

train=sample(seq(length(Default$default)),length(Default$default)*0.70,replace=FALSE)
# Estimación de modelo probit
glm.tr=glm(default[train]~.,data=Default[train,],family=binomial(link=probit))
#predicción
probs=predict.glm(glm.tr,newdata=Default[-train,],type="response")
pred=ifelse(probs>mean(probs),1,0)
table(pred,Default$default[-train])
library(ROCR)
predict.rocr  <- prediction (probs,Default$default[-train])
perf.rocr     <- performance(predict.rocr,"tpr","fpr") #True y Tasa de falsos positivos
auc <- as.numeric(performance(predict.rocr ,"auc")@y.values)
plot(perf.rocr,type='o', main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)
# Modelo discriminante lineal
library(MASS)
data(iris)
attach(iris)
# división de la muestra en entrenamiento y validacion
train=sample(seq(length(iris$Species)),length(iris$Species)*0.70,replace=FALSE)
# Lineal Discriminat Analisys
lda.tr=lda(Species[train]~.,data=iris[train,])
#predicción
probs=predict(lda.tr,newdata=iris[-train,],type="prob")
data.frame(probs)[1:5,]
table(probs$class,iris$Species[-train])
mean(probs$class==iris$Species[-train]) #porcentaje de bien clasificados

#Ejemplo para clase
data(cpus, package="MASS")

# Arbol de decision
library(tree)
data(iris)
attach(iris)
# división de la muestra en entrenamiento y validacion
train=sample(seq(length(Species)),length(Species)*0.70,replace=FALSE)
# Arbol de decision
iris.tree = tree(Species~.,iris,subset=train)
summary(iris.tree)
plot(iris.tree);text(iris.tree,pretty=0)
#Predicción
tree.pred=predict(iris.tree,iris[-train,],type="class")
summary(tree.pred)
with(iris[-train,],table(tree.pred,Species))
# Poda arbol de decision
cv.iris=cv.tree(iris.tree,FUN=prune.misclass)
cv.iris
plot(cv.iris)
prune.iris=prune.misclass(iris.tree,best=3)
plot(prune.iris);text(iris.tree,pretty=0)
# Máquina soporte vector
library(e1071)
data(iris)
attach(iris)
# división de la muestra en entrenamiento y validacion
train=sample(seq(length(Species)),length(Species)*0.70,replace=FALSE)
# Máquina Soporte Vector
svmfit=svm(Species~.,data=iris,kernel="linear",scale=FALSE,subset=train)
print(svmfit)
table(iris$Species[train],svmfit$fitted)
# Predicción para la muestra test
svm.pred=predict(svmfit,iris[-train,])
summary(svm.pred)
with(iris[-train,],table(svm.pred,Species))
datos=read.csv("http://www.icane.es/data/api/jobseekers-registered-unemployment-municipalities-gender-education-level.google-json?s=%5Bmunicipio.municipio%5Df%3B%5Bmes_nombre_std.meses%5Dc%3D%5B2016%5D.%5BSeptiembre%5D%3B%5Bsexo.sexo%5Dc%3B%5Bnivel_estudios_std.nivelestudios%5Dc%3B%5BMeasures%5Dc")
library(XML)
datos=readHTMLTable("http://www.icane.es/u/12p6")
install.packages("rjson")
library(rjson)
result <- fromJSON(file = "http://www.icane.es/data/api/jobseekers-registered-unemployment-municipalities-gender-education-level.json?s=%5Bmunicipio.municipio%5Df%3B%5Bmes_nombre_std.meses%5Dc%3D%5B2016%5D.%5BSeptiembre%5D%3B%5Bsexo.sexo%5Dc%3D%5Bsexo.sexo%5D%3B%5Bnivel_estudios_std.nivelestudios%5Dc%3D%5Bnivel_estudios_std.nivelestudios%5D%3B%5BMeasures%5Dc%3D%5BCMParo%5D")
result2 <- data.frame(result$data)
str(result2)












           





