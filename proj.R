library(ggplot2)
library(glmnet)
library(e1071)
library(plyr)
library(reshape2)
set.seed(107346)

setwd("~/Dropbox/MCC/AprendizajeMaquina/Proj")
alacranes <- read.csv("~/Dropbox/MCC/AprendizajeMaquina/Proj/csvlist1.csv")
alacranes$tipo <- as.factor(alacranes$tipo)
#Quitamos la columna ID
alacranes <- alacranes[,!names(alacranes) %in% c("ID")]

#Escalamos las variables a mu = 0, sigma = 1
alacranes.scaled <- data.frame(scale(alacranes[,-141]),tipo = alacranes$tipo)

#Reducción de dimensión
pca <- princomp(~., data=alacranes.scaled[, -141], cor = TRUE, na.action=na.exclude)
summary(pca)

#Acumulando  0.958141810 de la varianza
datos.red <- data.frame(pca$scores[,1:34], tipo = alacranes.scaled$tipo)

#
ggplot(aes(x = Comp.1, y = Comp.2, color = tipo), data = datos.red) + geom_point(position = position_jitter(width = .5))

# Prueba inicial
svm.1 <- svm(tipo ~ Comp.1 + Comp.2, data = datos.red, kernel = "radial", gamma = 1, cost = 100)
dat.x <- expand.grid(Comp.1 = seq(-30,30,0.5), Comp.2 = seq(-20,20,0.5))
dat.x$preds.1 <- predict(svm.1, newdata = dat.x)
ggplot(dat.x, aes(x = Comp.1, y = Comp.2, colour = preds.1)) + geom_point(size = 1) + geom_point(aes(x = Comp.1, y = Comp.2, color = tipo), data = datos.red, size = 3)


#Seleccionando el mejor parámetro C
#Cross - validation
obs <- sample(seq(1, dim(alacranes.scaled)[1]),dim(alacranes.scaled)[1], replace=F)
max <- length(obs)/5
kfolds <- split(obs, ceiling(seq_along(obs)/max))


########### Valores de C muy chicos benefician al SVM #######################
cross.validation <- sapply(c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000),function(C){
  medias.C <- sapply(seq(1, 5), function(i){
    train <- datos.red[unlist(kfolds[c(-i)]),]
    test <- datos.red[kfolds[[i]],]
    svm.c <- svm(tipo ~ ., data = train, kernel = "radial", gamma = 1, cost = C)
    preds <- predict(svm.c, newdata = test)
    mean(preds == test$tipo)
  })
})
cross.validation <- t(cross.validation)
cv.summarize <- data.frame(C = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000), mean = rowSums(cross.validation)/5, sd = apply(cross.validation, 1, sd))
ggplot(aes(x = log(C), y = mean), data = cv.summarize) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width=.1)) + geom_point()


svm.opt <- svm(tipo ~ Comp.1 + Comp.2, data = datos.red, kernel = "radial", gamma = 2, cost = 100)
dat.x <- expand.grid(Comp.1 = seq(-30,30,0.5), Comp.2 = seq(-20,20,0.5))
dat.x$preds.1 <- predict(svm.opt, newdata = dat.x)
ggplot(dat.x, aes(x = Comp.1, y = Comp.2, colour = preds.1)) + geom_point(size = 1) + geom_point(aes(x = Comp.1, y = Comp.2, color = tipo), data = datos.red, size = 3)
