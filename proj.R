library(ggplot2)
library(e1071)
library(plyr)
library(reshape2)
library(nnet)
library(tree)
library(randomForest)
set.seed(107346)

alacranes <- read.csv("csvlist1.csv")
alacranes[which(alacranes$tipo == 1111), c("tipo")] <- 0
alacranes[which(alacranes$tipo == 2222), c("tipo")] <- 1
alacranes$tipo <- as.factor(alacranes$tipo)
#Quitamos la columna ID
alacranes <- alacranes[,!names(alacranes) %in% c("ID")]

summary(alacranes)

#Escalamos las variables a mu = 0, sigma = 1
alacranes.scaled <- data.frame(scale(alacranes[,-141]),tipo = alacranes$tipo)

#Reducción de dimensión
pca <- princomp(~., data=alacranes.scaled[, -141], cor = TRUE, na.action=na.exclude)
summary(pca)

#Acumulando  0.973967970 de la varianza
datos.red <- data.frame(pca$scores[,1:40], tipo = alacranes.scaled$tipo)

#
ggplot(aes(x = Comp.1, y = Comp.2, color = tipo), data = datos.red) + geom_point(position = position_jitter(width = .5))

#Cross-validation
obs <- sample(seq(1, dim(alacranes.scaled)[1]),dim(alacranes.scaled)[1], replace=F)
max <- length(obs)/5
kfolds <- split(obs, ceiling(seq_along(obs)/max))


################################################################
#                           ANN                                #
################################################################
ann.cross.validation <- sapply(seq(1,5), function(i){
  train <- datos.red[unlist(kfolds[c(-i)]),]
  test <- datos.red[kfolds[[i]],]
  ann <- nnet(tipo ~ ., data = train, size = 12, decay = 1E-9, maxit = 1000)
  preds <- predict(ann, newdata = test)
  mean(preds == test$tipo)
})
mean(ann.cross.validation)


################################################################
#                           SVM                                #
################################################################

# Prueba inicial
svm.1 <- svm(tipo ~ Comp.1 + Comp.2, data = datos.red, kernel = "radial", gamma = 1, cost = 100)
dat.x <- expand.grid(Comp.1 = seq(-30,30,0.5), Comp.2 = seq(-20,20,0.5))
dat.x$preds.1 <- predict(svm.1, newdata = dat.x)
ggplot(dat.x, aes(x = Comp.1, y = Comp.2, colour = preds.1)) + geom_point(size = 1) + geom_point(aes(x = Comp.1, y = Comp.2, color = tipo), data = datos.red, size = 3)


########### Seleccionando C mediante validación cruzada #######################
cross.validation <- sapply(c(0.0001, 0.001,0.005, 0.01,0.05, 0.1, 0.5, 1, 2, 3, 5, 10, 50, 100, 250, 500, 1000, 5000, 10000, 15000),function(C){
  medias.C <- sapply(seq(1, 5), function(i){
    #train <- datos.red[unlist(kfolds[c(-i)]),]
    #test <- datos.red[kfolds[[i]],]
    
    train <- alacranes.scaled[unlist(kfolds[c(-i)]),]
    test <- alacranes.scaled[kfolds[[i]],]
    
    svm.c <- svm(tipo ~ ., data = train, kernel = "radial", gamma = 2, cost = C)
    preds <- predict(svm.c, newdata = test)
    mean(preds == test$tipo)
  })
})
cross.validation <- t(cross.validation)
cv.summarize <- data.frame(C = c(0.0001, 0.001,0.005, 0.01,0.05, 0.1, 0.5, 1, 2, 3, 5, 10, 50, 100, 250, 500, 1000, 5000, 10000, 15000), mean = rowSums(cross.validation)/5, sd = apply(cross.validation, 1, sd))
max(cv.summarize$mean)
ggplot(aes(x = log(C), y = mean), data = cv.summarize) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width=.1)) + geom_point()

# SVM optimo
svm.opt.2 <- svm(tipo ~ Comp.1 + Comp.2, data = datos.red, kernel = "radial", gamma = 2, cost = 100)
dat.x <- expand.grid(Comp.1 = seq(-30,30,0.5), Comp.2 = seq(-20,20,0.5))
dat.x$preds.svm <- predict(svm.opt.2, newdata = dat.x)
ggplot(dat.x, aes(x = Comp.1, y = Comp.2, colour = preds.svm)) + geom_point(size = 1) + geom_point(aes(x = Comp.1, y = Comp.2, color = tipo), data = datos.red, size = 3)

################################################################
#                    Sesgo y Varianza                          #
################################################################

# Ocupamos statrified sampling para asegurar que tenemos
# al menos un elemento de cada clase en la muestra
# evitando el error de SVM vacio
##
idx.tipo1 <- sample(which(datos.red$tipo == 1), .8 * 174)
idx.tipo0 <- sample(which(datos.red$tipo == 0), .8 * 62)
train <- rbind(datos.red[idx.tipo1,], datos.red[idx.tipo0,])
test <- rbind(datos.red[-idx.tipo0,],datos.red[-idx.tipo1,])

#Shuffle data
train <- train[sample(dim(train)[1]),]
test <- test[sample(dim(test)[1]),]

errs <- sapply(seq(20, dim(train)[1]), function(i){
  svm.opt <- svm(tipo ~ ., data = train[1:i,], kernel = "radial", gamma = 2, cost = 100)
  #Err train
  preds.train <- predict(svm.opt, newdata = train[1:i,])
  err.train <- mean(preds.train != train[1:i,]$tipo)
  #Err test
  preds.test <- predict(svm.opt, newdata = test)
  err.test <- mean(preds.test != test$tipo)
  list(i, err.train, err.test)
})
errs <- t(errs)
errs.df.m<- melt(data.frame(i = unlist(errs[,1]),err.train = unlist(errs[,2]), err.test = unlist(errs[,3])), id=c("i"))
ggplot(aes(x = i, y = value, colour=variable), data = errs.df.m) + geom_line()

################################################################
#                    Random Forest                             #
################################################################
tree.cross.validation <- sapply(seq(1,5), function(i){
  train <- datos.red[unlist(kfolds[c(-i)]),]
  test <- datos.red[kfolds[[i]],]
  rf <- randomForest(tipo~.,data=train, ntree=350, keep.forest=TRUE, importance=TRUE)
  preds <- predict(rf, newdata = test)
  mean(preds == test$tipo)
})
mean(tree.cross.validation)

rf.opt.2 <- randomForest(tipo ~ Comp.1 + Comp.2, data = datos.red, ntree = 350, keep.forest=TRUE, importance=TRUE)
dat.x$preds.rf <- predict(rf.opt.2, newdata = dat.x)
ggplot(dat.x, aes(x = Comp.1, y = Comp.2, colour = preds.rf)) + geom_point(size = 1) + geom_point(aes(x = Comp.1, y = Comp.2, color = tipo), data = datos.red, size = 3)

################################################################
#                    Comparando modelos                        #
################################################################
set.seed(107346)
idx.tipo1 <- sample(which(datos.red$tipo == 1), .8 * 174)
idx.tipo0 <- sample(which(datos.red$tipo == 0), .8 * 62)
train <- rbind(datos.red[idx.tipo1,], datos.red[idx.tipo0,])
test <- rbind(datos.red[-idx.tipo0,],datos.red[-idx.tipo1,])

#Shuffle data
train <- train[sample(dim(train)[1]),]
test <- test[sample(dim(test)[1]),]


svm.opt <- svm(tipo ~ ., data = train, kernel = "radial", gamma = 2, cost = 100)
preds.opt <- predict(svm.opt, newdata = test)
t.confusion <- table(preds.opt, test$tipo)
t.confusion

prec.svm <- t.confusion[1] / (t.confusion[1] + t.confusion[3])
prec.svm
recall.svm <- t.confusion[1] / (t.confusion[1] + t.confusion[2])
recall.svm
f1.svm <- 2 * (prec.svm * recall.svm) / (prec.svm + recall.svm)
f1.svm

rf.opt <- randomForest(tipo ~ ., data = train, keep.forest=TRUE, importance = TRUE)
preds.opt <- predict(rf.opt, newdata = test)
t.confusion <- table(preds.opt, test$tipo)
t.confusion

prec.rf <- t.confusion[1] / (t.confusion[1] + t.confusion[3])
prec.rf
recall.rf <- t.confusion[1] / (t.confusion[1] + t.confusion[2])
recall.rf
f1.rf <- 2 * (prec.svm * recall.svm) / (prec.svm + recall.svm)
f1.rf

################################################################
#                    Sesgo y Varianza                          #
################################################################

# Ocupamos statrified sampling para asegurar que tenemos
# al menos un elemento de cada clase en la muestra
# evitando el error de SVM vacio
##
idx.tipo1 <- sample(which(datos.red$tipo == 1), .8 * 174)
idx.tipo0 <- sample(which(datos.red$tipo == 0), .8 * 62)
train <- rbind(datos.red[idx.tipo1,], datos.red[idx.tipo0,])
test <- rbind(datos.red[-idx.tipo0,],datos.red[-idx.tipo1,])

#Shuffle data
train <- train[sample(dim(train)[1]),]
test <- test[sample(dim(test)[1]),]

errs <- sapply(seq(20, dim(train)[1]), function(i){
  rf.opt <- randomForest(tipo ~ ., data = train[1:i,], keep.forest=TRUE, importance=TRUE)
  #Err train
  preds.train <- predict(rf.opt, newdata = train[1:i,])
  err.train <- mean(preds.train != train[1:i,]$tipo)
  #Err test
  preds.test <- predict(rf.opt, newdata = test)
  err.test <- mean(preds.test != test$tipo)
  list(i, err.train, err.test)
})
errs <- t(errs)
errs.df.m<- melt(data.frame(i = unlist(errs[,1]),err.train = unlist(errs[,2]), err.test = unlist(errs[,3])), id=c("i"))
ggplot(aes(x = i, y = value, colour=variable), data = errs.df.m) + geom_line()



#######################################################
#                    Bootstrap                        #
#######################################################
idx.tipo1 <- sample(which(alacranes.scaled$tipo == 1), .8 * 174)
idx.tipo0 <- sample(which(alacranes.scaled$tipo == 0), .8 *0.7372881* 62)
train <- alacranes.scaled[c(idx.tipo1, idx.tipo0),]
test <- alacranes.scaled[-c(idx.tipo1, idx.tipo0),]

#Shuffle data
train <- train[sample(dim(train)[1]),]
test <- test[sample(dim(test)[1]),]

#bootstrap sobre el train
train.boot <- train[sample(1:175, 1000, replace = T),]

svm.opt <- svm(tipo ~ ., data = train.boot, kernel = "radial", gamma = 2, cost = 100)
#validacion final
preds <- predict(svm.opt, newdata = test)
mean(preds == test$tipo)