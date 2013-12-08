library(rpart)

# Estableces la carpeta de trabajo
setwd("/Users/carlos/Documents/MATLAB")

# Carga de datos
alacranes<-read.csv("csvlist1.csv")

# Convertimos los datos y eliminamos la variable ID
alacranes[alacranes$tipo=="1111",]$tipo<-"No Venenoso"
alacranes[alacranes$tipo=="2222",]$tipo<-"Peligroso"
#alacranes[alacranes$tipo=="3333",]$tipo<-"Mortal"
alacranes$tipo<-as.factor(alacranes$tipo)
alacranes$ID<-NULL

# Muestra de entrenamiento y de prueba
set.seed(20130407)
MuestraEntrenamiento<-sample(1:50,40,replace=F)
alacranesEntrenamiento<-alacranes[MuestraEntrenamiento,]
alacranesPrueba<-alacranes[-MuestraEntrenamiento,]
# Modelar arbol
tree1<-rpart(tipo~.,alacranesEntrenamiento)
plot(tree1)
text(tree1)
table(alacranesPrueba$tipo,predict(tree1,newdata=alacranesPrueba ,type="class"))
summary(tree1)