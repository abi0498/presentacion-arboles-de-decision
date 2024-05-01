install.packages("mlbench")

library(tidyverse)
library(caret)
#libreria para clasificacion
library(rpart)
install.packages("rattle")
library(rattle)
library(rpart.plot)
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("tiny")

help(PimaIndiansDiabetes)

summary(PimaIndiansDiabetes)
data<-PimaIndiansDiabetes
rm(entrenamientp)
# dividimos en 80% y 20% la data, para entrenamiento y test.
ind<-sample(2,nrow(PimaIndiansDiabetes),replace = TRUE, prob = c(0.8,0.2)) #80% entrenamiento, 20% teste


entrenamiento<-PimaIndiansDiabetes[ind==1,]
test<-PimaIndiansDiabetes[ind==2,]

#modelamos con arboles de decision
require(rpart)

control.poda<-rpart.control(maxdepth = 5,minsplit = 10)





arbol<-rpart(formula = diabetes~pregnant+glucose+pressure+triceps+insulin+mass+pedigree+age,
             data=entrenamiento, method = 'class', control = control.poda)
arbol4<-rpart(formula = diabetes~pregnant+glucose+pressure+triceps+insulin+mass+pedigree+age,
             data=entrenamiento, method = 'class')


summary(arbol)




print(arbol)
plotcp(arbol4)



table2


#graficar arbol
fancyRpartPlot(arbol,type=2,digits = -2,box.palette = "RdYlBu",  # Paleta de colores para los nodos
               yesno = 1,split.cex = 1.1,cex=0.53)#este
prp(arbol,type=, nn= TRUE, fallen.leaves = TRUE, main="DIagrama de arbol",faclen=4,varlen=8, shadow.col = "skyblue",
    box.palette = c("red","palegreen3"))

testpredic_arbol<-predict(arbol,newdata=test,type="class")
testpredic_arbol2<-predict(arbol4,newdata=test,type="class")

d<-data.frame(testpredic_arbol)
# hacemos las prediccion con nuestro test
dia_predic<-cbind(PimaIndiansDiabetes,predic_arbol)
#Matriz de confusion arbol prepodado

table(testpredic_arbol,test$diabetes)
#Matriz de confusion arbol sin podar
table(testpredic_arbol2,test$diabetes)
table(testpredic_arbol,testpredic_arbol2)

compa1<-data.frame(testpredic_arbol,testpredic_arbol2)
mean(test$mass)
test
#matriz de conufison para el arbol de decion prepodado
table(testpredic_arbol,test$diabetes)
#130 teste
predict(arbol,newdata = data.frame(pregnant=9,glucose=122,pressure=56,triceps=0,insulin=0,mass=27.9,pedigree=0.741,age=62)
        ,type="class")
# hacemos el podado del arbol
base_accuracy <- mean(predic_arbold == test$diabetes)
base_accuracy
table<-arbol$cptable
table
plotcp(arbol)


#poda de complejidad de costos

table4<-arbol4$cptable
table4
plotcp(arbol4)

rpart.plot(prune(arbol4, cp= 0.01699029 ))
d
d<-prune(arbol4, 0.01699029 )
table5<-d$cptable
table5


table(d,test$diabetes)

predic_arbold<-predict(d,newdata=test,type="class")
table(predic_arbold,test$diabetes)



##########################################################################################################
#post podado
Arbolp<-prune(arbol4,arbol$cptable[4,"CP"])
Arbolp2<-prune(arbol,cp= 0.01741294)
 fancyRpartPlot(Arbolp,type=2,digits = -2,box.palette = "RdYlBu",  # Paleta de colores para los nodos
                yesno = 1,split.cex = 1.1,cex=0.53)#este
 
 fancyRpartPlot(Arbolp2,type=2,digits = -2,box.palette = "RdYlBu",  # Paleta de colores para los nodos
                yesno = 1,split.cex = 1.1,cex=0.53)
 
 table2<-Arbolp$cptable
 table2
 plotcp(Arbolp)
 
predic_arbolp<-predict(Arbolp,newdata = test,type="class")
#matriz de confusion arbol post podado

table(predic_arbolp,test$diabetes)

help(cptable)



