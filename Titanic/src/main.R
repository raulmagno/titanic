library('ProjectTemplate')
load.project()

# Titanic analysis
#
library(ggplot2)
library(caret)
library(rattle)

setwd("~/R/ScriptsR/Titanic")
source("functions.R") 
setwd("~/R/ScriptsR/Titanic/data")
train <- read.csv("train.csv",sep = ",",stringsAsFactors=FALSE)
test <- read.csv("test.csv",sep = ",",stringsAsFactors=FALSE)
#ShowAlldistributions(train)
#Pclass_survival <- table(train$Survived, train$Pclass)
train = train[-c(1,9:12)]
PassengerId <- test[1] 
test = test[-c(1,8:11)]


train.to.apply <- ETL(train,1)
test.to.apply <- ETL(test,2) 

#LinearModel(train.to.apply,test.to.apply,PassengerId)
pred <- RandomForest(train.to.apply,test.to.apply,PassengerId)

#getTree(modFit$finalModel,k=2)
#test.to.apply$predRight <- pred == test.to.apply$Survived
#table(pred,test.to.apply$Survived)

modFit <- train(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother, data = train.to.apply,method = "rpart")
fancyRpartPlot(modFit$finalModel)


