ShowAlldistributions <- function(dataframe.to.plots){
  
  plotar <- function(elemento,name){
    qplot(x = elemento, data = data.frame(elemento=elemento), color = I('blue'))
    nome <- paste(name ,".jpg")
    ggsave(file = nome,path = '~/R/ScriptsR/Titanic/distributions')
    dev.off()
  }
  
  for(i in 1:ncol(dataframe.to.plots))
  {   
    nome <- paste(colnames(dataframe.to.plots)[i])
    plotar(dataframe.to.plots[,i],nome)   
  }
  
}
ETL <- function(dataframe.to.transform,flag){
  
  dataframe.to.transform$Sex = gsub("female", 1, dataframe.to.transform$Sex)
  dataframe.to.transform$Sex = gsub("^male", 0, dataframe.to.transform$Sex)
  
  master_vector = grep("Master.",dataframe.to.transform$Name, fixed=TRUE)
  miss_vector = grep("Miss.", dataframe.to.transform$Name, fixed=TRUE)
  mrs_vector = grep("Mrs.", dataframe.to.transform$Name, fixed=TRUE)
  mr_vector = grep("Mr.", dataframe.to.transform$Name, fixed=TRUE)
  dr_vector = grep("Dr.", dataframe.to.transform$Name, fixed=TRUE)
  
  PassengerId = dataframe.to.transform[1]
  for(i in master_vector) {
    dataframe.to.transform$Name[i] = "Master"  
  }
  for(i in miss_vector) {
    dataframe.to.transform$Name[i] = "Miss"
  }
  for(i in mrs_vector) {
    dataframe.to.transform$Name[i] = "Mrs"
  }
  for(i in mr_vector) {
    dataframe.to.transform$Name[i] = "Mr"
  }
  for(i in dr_vector) {
    dataframe.to.transform$Name[i] = "Dr"
  }
  
  master_age = round(mean(dataframe.to.transform$Age[dataframe.to.transform$Name == "Master"], na.rm = TRUE), digits = 2)
  miss_age = round(mean(dataframe.to.transform$Age[dataframe.to.transform$Name == "Miss"], na.rm = TRUE), digits =2)
  mrs_age = round(mean(dataframe.to.transform$Age[dataframe.to.transform$Name == "Mrs"], na.rm = TRUE), digits = 2)
  mr_age = round(mean(dataframe.to.transform$Age[dataframe.to.transform$Name == "Mr"], na.rm = TRUE), digits = 2)
  dr_age = round(mean(dataframe.to.transform$Age[dataframe.to.transform$Name == "Dr"], na.rm = TRUE), digits = 2)
  
  for (i in 1:nrow(dataframe.to.transform)) {
    if (is.na(dataframe.to.transform[i,"Age"])) {     
      if (dataframe.to.transform$Name[i] == "Master") {
        dataframe.to.transform$Age[i] = master_age
      } else if (dataframe.to.transform$Name[i] == "Miss") {
        dataframe.to.transform$Age[i] = miss_age
      } else if (dataframe.to.transform$Name[i] == "Mrs") {
        dataframe.to.transform$Age[i] = mrs_age
      } else if (dataframe.to.transform$Name[i] == "Mr") {
        dataframe.to.transform$Age[i] = mr_age
      } else if (dataframe.to.transform$Name[i] == "Dr") {
        dataframe.to.transform$Age[i] = dr_age
      } else {
        print("Uncaught Title")
      }
    }
  }
  
  if(flag == 2){  
    dataframe.to.transform[89, "Age"] = miss_age    
  }
  
  dataframe.to.transform["Child"] = NA
  for (i in 1:nrow(dataframe.to.transform)) {
    #print (i)
    
    if (dataframe.to.transform$Age[i] <= 12) {
      dataframe.to.transform$Child[i] = 1
    } else {
      dataframe.to.transform$Child[i] = 2
    }
  }
  
  dataframe.to.transform["Family"] = NA
  
  for(i in 1:nrow(dataframe.to.transform)) {
    x = dataframe.to.transform$SibSp[i]
    y = dataframe.to.transform$Parch[i]
    dataframe.to.transform$Family[i] = x + y + 1
  }
  
  dataframe.to.transform["Mother"] = NA
  for(i in 1:nrow(dataframe.to.transform)) {
    if(dataframe.to.transform$Name[i] == "Mrs" & dataframe.to.transform$Parch[i] > 0) {
      dataframe.to.transform$Mother[i] = 1
    } else {
      dataframe.to.transform$Mother[i] = 2
    }
  }
  
  return (dataframe.to.transform) 
  
}

LinearModel <- function(train.to.apply,test.to.apply,PassengerId){
  
  train.glm <- glm(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother, family = binomial, data = train.to.apply)
  summary(train.glm)
  
  p.hats <- predict.glm(train.glm, newdata = test.to.apply, type = "response")
  
  survival <- vector()
  for(i in 1:length(p.hats)) {
    if(p.hats[i] > .5) {
      survival[i] <- 1
    } else {
      survival[i] <- 0
    }
  }
  
  kaggle.sub <- cbind(PassengerId,survival)
  colnames(kaggle.sub) <- c("PassengerId", "Survived")
  write.csv(kaggle.sub, file = "kaggleLinarModel.csv", row.names = FALSE)  
}

RandomForest <- function(train.to.apply,test.to.apply,PassengerId){
  
  modFit <- train(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother, data = train.to.apply,method = "rf", proxy = TRUE)
  pred <- predict(modFit,test.to.apply)
  
  survival <- vector()
  for(i in 1:length(pred)) {
    if(pred[i] > .5) {
      survival[i] <- 1
    } else {
      survival[i] <- 0
    }
  }
  kaggle.sub <- cbind(PassengerId,survival)
  colnames(kaggle.sub) <- c("PassengerId", "Survived")
  write.csv(kaggle.sub, file = "kaggleRandomForest.csv", row.names = FALSE) 
  return (pred)
}