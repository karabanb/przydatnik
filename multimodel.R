
library(dplyr)
library(mlr)
library(caret)

data("GermanCredit")

classif.rpart<- makeLearner("classif.rpart", predict.type = "prob")




task.list<-list()
m <- list()

name <- colnames(GermanCredit)

for (i in 1:length(name)){
  df <- GermanCredit[,c(i,10)]
  task.list[[i]]<- makeClassifTask(data = df, target="Class")
  names(task.list)[i]<- name[i]
}

t <- list()
m <- list()
for (j in 1: length(task.list)){
  t[[j]]<- mlr::train(task = task.list[[j]], learner = classif.rpart)
  m[[j]]<- getLearnerModel(t[[j]])
}

rdesc <- makeResampleDesc(method = "CV",stratify = TRUE, iters = 10)

r<-list()

for (i in 1:length(task.list)){
  r[[i]] <-resample(
                      learner = classif.rpart,
                      task = task.list[[i]],
                      extract = getLearnerModel,
                      resampling = rdesc
  ) 
}

table <- data.frame()

for (i in 1:length(r)){
  table[i,1] <- r[[i]]$aggr
}



name  <- colnames(iris)

for ( i in name){
  if(is.factor(iris[,i])==TRUE){
    next}
  z<-sum(iris[,i])
  
  
  print(z)
}
