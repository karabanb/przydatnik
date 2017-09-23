
#### loading required libraries #######

library(caret)
library(mlr)
library(dplyr)
library(ggplot2)
library(rpart)
library(ROCR)
library(pROC)

##### loading daset ####################

data("iris")
names(iris)
attach(iris)

raw.data <- dplyr::filter(iris, Species %in% c("virginica", "versicolor"))
raw.data$Species <- droplevels(raw.data$Species)


###### building the models  ############

fitControl <- trainControl(method = "boot",
                           number = 100,
                         ##  repeats = 100,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

m1.0 <- caret::train(Species~.,
                     data = raw.data,
                     method = "rpart",
                     trControl = fitControl,
                     metric = "ROC")
 
m1.0_pred <- predict(m1.0, newdata = raw.data, type = "raw")

confusionMatrix(data = m1.0_pred, reference =  raw.data$Species, positive = "virginica")

m1.0_pred <- ifelse(m1.0_pred %in% "virginica",1,0)

plot.roc(roc(response = raw.data$Species, predictor = m1.0_pred))


#### ---------------------------------- #####

task <- makeClassifTask(data = raw.data, target = "Species")

lrn <- makeLearner("classif.rpart", predict.type = "prob")

n <- nrow(raw.data)

train.set <- sample(n, size = 2/3 * n)
test.set <- setdiff(1:n, train.set)

m2.0 <-mlr::train(lrn, task, subset = train.set)

pred <- predict(m2.0, task = task, subset = test.set)

mlr::performance(pred, measures = list(auc))

 