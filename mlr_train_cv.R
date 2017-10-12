
library(mlr)
library(caret)

data("GermanCredit")

####### building the models ######

classif.task <- makeClassifTask(data = GermanCredit, target = "Class")

classif.lrn <-makeLearner("classif.rpart", predict.type = "prob")

## definiovanie CV

rdesc <- makeResampleDesc("CV", iters = 3)

r <- resample(learner = classif.lrn,task = classif.task, resampling = rdesc)

