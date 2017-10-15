
library(mlr)
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)

data("GermanCredit")

####### building the models ######

classif.task <- makeClassifTask(data = GermanCredit, target = "Class")

classif.lrn <-makeLearner("classif.rpart", predict.type = "prob")

#### modyfing of the hyperparameteres #######

classif.lrn <- setHyperPars(classif.lrn, maxdepth =3)

m1 <- mlr::train(classif.lrn, classif.task)

m1.rpart <- getLearnerModel(m1)   

rpart.plot(m1.rpart)

## definiovanie CV

rdesc <- makeResampleDesc("CV",predict = "test", iters = 10)

r <- resample(
                learner = classif.lrn
              , task = classif.task
              , resampling = rdesc
              , measures =  list(auc,acc, fpr, tnr)
              , models = TRUE
              )

## wyniki CV

CVScore <- r$measures.test

summary(r$measures.test[,-1])

barsAUC <- ggplot(data = CVScore, aes(iter, auc)) +
           geom_col() +
           geom_line( aes(iter, mean(auc)))

barsACC <- ggplot(data = CVScore, aes(iter, acc)) +
           geom_col() +
           geom_line( aes(iter, mean(acc)))

## modele

modelsList <- lapply(r$models, getLearnerModel)

sapply(modelsList, rpart.plot)






