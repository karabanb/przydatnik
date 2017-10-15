
library(mlr)
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)

data("GermanCredit")

set.seed(1234)

####### building the models ######

classif.task <- makeClassifTask(data = GermanCredit, target = "Class")

classif.lrn <-makeLearner("classif.rpart", predict.type = "prob")

#### modyfing of the hyperparameteres #######

classif.lrn <- setHyperPars(classif.lrn, maxdepth =3, xval =5)

m1 <- mlr::train(classif.lrn, classif.task)

m1.rpart <- getLearnerModel(m1)   

rpart.plot(m1.rpart)

## definiovanie CV
set.seed(1234)
rdesc <- makeResampleDesc("CV",predict = "test", iters = 4, stratify = TRUE)

r <- resample(
                learner = classif.lrn
              , task = classif.task
              , resampling = rdesc
              , measures =  list(auc,acc, fpr, tnr)
              , extract = getLearnerModel
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

## modele sprawdzenie

rin <- makeResampleInstance(rdesc,classif.task)

# na calym










