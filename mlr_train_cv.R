
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

classif.lrn <- setHyperPars(classif.lrn, maxdepth =10, xval =5, minsplit = 5)

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

# hyperparameter tuning

minsplit <- makeParamSet(
                makeDiscreteParam("minsplit",
                                  values = c(5,10,15,25,30,40,60)),
                makeDiscreteParam("maxdepth",
                                  values = c(seq(2,12,1)))
                        )
ctrl <- makeTuneControlGrid()

set.seed(1234)

res <- tuneParams(learner = classif.lrn,
                  task = classif.task,
                  par.set = minsplit,
                  resampling = rdesc,
                  control = ctrl,
                  measures = list(auc,acc)
)









