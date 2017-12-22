
library(mlr)
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)

data("GermanCredit")
####
set.seed(1234)

####### pararell computing ---------------------------------------------------------------------------------------------

#parallelStartSocket(4)

####### building the models --------------------------------------------------------------------------------------------

classif.task <- makeClassifTask(data = select(GermanCredit, Class, Duration), target = "Class")
classif.cost.task <- makeClassifTask(data = GermanCredit, target = "Class")

classif.lrn.m1 <- makeLearner("classif.rpart", predict.type = "prob")
classif.lrn.m2 <- makeLearner("classif.ctree", predict.type = "prob")


## definiovanie CV

set.seed(1234)

rdesc <- makeResampleDesc("CV",predict = "test", iters = 10, stratify = TRUE)
rdescLOO <- makeResampleDesc("LOO",predict = "test")
rdescRCV <- makeResampleDesc("RepCV",predict = "test", reps = 7, folds = 10, stratify = TRUE)

r <- resample(
                learner = classif.lrn.m1
              , task = classif.task
              , resampling = rdesc
              , measures =  list(auc,acc, fpr, tnr)
              , extract = getLearnerModel
              )

#rLOO <- resample(
#                 learner = classif.lrn
#                , task = classif.task
#                , resampling = rdescLOO
#                , measures =  list(auc,acc, fpr, tnr)
#                , extract = getLearnerModel
#                )

rRCV <- resample(
                  learner = classif.lrn.m1
                , task = classif.task
                , resampling = rdescRCV
                , measures =  list(mmce,auc)
                , extract = getLearnerModel               
                )

## wyniki CV na domyslnych parametrach rpart



## modele sprawdzenie

rin <- makeResampleInstance(rdesc,classif.task)

# hyperparameter tuning

param.m1 <- makeParamSet(
                makeDiscreteParam("minsplit",
                                  values = c(25,30,35)),
                makeDiscreteParam("maxdepth",
                                  values = c(seq(2,8,1)))
                        )

ctrl.m1 <- makeTuneControlGrid()

param.m2 <- makeParamSet(
                makeDiscreteParam("minsplit",
                                  values = c(10,25,30,35)),
                makeDiscreteParam("maxdepth",
                                  values = c(seq(2,10,1)))
                        )

ctrl.m2 <- makeTuneControlGrid()

res.m1 <- tuneParams(learner = classif.lrn.m1,
                     task = classif.task,
                     par.set = param.m1,
                     resampling = rdescRCV,
                     control = ctrl.m1,
                     measures = list(f1, auc, mmce)
                    )

res.m2 <- tuneParams(learner = classif.lrn.m2,
                       task = classif.task,
                       par.set = param.m2,
                       resampling = rdescRCV,
                       control = ctrl.m2,
                       measures = list(f1, auc, mmce)
                      )

# dodac rankingi parametrow

param.rank.m1 <- generateHyperParsEffectData(res.m1)$data

param.rank.m2 <- generateHyperParsEffectData(res.m2)$data

### budowa klasyfikatora na podstawie optymalnych parametrow

classif.lrn.m1 <- setHyperPars(classif.lrn.m1, maxdepth =8, xval =10, minsplit = 25)
classif.lrn.m2 <- setHyperPars(classif.lrn.m2, maxdepth = 9, minsplit =30)


m1 <- mlr::train(classif.lrn.m1, classif.task)
m2 <- mlr::train(classif.lrn.m2, classif.task)

m1.rpart <- getLearnerModel(m1)
m2.ctree <- getLearnerModel(m2)

rpart.plot(m1.rpart)
plot(m2.ctree)

pr.m1 <- predict(m1.rpart, newdata = GermanCredit, type = "class")
pr.m2 <- predict(m2.ctree, newdata = GermanCredit, type = "response")

confusionMatrix(data = pr.m1, reference = GermanCredit$Class)
confusionMatrix(data = pr.m2, reference = GermanCredit$Class)

### sprawdzenie zmiennosci klasyfikatorow ##################

rRCV.m1 <- resample(
                  learner = classif.lrn.m1
                , task = classif.task
                , resampling = rdescRCV
                , measures =  list(mmce,auc)
                , extract = getLearnerModel               
                )


RCV.m1 <- cbind(rRCV.m1$measures.test, rRCV$pred$instance$group)
RCV.m1 <- RCV.m1 %>% rename(., rep = 'rRCV$pred$instance$group')

barsAUC.m1 <- ggplot(data = RCV.m1, aes(iter, auc)) +
  geom_col(aes(fill = rep)) +
  geom_line( aes(iter, mean(auc))) +
  scale_fill_brewer(palette = "Paired") +
  ylim(0,1) +
  theme_bw()

barsMMCE.m1 <- ggplot(data = RCV.m1, aes(iter, mmce)) +
  geom_col(aes(fill = rep)) +
  geom_line( aes(iter, mean(mmce)))+
  scale_fill_brewer(palette = "Paired") +
  ylim(0,0.5) +
  theme_bw()


rRCV.m2 <- resample(
  learner = classif.lrn.m2
  , task = classif.task
  , resampling = rdescRCV
  , measures =  list(mmce,auc)
  , extract = getLearnerModel               
)

RCV.m2 <- cbind(rRCV.m2$measures.test, rRCV$pred$instance$group)
RCV.m2 <- RCV.m2 %>% rename(., rep = 'rRCV$pred$instance$group')

barsAUC.m2 <- ggplot(data = RCV.m2, aes(iter, auc)) +
  geom_col(aes(fill = rep)) +
  geom_line( aes(iter, mean(auc))) +
  scale_fill_brewer(palette = "Paired") +
  ylim(0,1) +
  theme_bw()

barsMMCE.m2 <- ggplot(data = RCV.m2, aes(iter, mmce)) +
  geom_col(aes(fill= rep)) +
  geom_line( aes(iter, mean(mmce))) +
  scale_fill_brewer(palette = "Paired")+
  ylim(0,0.5)+
  theme_bw()+
  annotate("text", x = 20, y = 0.4, label = paste("srednia ", mean(RCV.m2$mmce)))





