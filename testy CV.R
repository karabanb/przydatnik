
## dane pochodza z pliku mlr_train_cv_R


m1.pred <- predict(r$extract[[1]], newdata = GermanCredit, type = "class")
m2.pred <- predict(r$extract[[2]], newdata = GermanCredit, type = "class")
m3.pred <- predict(r$extract[[3]], newdata = GermanCredit, type = "class")
m4.pred <- predict(r$extract[[4]], newdata = GermanCredit, type = "class")

cf.1pred <- table(m1.pred,GermanCredit$Class)
acc.m1pred <- sum(diag(cf.1pred))/sum(cf.1pred)

### sprawdzanie modelu z iteracji 1

m1.pred1 <- predict(r$extract[[1]], 
                    newdata = GermanCredit[rin$test.inds[[1]],],
                    type = "class")

m1.pred2 <- predict(r$extract[[1]], 
                    newdata = GermanCredit[rin$test.inds[[2]],],
                    type = "class")

m1.pred3 <- predict(r$extract[[1]], 
                    newdata = GermanCredit[rin$test.inds[[3]],],
                    type = "class")

m1.pred4 <- predict(r$extract[[1]], 
                    newdata = GermanCredit[rin$test.inds[[4]],],
                    type = "class")


cf.m1pred1 <- table(m1.pred1,GermanCredit[rin$test.inds[[1]],"Class"])
cf.m1pred2 <- table(m1.pred2,GermanCredit[rin$test.inds[[2]],"Class"])
cf.m1pred3 <- table(m1.pred3,GermanCredit[rin$test.inds[[3]],"Class"])
cf.m1pred4 <- table(m1.pred4,GermanCredit[rin$test.inds[[4]],"Class"])

acc.m1pred1 <- sum(diag(cf.m1pred1))/sum(cf.m1pred1) # 0.724
acc.m1pred2 <- sum(diag(cf.m1pred2))/sum(cf.m1pred2) # 0.756
acc.m1pred3 <- sum(diag(cf.m1pred3))/sum(cf.m1pred3) # 0.736 !!! OK
acc.m1pred4 <- sum(diag(cf.m1pred4))/sum(cf.m1pred4) # 0.78

(mean(c(acc.m1pred1, acc.m1pred2, acc.m1pred3, acc.m1pred4))) ##CVScore ACC 0.736 , test 0.749


## sprawdzenie modelu z 2 iteracji

m2.pred1 <- predict(r$extract[[2]], 
                    newdata = GermanCredit[rin$test.inds[[1]],],
                    type = "class")

m2.pred2 <- predict(r$extract[[2]], 
                    newdata = GermanCredit[rin$test.inds[[2]],],
                    type = "class")

m2.pred3 <- predict(r$extract[[2]], 
                    newdata = GermanCredit[rin$test.inds[[3]],],
                    type = "class")

m2.pred4 <- predict(r$extract[[2]], 
                    newdata = GermanCredit[rin$test.inds[[4]],],
                    type = "class")


cf.m2pred1 <- table(m2.pred1,GermanCredit[rin$test.inds[[1]],"Class"])
cf.m2pred2 <- table(m2.pred2,GermanCredit[rin$test.inds[[2]],"Class"])
cf.m2pred3 <- table(m2.pred3,GermanCredit[rin$test.inds[[3]],"Class"])
cf.m2pred4 <- table(m2.pred4,GermanCredit[rin$test.inds[[4]],"Class"])

acc.m2pred1 <- sum(diag(cf.m2pred1))/sum(cf.m2pred1) # 0.724
acc.m2pred2 <- sum(diag(cf.m2pred2))/sum(cf.m2pred2) # 0.756
acc.m2pred3 <- sum(diag(cf.m2pred3))/sum(cf.m2pred3) # 0.736 !!! OK
acc.m2pred4 <- sum(diag(cf.m2pred4))/sum(cf.m2pred4) # 0.78

(mean(c(acc.m2pred1, acc.m2pred2, acc.m2pred3, acc.m2pred4))) ##CVScore ACC 0.728 , test 0.749

## sprawdzenie modelu z 3 iteracji

m3.pred1 <- predict(r$extract[[3]], 
                    newdata = GermanCredit[rin$test.inds[[1]],],
                    type = "class")

m3.pred2 <- predict(r$extract[[3]], 
                    newdata = GermanCredit[rin$test.inds[[2]],],
                    type = "class")

m3.pred3 <- predict(r$extract[[3]], 
                    newdata = GermanCredit[rin$test.inds[[3]],],
                    type = "class")

m3.pred4 <- predict(r$extract[[3]], 
                    newdata = GermanCredit[rin$test.inds[[4]],],
                    type = "class")


cf.m3pred1 <- table(m3.pred1,GermanCredit[rin$test.inds[[1]],"Class"])
cf.m3pred2 <- table(m3.pred2,GermanCredit[rin$test.inds[[2]],"Class"])
cf.m3pred3 <- table(m3.pred3,GermanCredit[rin$test.inds[[3]],"Class"])
cf.m3pred4 <- table(m3.pred4,GermanCredit[rin$test.inds[[4]],"Class"])

acc.m3pred1 <- sum(diag(cf.m3pred1))/sum(cf.m3pred1) # 0.744
acc.m3pred2 <- sum(diag(cf.m3pred2))/sum(cf.m3pred2) # 0.748
acc.m3pred3 <- sum(diag(cf.m3pred3))/sum(cf.m3pred3) # 0.716 
acc.m3pred4 <- sum(diag(cf.m3pred4))/sum(cf.m3pred4) # 0.728 !!!! OK

(mean(c(acc.m3pred1, acc.m3pred2, acc.m3pred3, acc.m3pred4))) ##CVScore ACC 0.728 , test 0.734


## sprawdzenie modelu z 4 iteracji

m4.pred1 <- predict(r$extract[[4]], 
                    newdata = GermanCredit[rin$test.inds[[1]],],
                    type = "class")

m4.pred2 <- predict(r$extract[[4]], 
                    newdata = GermanCredit[rin$test.inds[[2]],],
                    type = "class")

m4.pred3 <- predict(r$extract[[4]], 
                    newdata = GermanCredit[rin$test.inds[[3]],],
                    type = "class")

m4.pred4 <- predict(r$extract[[4]], 
                    newdata = GermanCredit[rin$test.inds[[4]],],
                    type = "class")


cf.m4pred1 <- table(m4.pred1,GermanCredit[rin$test.inds[[1]],"Class"])
cf.m4pred2 <- table(m4.pred2,GermanCredit[rin$test.inds[[2]],"Class"])
cf.m4pred3 <- table(m4.pred3,GermanCredit[rin$test.inds[[3]],"Class"])
cf.m4pred4 <- table(m4.pred4,GermanCredit[rin$test.inds[[4]],"Class"])

acc.m4pred1 <- sum(diag(cf.m4pred1))/sum(cf.m4pred1) # 0.748
acc.m4pred2 <- sum(diag(cf.m4pred2))/sum(cf.m4pred2) # 0.712
acc.m4pred3 <- sum(diag(cf.m4pred3))/sum(cf.m4pred3) # 0.704
acc.m4pred4 <- sum(diag(cf.m4pred4))/sum(cf.m4pred4) # 0.78

(mean(c(acc.m4pred1, acc.m4pred2, acc.m4pred3, acc.m4pred4))) ##CVScore ACC 0.708 , test 0.736
