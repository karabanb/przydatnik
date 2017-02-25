
library(caret)
data("GermanCredit")

t.c<-trainControl(method = "repeatedcv", number = 5, repeats = 3)
in.train<-createDataPartition(y=GermanCredit$Class,p=0.7, list = FALSE)
train.data<-GermanCredit[in.train,]

m1<-glm(formula = Class~., data = train.data, family = binomial(link = "logit"))
m1<-step(m1, direction = "both")

predicted.train<-stats::predict(m1,train.data, type = "response")

data2cutoff<-as.data.frame(cbind(train.data$Class, predicted.train))
data2cutoff$real<-ifelse(data2cutoff$V1==2,1,0)
data2cutoff<-data2cutoff[,-1]
