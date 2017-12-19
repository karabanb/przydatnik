
library(caret)
library(ggplot2)
data("GermanCredit")

#testeer po reinstalacji gita

t.c<-trainControl(method = "repeatedcv", number = 5, repeats = 1)
in.train<-createDataPartition(y=GermanCredit$Class,p=0.7, list = FALSE)
train.data<-GermanCredit[in.train,]

m1<-glm(formula = Class~., data = train.data, family = binomial(link = "logit"))
m1<-step(m1, direction  = "both")

predicted.train<-stats::predict(m1,train.data, type = "response")

data2cutoff<-as.data.frame(cbind(train.data$Class, predicted.train))
data2cutoff$real<-ifelse(data2cutoff$V1==2,1,0)
data2cutoff<-data2cutoff[,-1]


score<-round(seq(min(data2cutoff$predicted.train),max(data2cutoff$predicted.train),0.01),2) ## sekwencja score co 1 pkt
score<-seq(0.05,0.99,0.01)

summary(data2cutoff)

df.tresholds<-data.frame(treshold=score,
               TP=0,
               FP=0,
               TN=0,
               FN=0,
               ACC=0,
               Gain=0
               )

####################################
data2cutoff$pred.cut<-0
conf.matrix<-matrix(0,2,2)

LGD<-0.6 # czesc straconego kapitalu
profit<-0.5 # marza na udzielonej pozyczce
pozyczka<-3000 # wartosc kapitalu w udzielonej pozyczce
for (i in score ){
s<-i  

for (j in 1: ncol(data2cutoff)) {
  data2cutoff$pred.cut<-ifelse(data2cutoff$predicted.train<s,0,1)
  }
  conf.matrix<-matrix(0,2,2)
  conf.matrix<-table(data2cutoff$real, data2cutoff$pred.cut)
  df.tresholds[df.tresholds$treshold==i,"ACC"]<-sum(diag(conf.matrix))/sum(conf.matrix)
  df.tresholds[df.tresholds$treshold==i,"TP"]<-conf.matrix[2,2]
  df.tresholds[df.tresholds$treshold==i,"TN"]<-conf.matrix[1,1]
  df.tresholds[df.tresholds$treshold==i,"FP"]<-conf.matrix[1,2]
  df.tresholds[df.tresholds$treshold==i,"FN"]<-conf.matrix[2,1]
  df.tresholds[df.tresholds$treshold==i,"Gain"]<-(conf.matrix[2,2]*pozyczka*profit)-(conf.matrix[1,2]*pozyczka*LGD)
}

(cut.point<-df.tresholds[which.max(df.tresholds$Gain),"treshold"])

ggplot(data=df.tresholds, aes(x=treshold, y=Gain))+geom_line()+geom_vline(xintercept = cut.point)

