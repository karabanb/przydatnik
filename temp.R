
library(ggplot2)

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

LGD<-0.49
profit<-0.5
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
df.tresholds[df.tresholds$treshold==i,"Gain"]<-(conf.matrix[2,2]*3000*profit)-(conf.matrix[1,2]*3000*LGD)
}

(cut.point<-df.tresholds[which.max(df.tresholds$Gain),"treshold"])

ggplot(data=df.tresholds, aes(x=treshold, y=Gain))+geom_line()+geom_vline(xintercept = cut.point)
