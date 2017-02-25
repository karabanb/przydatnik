
score<-seq(0.01,1,0.01) ## sekwencja score co 1 pkt

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

for (i in score) {
s<-i  

for (j in 1: ncol(data2cutoff)) {
  data2cutoff$pred.cut<-ifelse(data2cutoff$predicted.train<s,0,1)
}
conf.matrix<-table(data2cutoff$real, data2cutoff$pred.cut)
df.tresholds[df.tresholds$treshold==i,"ACC"]<-sum(diag(conf.matrix))/sum(conf.matrix)

}
