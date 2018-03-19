# Matthew Schwartzer

# Required packages
require(ggplot2)

# Data
trueClass = matrix(c('1', '1', '0', '0', '1', '1', '0', '0', '1', '0'), ncol = 10)
m1 = matrix(c(0.73, 0.69, 0.44, 0.55, 0.67, 0.47, 0.08, 0.15, 0.45, 0.35), ncol=10)
m2 = matrix(c(0.61, 0.03, 0.68, 0.31, 0.45, 0.09, 0.38, 0.05, 0.01, 0.04), ncol=10)

# Function to return sensitivity and specificity
rocdata = function(data,trueClass,t){

eclass = matrix(, ncol = 10, nrow=100)
for (iii in 1:100){
for (ii in 1:10){
  eclass[iii,ii] = if (data[,ii] > (iii/100)) 1 else 0
}
}

tp = matrix(, ncol = 10, nrow=100)
for (iii in 1:100){
  for (ii in 1:10)
  tp[iii,ii] = if (eclass[iii,ii] == 1 & trueClass[,ii] == 1) 1 else 0
}

fn = matrix(, ncol = 10, nrow=100)
for (iii in 1:100){
  for (ii in 1:10)
    fn[iii,ii] = if (eclass[iii,ii] == 0 & trueClass[,ii] == 1) 1 else 0
}

tn = matrix(, ncol = 10, nrow=100)
for (iii in 1:100){
  for (ii in 1:10)
    tn[iii,ii] = if (eclass[iii,ii] == 0 & trueClass[,ii] == 0) 1 else 0
}

fp = matrix(, ncol = 10, nrow=100)
for (iii in 1:100){
  for (ii in 1:10)
    fp[iii,ii] = if (eclass[iii,ii] == 1 & trueClass[,ii] == 0) 1 else 0
}

sensitivity = matrix(, ncol = 1, nrow=100)
for (iii in 1:100){
    sensitivity[iii,] = sum(tp[iii,])/(sum(tp[iii,])+sum(fn[iii,]))
}

specificity = matrix(, ncol = 1, nrow=100)
for (iii in 1:100){
  specificity[iii,] = sum(tn[iii,])/(sum(tn[iii,])+sum(fp[iii,]))
}

recall = sum(tp[(t*100),])/(sum(tp[(t*100),])+sum(fn[(t*100),]))
precision = sum(tp[(t*100),])/(sum(tp[(t*100),])+sum(fp[(t*100),]))
fScore = 2 * ((recall * precision)/(recall + precision))

return(list(sensitivity=sensitivity,specificity=specificity,recall=recall,precision=precision,fScore=fScore))
}

# Call rocdata function and store results
m1data = rocdata(m1,trueClass,.5)
m2data = rocdata(m2,trueClass,.5)

partd1 = rocdata(m1,trueClass,.1)
partd2 = rocdata(m1,trueClass,.1)

m1roccurve = ggplot(as.data.frame(m1data), aes(x=as.data.frame((1-m1data$sensitivity)),y=as.data.frame(m1data$specificity))) + 
  geom_line() + geom_abline(intercept=0,slope = 1) + labs(title="ROC Curve for Model 1")+xlab("False Positive Rate")+
  ylab("True Positive Rate") + xlim(0,1) + ylim(0,1)

m2roccurve = ggplot(as.data.frame(m2data), aes(x=as.data.frame((1-m2data$sensitivity)),y=as.data.frame(m2data$specificity))) + 
geom_line() + geom_abline(intercept=0,slope = 1) + labs(title="ROC Curve for Model 2")+xlab("False Positive Rate")+
  ylab("True Positive Rate") + xlim(0,1) + ylim(0,1)
