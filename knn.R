# Matthew Schwartzer

# Required Packages
require(readr)
require(philentropy)
require(ggthemes)

# Import Data (SAVED LINKS TO WORKING DIRECTORY), must run crossValidation.R first to create training and testing datasets
#https://archive.ics.uci.edu/ml/datasets/car+evaluation
car = read.table("car.txt", header = FALSE, sep=",")

#https://archive.ics.uci.edu/ml/datasets/Credit+Approval
credit = read.table("credit.txt", header = FALSE, sep=",")

# https://archive.ics.uci.edu/ml/datasets/ionosphere
iono = read.csv("Ionosphere.csv", header = FALSE)

# KNN Function for Car dataset
matt_knn_car = function(train,test,k){
  
# Mode Function  
Mode <- function(x) {
  modex = unique(x)
  modex[which.max(tabulate(match(x, modex)))]
  }

# Data dimensions
trainrows = nrow(train)
testrows = nrow(test)

# Class label
trainlab = train[,7]
testlab = as.matrix(test[,7],nrow=testrows)

train = train[,-7]
test = test[,-7]

# Combine for feature scaling
train = rbind(train,test)
trainrowstemp = nrow(train)

buyingvhigh=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  buyingvhigh[p] = if (train[p,1] == "vhigh") {1} else {0}
}

buyinghigh=matrix(,nrow=trainrowstemp)
  for (p in 1:trainrowstemp){
    buyinghigh[p] = if (train[p,1] == "high") {1} else {0}
  }

buyingmed=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  buyingmed[p] = if (train[p,1] == "med") {1} else {0}
}

buyinglow=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  buyinglow[p] = if (train[p,1] == "low") {1} else {0}
}

maintvhigh=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  maintvhigh[p] = if (train[p,2] == "vhigh") {1} else {0}
}

mainthigh=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  mainthigh[p] = if (train[p,2] == "high") {1} else {0}
}

maintmed=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  maintmed[p] = if (train[p,2] == "med") {1} else {0}
}

maintlow=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  maintlow[p] = if (train[p,2] == "low") {1} else {0}
}

doors2=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  doors2[p] = if (train[p,3] == 2) {1} else {0}
}

doors3=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  doors3[p] = if (train[p,3] == 3) {1} else {0}
}

doors4=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  doors4[p] = if (train[p,3] == 4) {1} else {0}
}

doors5more=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  doors5more[p] = if (train[p,3] == "5more") {1} else {0}
}

persons2=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  persons2[p] = if (train[p,4] == 2) {1} else {0}
}

persons4=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  persons4[p] = if (train[p,4] == 4) {1} else {0}
}

personsmore=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  personsmore[p] = if (train[p,4] == "more") {1} else {0}
}

lugbootsmall=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  lugbootsmall[p] = if (train[p,5] == "small") {1} else {0}
}

lugbootmed=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  lugbootmed[p] = if (train[p,5] == "med") {1} else {0}
}

lugbootbig=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  lugbootbig[p] = if (train[p,5] == "big") {1} else {0}
}

safteylow=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  safteylow[p] = if (train[p,6] == "low") {1} else {0}
}

safteymed=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  safteymed[p] = if (train[p,6] == "med") {1} else {0}
}

safteyhigh=matrix(,nrow=trainrowstemp)
for (p in 1:trainrowstemp){
  safteyhigh[p] = if (train[p,6] == "high") {1} else {0}
}

car_knn_data = cbind(buyingvhigh,buyinghigh,buyingmed,buyinglow,maintvhigh,mainthigh,maintmed,maintlow,doors2,doors3,
                     doors4,doors5more,persons2,persons4,personsmore,lugbootsmall,lugbootmed,lugbootbig,safteylow,
                     safteymed,safteyhigh)

train2 = car_knn_data[1:trainrows,]
test2 = car_knn_data[(1+trainrows):(trainrows+testrows),]

knnEuclidian = matrix(,nrow=trainrows,ncol=testrows)
knnManhattan = matrix(,nrow=trainrows,ncol=testrows)
for (te in 1:testrows){  
  for (tr in 1:trainrows){
  knnEuclidian[tr,te] = dist(rbind(train2[tr,],test2[te,]),method = "euclidian")
  knnManhattan[tr,te] = dist(rbind(train2[tr,],test2[te,]),method = "manhattan")
  }
}

knnEuclidian = cbind(knnEuclidian,trainlab)
knnEuclidianPred = matrix(,nrow=testrows,ncol = k)
for (n in 1:testrows){
  tempknn = knnEuclidian[order(knnEuclidian[,n]),]
knnEuclidianPred[n,] = tempknn[1:k,(testrows+1)]
}
knnEuclidianPred[knnEuclidianPred==1] = "acc"
knnEuclidianPred[knnEuclidianPred==2] = "good"
knnEuclidianPred[knnEuclidianPred==3]<- "unacc"
knnEuclidianPred[knnEuclidianPred==4]<- "vgood"

euclidianPred = matrix(,nrow=testrows)
for (h in 1:testrows){
  euclidianPred[h] = Mode(knnEuclidianPred[h,])
}

euclidianError = matrix(,nrow=testrows)
for (r in 1:testrows){
  euclidianError[r] = euclidianPred[r] == testlab[r]
}
euclidianError = 1-(sum(euclidianError)/testrows)

knnManhattan = cbind(knnManhattan,trainlab)
knnManhattanPred = matrix(,nrow=testrows,ncol = k)
for (n in 1:testrows){
  tempknn = knnManhattan[order(knnManhattan[,n]),]
  knnManhattanPred[n,] = tempknn[1:k,(testrows+1)]
}
knnManhattanPred[knnManhattanPred==1] = "acc"
knnManhattanPred[knnManhattanPred==2] = "good"
knnManhattanPred[knnManhattanPred==3]<- "unacc"
knnManhattanPred[knnManhattanPred==4]<- "vgood"

manhattanPred = matrix(,nrow=testrows)
for (h in 1:testrows){
  manhattanPred[h] = Mode(knnManhattanPred[h,])
}

manhattanError = matrix(,nrow=testrows)
for (r in 1:testrows){
  manhattanError[r] = manhattanPred[r] == testlab[r]
}
manhattanError = 1-(sum(manhattanError)/testrows)

return(list(euclidianPred=euclidianPred,manhattanPred=manhattanPred,euclidianError=euclidianError,
            manhattanError=manhattanError))
}

# k = 1
knncar11 = matt_knn_car(cardata$train1,cardata$test1,1)
knncar21 = matt_knn_car(cardata$train2,cardata$test2,1)
knncar31 = matt_knn_car(cardata$train3,cardata$test3,1)
knncar41 = matt_knn_car(cardata$train4,cardata$test4,1)
knncar51 = matt_knn_car(cardata$train5,cardata$test5,1)
# k = 2 
knncar12 = matt_knn_car(cardata$train1,cardata$test1,2)
knncar22 = matt_knn_car(cardata$train2,cardata$test2,2)
knncar32 = matt_knn_car(cardata$train3,cardata$test3,2)
knncar42 = matt_knn_car(cardata$train4,cardata$test4,2)
knncar52 = matt_knn_car(cardata$train5,cardata$test5,2)
# k = 3
knncar13 = matt_knn_car(cardata$train1,cardata$test1,3)
knncar23 = matt_knn_car(cardata$train2,cardata$test2,3)
knncar33 = matt_knn_car(cardata$train3,cardata$test3,3)
knncar43 = matt_knn_car(cardata$train4,cardata$test4,3)
knncar53 = matt_knn_car(cardata$train5,cardata$test5,3)
# k = 4
knncar14 = matt_knn_car(cardata$train1,cardata$test1,4)
knncar24 = matt_knn_car(cardata$train2,cardata$test2,4)
knncar34 = matt_knn_car(cardata$train3,cardata$test3,4)
knncar44 = matt_knn_car(cardata$train4,cardata$test4,4)
knncar54 = matt_knn_car(cardata$train5,cardata$test5,4)
# k = 5
knncar15 = matt_knn_car(cardata$train1,cardata$test1,5)
knncar25 = matt_knn_car(cardata$train2,cardata$test2,5)
knncar35 = matt_knn_car(cardata$train3,cardata$test3,5)
knncar45 = matt_knn_car(cardata$train4,cardata$test4,5)
knncar55 = matt_knn_car(cardata$train5,cardata$test5,5)

# Manhattan
carmanhattanerror1 = sum(knncar11$manhattanError,knncar21$manhattanError,knncar31$manhattanError,
                            knncar41$manhattanError,knncar51$manhattanError)/5
carmanhattanerror2 = sum(knncar12$manhattanError,knncar22$manhattanError,knncar32$manhattanError,
                            knncar42$manhattanError,knncar52$manhattanError)/5
carmanhattanerror3 = sum(knncar13$manhattanError,knncar23$manhattanError,knncar33$manhattanError,
                            knncar43$manhattanError,knncar53$manhattanError)/5
carmanhattanerror4 = sum(knncar14$manhattanError,knncar24$manhattanError,knncar34$manhattanError,
                            knncar44$manhattanError,knncar54$manhattanError)/5
carmanhattanerror5 = sum(knncar15$manhattanError,knncar25$manhattanError,knncar35$manhattanError,
                            knncar45$manhattanError,knncar55$manhattanError)/5
carmanhattanerror = as.data.frame(cbind(c(1,2,3,4,5),rbind(carmanhattanerror1,carmanhattanerror2,
                                                              carmanhattanerror3,carmanhattanerror4,carmanhattanerror5)))
carmanhattanerrorplot = ggplot(carmanhattanerror, aes(x=carmanhattanerror[,1],y=carmanhattanerror[,2])) + 
  geom_line() + theme_wsj() + theme(axis.title=element_text(size=20)) +
  labs(title="Misclassification Rate \nManhattan Distance",x="k",y="Error Rate")

# Euclidian
careuclidianerror1 = sum(knncar11$euclidianError,knncar21$euclidianError,knncar31$euclidianError,
                            knncar41$euclidianError,knncar51$euclidianError)/5
careuclidianerror2 = sum(knncar12$euclidianError,knncar22$euclidianError,knncar32$euclidianError,
                            knncar42$euclidianError,knncar52$euclidianError)/5
careuclidianerror3 = sum(knncar13$euclidianError,knncar23$euclidianError,knncar33$euclidianError,
                            knncar43$euclidianError,knncar53$euclidianError)/5
careuclidianerror4 = sum(knncar14$euclidianError,knncar24$euclidianError,knncar34$euclidianError,
                            knncar44$euclidianError,knncar54$euclidianError)/5
careuclidianerror5 = sum(knncar15$euclidianError,knncar25$euclidianError,knncar35$euclidianError,
                            knncar45$euclidianError,knncar55$euclidianError)/5
careuclidianerror = as.data.frame(cbind(c(1,2,3,4,5),rbind(careuclidianerror1,careuclidianerror2,
                                                              careuclidianerror3,careuclidianerror4,careuclidianerror5)))
careuclidianerrorplot = ggplot(careuclidianerror, aes(x=careuclidianerror[,1],y=careuclidianerror[,2])) + 
  geom_line() + theme_wsj() + theme(axis.title=element_text(size=20)) +
  labs(title="Misclassification Rate \nEuclidian Distance",x="k",y="Error Rate")

###################################################################################################################



###################################################################################################################

matt_knn_credit = function(train,test,k){
  
  # Mode Function  
  Mode = function(x) {
    modex = unique(x)
    modex[which.max(tabulate(match(x, modex)))]
  }
  
  # Data dimensions
  trainrows = nrow(train)
  testrows = nrow(test)
  
  # Class label
  trainlab = train[,16]
  testlab = as.matrix(test[,16],nrow=testrows)
  
  train = train[,-16]
  test = test[,-16]

# Combine for feature scaling
  train1 = rbind(train,test)
  trainrowstemp = nrow(train1)
  
  # Create numeric data
  train1[,1] = ifelse(train1[,1] == "a", 1, 2)
  train1[,1] = as.numeric(train1[,1])
  
  train1[,2] = as.numeric(train1[,2])
  
  train1[,3] = as.numeric(train1[,3])
  
  train1[,4] = as.numeric(train1[,4])
  
  train1[,5] = ifelse(train1[,5] == "g", 1, ifelse(train1[,5] == "gg", 2, 3))
  train1[,5] = as.numeric(train1[,5])
  
  train1[,6] = ifelse(train1[,6]=="aa",1, ifelse(train1[,6]=="c",2, ifelse(train1[,6]=="cc",3, ifelse(train1[,6]=="d",4,
                ifelse(train1[,6]=="e",5, ifelse(train1[,6]=="ff",6, ifelse(train1[,6]=="i",7, ifelse(train1[,6]=="j",8,
                ifelse(train1[,6]=="k",9, ifelse(train1[,6]=="m",10, ifelse(train1[,6]=="q",11, ifelse(train1[,6]=="r",12,
                ifelse(train1[,6]=="w",13,14)))))))))))))
  train1[,6] = as.numeric(train1[,6])
  
  train1[,7] = ifelse(train1[,7]=="v",1, ifelse(train1[,7]=="h",2, ifelse(train1[,7]=="bb",3, ifelse(train1[,7]=="ff",4,
                ifelse(train1[,7]=="j",5, ifelse(train1[,7]=="z",6, ifelse(train1[,7]=="o",7, ifelse(train1[,7]=="dd",8,9))))))))
  train1[,7] = as.numeric(train1[,7])
  
  train1[,8] = as.numeric(train1[,8])
  
  train1[,9] = ifelse(train1[,9]=="t",1,2)
  train1[,9] = as.numeric(train1[,9])
  
  train1[,10] = ifelse(train1[,10]=="t",1,2)
  train1[,10] = as.numeric(train1[,10])
  
  train1[,11] = as.numeric(train1[,11])
  
  train1[,12] = ifelse(train1[,12]=="t",1,2)
  train1[,12] = as.numeric(train1[,12])
  
  train1[,13] = ifelse(train1[,13]=="g",1,ifelse(train1[,13]=="s",2,3))
  train1[,13] = as.numeric(train1[,13])
  
  train1[,14] = as.numeric(train1[,14])
  
  train1[,15] = as.numeric(train1[,15])
  
train2 = train1[1:trainrows,]
test2 = train1[(1+trainrows):(trainrows+testrows),]
  
  knnEuclidian = matrix(,nrow=trainrows,ncol=testrows)
  knnManhattan = matrix(,nrow=trainrows,ncol=testrows)
  for (te in 1:testrows){  
    for (tr in 1:trainrows){
      knnEuclidian[tr,te] = dist(rbind(train2[tr,],test2[te,]),method = "euclidian")
      knnManhattan[tr,te] = dist(rbind(train2[tr,],test2[te,]),method = "manhattan")
    }
  }
  
  knnEuclidian = cbind(knnEuclidian,trainlab)
  knnEuclidianPred = matrix(,nrow=testrows,ncol = k)
  for (n in 1:testrows){
    tempknn = knnEuclidian[order(knnEuclidian[,n]),]
    knnEuclidianPred[n,] = tempknn[1:k,(testrows+1)]
  }
  knnEuclidianPred[knnEuclidianPred==1] = "-"
  knnEuclidianPred[knnEuclidianPred==2] = "+"
  
  euclidianPred = matrix(,nrow=testrows)
  for (h in 1:testrows){
    euclidianPred[h] = Mode(knnEuclidianPred[h,])
  }
  
  euclidianError = matrix(,nrow=testrows)
  for (r in 1:testrows){
    euclidianError[r] = euclidianPred[r] == testlab[r]
  }
  euclidianError = 1-(sum(euclidianError)/testrows)
  
  knnManhattan = cbind(knnManhattan,trainlab)
  knnManhattanPred = matrix(,nrow=testrows,ncol = k)
  for (n in 1:testrows){
    tempknn = knnManhattan[order(knnManhattan[,n]),]
    knnManhattanPred[n,] = tempknn[1:k,(testrows+1)]
  }
  knnManhattanPred[knnManhattanPred==1] = "-"
  knnManhattanPred[knnManhattanPred==2] = "+"
  
  manhattanPred = matrix(,nrow=testrows)
  for (h in 1:testrows){
    manhattanPred[h] = Mode(knnManhattanPred[h,])
  }
  
  manhattanError = matrix(,nrow=testrows)
  for (r in 1:testrows){
    manhattanError[r] = manhattanPred[r] == testlab[r]
  }
  manhattanError = 1-(sum(manhattanError)/testrows)
  
  return(list(euclidianPred=euclidianPred,manhattanPred=manhattanPred,euclidianError=euclidianError,
              manhattanError=manhattanError))
}

# k = 1
knncredit11 = matt_knn_credit(creditdata$train1,creditdata$test1,1)
knncredit21 = matt_knn_credit(creditdata$train2,creditdata$test2,1)
knncredit31 = matt_knn_credit(creditdata$train3,creditdata$test3,1)
knncredit41 = matt_knn_credit(creditdata$train4,creditdata$test4,1)
knncredit51 = matt_knn_credit(creditdata$train5,creditdata$test5,1)
# k = 2 
knncredit12 = matt_knn_credit(creditdata$train1,creditdata$test1,2)
knncredit22 = matt_knn_credit(creditdata$train2,creditdata$test2,2)
knncredit32 = matt_knn_credit(creditdata$train3,creditdata$test3,2)
knncredit42 = matt_knn_credit(creditdata$train4,creditdata$test4,2)
knncredit52 = matt_knn_credit(creditdata$train5,creditdata$test5,2)
# k = 3
knncredit13 = matt_knn_credit(creditdata$train1,creditdata$test1,3)
knncredit23 = matt_knn_credit(creditdata$train2,creditdata$test2,3)
knncredit33 = matt_knn_credit(creditdata$train3,creditdata$test3,3)
knncredit43 = matt_knn_credit(creditdata$train4,creditdata$test4,3)
knncredit53 = matt_knn_credit(creditdata$train5,creditdata$test5,3)
# k = 4
knncredit14 = matt_knn_credit(creditdata$train1,creditdata$test1,4)
knncredit24 = matt_knn_credit(creditdata$train2,creditdata$test2,4)
knncredit34 = matt_knn_credit(creditdata$train3,creditdata$test3,4)
knncredit44 = matt_knn_credit(creditdata$train4,creditdata$test4,4)
knncredit54 = matt_knn_credit(creditdata$train5,creditdata$test5,4)
# k = 5
knncredit15 = matt_knn_credit(creditdata$train1,creditdata$test1,5)
knncredit25 = matt_knn_credit(creditdata$train2,creditdata$test2,5)
knncredit35 = matt_knn_credit(creditdata$train3,creditdata$test3,5)
knncredit45 = matt_knn_credit(creditdata$train4,creditdata$test4,5)
knncredit55 = matt_knn_credit(creditdata$train5,creditdata$test5,5)

# Manhattan
creditmanhattanerror1 = sum(knncredit11$manhattanError,knncredit21$manhattanError,knncredit31$manhattanError,
                          knncredit41$manhattanError,knncredit51$manhattanError)/5
creditmanhattanerror2 = sum(knncredit12$manhattanError,knncredit22$manhattanError,knncredit32$manhattanError,
                          knncredit42$manhattanError,knncredit52$manhattanError)/5
creditmanhattanerror3 = sum(knncredit13$manhattanError,knncredit23$manhattanError,knncredit33$manhattanError,
                          knncredit43$manhattanError,knncredit53$manhattanError)/5
creditmanhattanerror4 = sum(knncredit14$manhattanError,knncredit24$manhattanError,knncredit34$manhattanError,
                          knncredit44$manhattanError,knncredit54$manhattanError)/5
creditmanhattanerror5 = sum(knncredit15$manhattanError,knncredit25$manhattanError,knncredit35$manhattanError,
                          knncredit45$manhattanError,knncredit55$manhattanError)/5
creditmanhattanerror = as.data.frame(cbind(c(1,2,3,4,5),rbind(creditmanhattanerror1,creditmanhattanerror2,
                                                            creditmanhattanerror3,creditmanhattanerror4,creditmanhattanerror5)))
creditmanhattanerrorplot = ggplot(creditmanhattanerror, aes(x=creditmanhattanerror[,1],y=creditmanhattanerror[,2])) + 
  geom_line() + theme_wsj() + theme(axis.title=element_text(size=20)) +
  labs(title="Misclassification Rate \nManhattan Distance",x="k",y="Error Rate")

# Euclidian
crediteuclidianerror1 = sum(knncredit11$euclidianError,knncredit21$euclidianError,knncredit31$euclidianError,
                          knncredit41$euclidianError,knncredit51$euclidianError)/5
crediteuclidianerror2 = sum(knncredit12$euclidianError,knncredit22$euclidianError,knncredit32$euclidianError,
                          knncredit42$euclidianError,knncredit52$euclidianError)/5
crediteuclidianerror3 = sum(knncredit13$euclidianError,knncredit23$euclidianError,knncredit33$euclidianError,
                          knncredit43$euclidianError,knncredit53$euclidianError)/5
crediteuclidianerror4 = sum(knncredit14$euclidianError,knncredit24$euclidianError,knncredit34$euclidianError,
                          knncredit44$euclidianError,knncredit54$euclidianError)/5
crediteuclidianerror5 = sum(knncredit15$euclidianError,knncredit25$euclidianError,knncredit35$euclidianError,
                          knncredit45$euclidianError,knncredit55$euclidianError)/5
crediteuclidianerror = as.data.frame(cbind(c(1,2,3,4,5),rbind(crediteuclidianerror1,crediteuclidianerror2,
                                                            crediteuclidianerror3,crediteuclidianerror4,crediteuclidianerror5)))
crediteuclidianerrorplot = ggplot(crediteuclidianerror, aes(x=crediteuclidianerror[,1],y=crediteuclidianerror[,2])) + 
  geom_line() + theme_wsj() + theme(axis.title=element_text(size=20)) +
  labs(title="Misclassification Rate \nEuclidian Distance",x="k",y="Error Rate")

###################################################################################################################



###################################################################################################################

matt_knn_iono = function(train,test,k){
  
# Mode Function  
Mode = function(x) {
  modex = unique(x)
  modex[which.max(tabulate(match(x, modex)))]
}
  
# Data dimensions
trainrows = nrow(train)
testrows = nrow(test)
  
# Class label
trainlab = train[,34]
testlab = as.matrix(test[,34],nrow=testrows)
  
train = train[,-34]
test = test[,-34]

knnEuclidian = matrix(,nrow=trainrows,ncol=testrows)
knnManhattan = matrix(,nrow=trainrows,ncol=testrows)
for (te in 1:testrows){  
  for (tr in 1:trainrows){
    knnEuclidian[tr,te] = dist(rbind(train[tr,],test[te,]),method = "euclidian")
    knnManhattan[tr,te] = dist(rbind(train[tr,],test[te,]),method = "manhattan")
  }
}
  
knnEuclidian = cbind(knnEuclidian,trainlab)
knnEuclidianPred = matrix(,nrow=testrows,ncol = k)
for (n in 1:testrows){
  tempknn = knnEuclidian[order(knnEuclidian[,n]),]
  knnEuclidianPred[n,] = tempknn[1:k,(testrows+1)]
}
knnEuclidianPred[knnEuclidianPred==1] = "b"
knnEuclidianPred[knnEuclidianPred==2] = "g"

euclidianPred = matrix(,nrow=testrows)
for (h in 1:testrows){
  euclidianPred[h] = Mode(knnEuclidianPred[h,])
}
  
euclidianError = matrix(,nrow=testrows)
for (r in 1:testrows){
  euclidianError[r] = euclidianPred[r] == testlab[r]
}
euclidianError = 1-(sum(euclidianError)/testrows)
  
knnManhattan = cbind(knnManhattan,trainlab)
knnManhattanPred = matrix(,nrow=testrows,ncol = k)
for (n in 1:testrows){
  tempknn = knnManhattan[order(knnManhattan[,n]),]
  knnManhattanPred[n,] = tempknn[1:k,(testrows+1)]
}
knnManhattanPred[knnManhattanPred==1] = "b"
knnManhattanPred[knnManhattanPred==2] = "g"

manhattanPred = matrix(,nrow=testrows)
for (h in 1:testrows){
  manhattanPred[h] = Mode(knnManhattanPred[h,])
}
  
manhattanError = matrix(,nrow=testrows)
for (r in 1:testrows){
  manhattanError[r] = manhattanPred[r] == testlab[r]
}
manhattanError = 1-(sum(manhattanError)/testrows)
  
return(list(euclidianPred=euclidianPred,manhattanPred=manhattanPred,euclidianError=euclidianError,
              manhattanError=manhattanError))
}

knniono11 = matt_knn_iono(ionodata$train1,ionodata$test1,1)
knniono21 = matt_knn_iono(ionodata$train2,ionodata$test2,1)
knniono31 = matt_knn_iono(ionodata$train3,ionodata$test3,1)
knniono41 = matt_knn_iono(ionodata$train4,ionodata$test4,1)
knniono51 = matt_knn_iono(ionodata$train5,ionodata$test5,1)
# k = 2 
knniono12 = matt_knn_iono(ionodata$train1,ionodata$test1,2)
knniono22 = matt_knn_iono(ionodata$train2,ionodata$test2,2)
knniono32 = matt_knn_iono(ionodata$train3,ionodata$test3,2)
knniono42 = matt_knn_iono(ionodata$train4,ionodata$test4,2)
knniono52 = matt_knn_iono(ionodata$train5,ionodata$test5,2)
# k = 3
knniono13 = matt_knn_iono(ionodata$train1,ionodata$test1,3)
knniono23 = matt_knn_iono(ionodata$train2,ionodata$test2,3)
knniono33 = matt_knn_iono(ionodata$train3,ionodata$test3,3)
knniono43 = matt_knn_iono(ionodata$train4,ionodata$test4,3)
knniono53 = matt_knn_iono(ionodata$train5,ionodata$test5,3)
# k = 4
knniono14 = matt_knn_iono(ionodata$train1,ionodata$test1,4)
knniono24 = matt_knn_iono(ionodata$train2,ionodata$test2,4)
knniono34 = matt_knn_iono(ionodata$train3,ionodata$test3,4)
knniono44 = matt_knn_iono(ionodata$train4,ionodata$test4,4)
knniono54 = matt_knn_iono(ionodata$train5,ionodata$test5,4)
# k = 5
knniono15 = matt_knn_iono(ionodata$train1,ionodata$test1,5)
knniono25 = matt_knn_iono(ionodata$train2,ionodata$test2,5)
knniono35 = matt_knn_iono(ionodata$train3,ionodata$test3,5)
knniono45 = matt_knn_iono(ionodata$train4,ionodata$test4,5)
knniono55 = matt_knn_iono(ionodata$train5,ionodata$test5,5)

# Manhattan
ionomanhattanerror1 = sum(knniono11$manhattanError,knniono21$manhattanError,knniono31$manhattanError,
                            knniono41$manhattanError,knniono51$manhattanError)/5
ionomanhattanerror2 = sum(knniono12$manhattanError,knniono22$manhattanError,knniono32$manhattanError,
                            knniono42$manhattanError,knniono52$manhattanError)/5
ionomanhattanerror3 = sum(knniono13$manhattanError,knniono23$manhattanError,knniono33$manhattanError,
                            knniono43$manhattanError,knniono53$manhattanError)/5
ionomanhattanerror4 = sum(knniono14$manhattanError,knniono24$manhattanError,knniono34$manhattanError,
                            knniono44$manhattanError,knniono54$manhattanError)/5
ionomanhattanerror5 = sum(knniono15$manhattanError,knniono25$manhattanError,knniono35$manhattanError,
                            knniono45$manhattanError,knniono55$manhattanError)/5
ionomanhattanerror = as.data.frame(cbind(c(1,2,3,4,5),rbind(ionomanhattanerror1,ionomanhattanerror2,
                                                              ionomanhattanerror3,ionomanhattanerror4,ionomanhattanerror5)))
ionomanhattanerrorplot = ggplot(ionomanhattanerror, aes(x=ionomanhattanerror[,1],y=ionomanhattanerror[,2])) + 
  geom_line() + theme_wsj() + theme(axis.title=element_text(size=20)) +
  labs(title="Misclassification Rate \nManhattan Distance",x="k",y="Error Rate")

# Euclidian
ionoeuclidianerror1 = sum(knniono11$euclidianError,knniono21$euclidianError,knniono31$euclidianError,
                            knniono41$euclidianError,knniono51$euclidianError)/5
ionoeuclidianerror2 = sum(knniono12$euclidianError,knniono22$euclidianError,knniono32$euclidianError,
                            knniono42$euclidianError,knniono52$euclidianError)/5
ionoeuclidianerror3 = sum(knniono13$euclidianError,knniono23$euclidianError,knniono33$euclidianError,
                            knniono43$euclidianError,knniono53$euclidianError)/5
ionoeuclidianerror4 = sum(knniono14$euclidianError,knniono24$euclidianError,knniono34$euclidianError,
                            knniono44$euclidianError,knniono54$euclidianError)/5
ionoeuclidianerror5 = sum(knniono15$euclidianError,knniono25$euclidianError,knniono35$euclidianError,
                            knniono45$euclidianError,knniono55$euclidianError)/5
ionoeuclidianerror = as.data.frame(cbind(c(1,2,3,4,5),rbind(ionoeuclidianerror1,ionoeuclidianerror2,
                                                              ionoeuclidianerror3,ionoeuclidianerror4,ionoeuclidianerror5)))
ionoeuclidianerrorplot = ggplot(ionoeuclidianerror, aes(x=ionoeuclidianerror[,1],y=ionoeuclidianerror[,2])) + 
  geom_line() + theme_wsj() + theme(axis.title=element_text(size=20)) +
  labs(title="Misclassification Rate \nEuclidian Distance",x="k",y="Error Rate")

  