# Matthew Schwartzer

# Required Packages
require(readr)

# Must run crossValidation.R first to obtain training and testing datasets
matt_NB_car = function(train,test){

a1 = table(train[,1],train[,7])
a1dim = as.matrix(dim(a1))
for (a in 1:a1dim[1]) {
  for (aa in 1:a1dim[2]){
    a1[a,aa] = ifelse(a1[a,aa]==0, 1, a1[a,aa])
}
}
a1prob = a1
for (a in 1:a1dim[1]) {
  for (aa in 1:a1dim[2]){
    a1prob[a,aa] = a1[a,aa]/sum(a1[,aa])
  }
}

a2 = table(train[,2],train[,7])
a2dim = as.matrix(dim(a2))
for (a in 1:a2dim[1]) {
  for (aa in 1:a2dim[2]){
    a2[a,aa] = ifelse(a2[a,aa]==0, 1, a2[a,aa])
  }
}
a2prob = a2
for (a in 1:a2dim[1]) {
  for (aa in 1:a2dim[2]){
    a2prob[a,aa] = a2[a,aa]/sum(a2[,aa])
  }
}

a3 = table(train[,3],train[,7])
a3dim = as.matrix(dim(a3))
for (a in 1:a3dim[1]) {
  for (aa in 1:a3dim[2]){
    a3[a,aa] = ifelse(a3[a,aa]==0, 1, a3[a,aa])
  }
}
a3prob = a3
for (a in 1:a3dim[1]) {
  for (aa in 1:a3dim[2]){
    a3prob[a,aa] = a3[a,aa]/sum(a3[,aa])
  }
}

a4 = table(train[,4],train[,7])
a4dim = as.matrix(dim(a4))
for (a in 1:a4dim[1]) {
  for (aa in 1:a4dim[2]){
    a4[a,aa] = ifelse(a4[a,aa]==0, 1, a4[a,aa])
  }
}
a4prob = a4
for (a in 1:a4dim[1]) {
  for (aa in 1:a4dim[2]){
    a4prob[a,aa] = a4[a,aa]/sum(a4[,aa])
  }
}

a5 = table(train[,5],train[,7])
a5dim = as.matrix(dim(a5))
for (a in 1:a5dim[1]) {
  for (aa in 1:a5dim[2]){
    a5[a,aa] = ifelse(a5[a,aa]==0, 1, a5[a,aa])
  }
}
a5prob = a5
for (a in 1:a5dim[1]) {
  for (aa in 1:a5dim[2]){
    a5prob[a,aa] = a5[a,aa]/sum(a5[,aa])
  }
}

a6 = table(train[,6],train[,7])
a6dim = as.matrix(dim(a6))
for (a in 1:a6dim[1]) {
  for (aa in 1:a6dim[2]){
    a6[a,aa] = ifelse(a6[a,aa]==0, 1, a6[a,aa])
  }
}
a6prob = a6
for (a in 1:a6dim[1]) {
  for (aa in 1:a6dim[2]){
    a6prob[a,aa] = a6[a,aa]/sum(a6[,aa])
  }
}

testdim = as.matrix(dim(test))
predictiontable = matrix(,testdim[1])
temperror = matrix(,nrow=testdim[1])
for (repp in 1:testdim[1]){
temp1 = a1prob[test[repp,1],]
temp2 = a2prob[test[repp,2],]
temp3 = a3prob[test[repp,3],]
temp4 = a4prob[test[repp,4],]
temp5 = a5prob[test[repp,5],]
temp6 = a6prob[test[repp,6],]
probs = rbind(temp1,temp2,temp3,temp4,temp5,temp6)

probclass = matrix(,ncol=4)
  for (cll in 1:4){
  probclass[,cll] = probs[1,cll] * probs[2,cll] * probs[3,cll] * probs[4,cll] * probs[5,cll] * probs[6,cll]
  }
colnames(probclass) = c("acc", "good", "unacc", "vgood")
prediction = which.max(probclass)
prediction = if (prediction == 1) {"acc"} else if (prediction == 2) {"good"} else if (prediction == 3) {"unacc"} else {"vgood"}
predictiontable[repp] = prediction
temperror[repp] = predictiontable[repp] == test[repp,7]
}

error = 1-(sum(temperror)/length(temperror))

return(list(predictiontable=predictiontable,error=error))
}

car1 = matt_NB_car(cardata$train1,cardata$test1)
car2 = matt_NB_car(cardata$train2,cardata$test2)
car3 = matt_NB_car(cardata$train3,cardata$test3)
car4 = matt_NB_car(cardata$train4,cardata$test4)
car5 = matt_NB_car(cardata$train5,cardata$test5)

car_err = as.matrix(cbind(car1$error, car2$error, car3$error, car4$error, car5$error))
car_error = barplot(car_err, main="Misclassification Rate \n Car Data Set", xlab="Testing Set Number", 
                       ylab="Error", names.arg=c("1","2","3","4","5"), border="red", 
                       density=c(90, 70, 50, 40, 30, 20, 10), ylim = c(0,.25))

car_avg_error = (car1$error + car2$error + car3$error + car4$error + car5$error)/5
# Average error = .1990919

###################################################################################################################



###################################################################################################################

matt_NB_credit = function(train,test){
  
  trainrows = nrow(train)
  testrows = nrow(test)
  
  credit = rbind(train,test)
  
  credit[,2]=as.numeric(credit[,2])
  v2median=median(credit[,2])
  credit[,2][credit[,2]<v2median]=0
  credit[,2][credit[,2]>=v2median]=1
  credit[,2] = as.factor(credit[,2])
  v3median=median(credit[,3])
  credit[,3][credit[,3]<v3median]=0
  credit[,3][credit[,3]>=v3median]=1
  credit[,3] = as.factor(credit[,3])
  v8median=median(credit[,8])
  credit[,8][credit[,8]<v8median]=0
  credit[,8][credit[,8]>=v8median]=1
  credit[,8] = as.factor(credit[,8])
  v11median=median(credit[,11])+.05
  credit[,11][credit[,11]<v11median]=0
  credit[,11][credit[,11]>=v11median]=1
  credit[,11] = as.factor(credit[,11])
  credit[,14]=as.numeric(credit[,14])
  v14median=median(credit[,14])
  credit[,14][credit[,14]<v14median]=0
  credit[,14][credit[,14]>=v14median]=1
  credit[,14] = as.factor(credit[,14])
  v15median=median(credit[,15])
  credit[,15][credit[,15]<v15median]=0
  credit[,15][credit[,15]>=v15median]=1
  credit[,15] = as.factor(credit[,15])  
  
  train2 = credit[1:trainrows,]
  test2 = credit[(1+trainrows):(trainrows+testrows),]    
  
  a1 = table(train2[,1],train2[,16])
  a1dim = as.matrix(dim(a1))
  for (a in 1:a1dim[1]) {
    for (aa in 1:a1dim[2]){
      a1[a,aa] = ifelse(a1[a,aa]==0, 1, a1[a,aa])
    }
  }
  a1prob = a1
  for (a in 1:a1dim[1]) {
    for (aa in 1:a1dim[2]){
      a1prob[a,aa] = a1[a,aa]/sum(a1[,aa])
    }
  }
  
  a2 = table(train2[,2],train2[,16])
  a2dim = as.matrix(dim(a2))
  for (a in 1:a2dim[1]) {
    for (aa in 1:a2dim[2]){
      a2[a,aa] = ifelse(a2[a,aa]==0, 1, a2[a,aa])
    }
  }
  a2prob = a2
  for (a in 1:a2dim[1]) {
    for (aa in 1:a2dim[2]){
      a2prob[a,aa] = a2[a,aa]/sum(a2[,aa])
    }
  }
  
  a3 = table(train2[,3],train2[,16])
  a3dim = as.matrix(dim(a3))
  for (a in 1:a3dim[1]) {
    for (aa in 1:a3dim[2]){
      a3[a,aa] = ifelse(a3[a,aa]==0, 1, a3[a,aa])
    }
  }
  a3prob = a3
  for (a in 1:a3dim[1]) {
    for (aa in 1:a3dim[2]){
      a3prob[a,aa] = a3[a,aa]/sum(a3[,aa])
    }
  }
  
  a4 = table(train2[,4],train2[,16])
  a4dim = as.matrix(dim(a4))
  for (a in 1:a4dim[1]) {
    for (aa in 1:a4dim[2]){
      a4[a,aa] = ifelse(a4[a,aa]==0, 1, a4[a,aa])
    }
  }
  a4prob = a4
  for (a in 1:a4dim[1]) {
    for (aa in 1:a4dim[2]){
      a4prob[a,aa] = a4[a,aa]/sum(a4[,aa])
    }
  }
  
  a5 = table(train2[,5],train2[,16])
  a5dim = as.matrix(dim(a5))
  for (a in 1:a5dim[1]) {
    for (aa in 1:a5dim[2]){
      a5[a,aa] = ifelse(a5[a,aa]==0, 1, a5[a,aa])
    }
  }
  a5prob = a5
  for (a in 1:a5dim[1]) {
    for (aa in 1:a5dim[2]){
      a5prob[a,aa] = a5[a,aa]/sum(a5[,aa])
    }
  }
  
  a6 = table(train2[,6],train2[,16])
  a6dim = as.matrix(dim(a6))
  for (a in 1:a6dim[1]) {
    for (aa in 1:a6dim[2]){
      a6[a,aa] = ifelse(a6[a,aa]==0, 1, a6[a,aa])
    }
  }
  a6prob = a6
  for (a in 1:a6dim[1]) {
    for (aa in 1:a6dim[2]){
      a6prob[a,aa] = a6[a,aa]/sum(a6[,aa])
    }
  }
  
  a7 = table(train2[,7],train2[,16])
  a7dim = as.matrix(dim(a7))
  for (a in 1:a7dim[1]) {
    for (aa in 1:a7dim[2]){
      a7[a,aa] = ifelse(a7[a,aa]==0, 1, a7[a,aa])
    }
  }
  a7prob = a7
  for (a in 1:a7dim[1]) {
    for (aa in 1:a7dim[2]){
      a7prob[a,aa] = a7[a,aa]/sum(a7[,aa])
    }
  }
  
  a8 = table(train2[,8],train2[,16])
  a8dim = as.matrix(dim(a8))
  for (a in 1:a8dim[1]) {
    for (aa in 1:a8dim[2]){
      a8[a,aa] = ifelse(a8[a,aa]==0, 1, a8[a,aa])
    }
  }
  a8prob = a8
  for (a in 1:a8dim[1]) {
    for (aa in 1:a8dim[2]){
      a8prob[a,aa] = a8[a,aa]/sum(a8[,aa])
    }
  }
  
  a9 = table(train2[,9],train2[,16])
  a9dim = as.matrix(dim(a9))
  for (a in 1:a9dim[1]) {
    for (aa in 1:a9dim[2]){
      a9[a,aa] = ifelse(a9[a,aa]==0, 1, a9[a,aa])
    }
  }
  a9prob = a9
  for (a in 1:a9dim[1]) {
    for (aa in 1:a9dim[2]){
      a9prob[a,aa] = a9[a,aa]/sum(a9[,aa])
    }
  }
  
  a10 = table(train2[,10],train2[,16])
  a10dim = as.matrix(dim(a10))
  for (a in 1:a10dim[1]) {
    for (aa in 1:a10dim[2]){
      a10[a,aa] = ifelse(a10[a,aa]==0, 1, a10[a,aa])
    }
  }
  a10prob = a10
  for (a in 1:a10dim[1]) {
    for (aa in 1:a10dim[2]){
      a10prob[a,aa] = a10[a,aa]/sum(a10[,aa])
    }
  }
  
  a11 = table(train2[,11],train2[,16])
  a11dim = as.matrix(dim(a11))
  for (a in 1:a11dim[1]) {
    for (aa in 1:a11dim[2]){
      a11[a,aa] = ifelse(a11[a,aa]==0, 1, a11[a,aa])
    }
  }
  a11prob = a11
  for (a in 1:a11dim[1]) {
    for (aa in 1:a11dim[2]){
      a11prob[a,aa] = a11[a,aa]/sum(a11[,aa])
    }
  }  
  
  a12 = table(train2[,12],train2[,16])
  a12dim = as.matrix(dim(a12))
  for (a in 1:a12dim[1]) {
    for (aa in 1:a12dim[2]){
      a12[a,aa] = ifelse(a12[a,aa]==0, 1, a12[a,aa])
    }
  }
  a12prob = a12
  for (a in 1:a12dim[1]) {
    for (aa in 1:a12dim[2]){
      a12prob[a,aa] = a12[a,aa]/sum(a12[,aa])
    }
  }
  
  a13 = table(train2[,13],train2[,16])
  a13dim = as.matrix(dim(a13))
  for (a in 1:a13dim[1]) {
    for (aa in 1:a13dim[2]){
      a13[a,aa] = ifelse(a13[a,aa]==0, 1, a13[a,aa])
    }
  }
  a13prob = a13
  for (a in 1:a13dim[1]) {
    for (aa in 1:a13dim[2]){
      a13prob[a,aa] = a13[a,aa]/sum(a13[,aa])
    }
  }
  
  a14 = table(train2[,14],train2[,16])
  a14dim = as.matrix(dim(a14))
  for (a in 1:a14dim[1]) {
    for (aa in 1:a14dim[2]){
      a14[a,aa] = ifelse(a14[a,aa]==0, 1, a14[a,aa])
    }
  }
  a14prob = a14
  for (a in 1:a14dim[1]) {
    for (aa in 1:a14dim[2]){
      a14prob[a,aa] = a14[a,aa]/sum(a14[,aa])
    }
  }
  
  a15 = table(train2[,15],train2[,16])
  a15dim = as.matrix(dim(a15))
  for (a in 1:a15dim[1]) {
    for (aa in 1:a15dim[2]){
      a15[a,aa] = ifelse(a15[a,aa]==0, 1, a15[a,aa])
    }
  }
  a15prob = a15
  for (a in 1:a15dim[1]) {
    for (aa in 1:a15dim[2]){
      a15prob[a,aa] = a15[a,aa]/sum(a15[,aa])
    }
  }
  
test2dim = as.matrix(dim(test2))
predictiontable = matrix(,test2dim[1])
temperror = matrix(,nrow=test2dim[1])
  for (repp in 1:test2dim[1]){
    temp1 = a1prob[test2[repp,1],]
    temp2 = a2prob[test2[repp,2],]
    temp3 = a3prob[test2[repp,3],]
    temp4 = a4prob[test2[repp,4],]
    temp5 = a5prob[test2[repp,5],]
    temp6 = a6prob[test2[repp,6],]
    temp7 = a7prob[test2[repp,7],]
    temp8 = a8prob[test2[repp,8],]
    temp9 = a9prob[test2[repp,9],]
    temp10 = a10prob[test2[repp,10],]
    temp11 = a11prob[test2[repp,11],]
    temp12 = a12prob[test2[repp,12],]
    temp13 = a13prob[test2[repp,13],]
    temp14 = a14prob[test2[repp,14],]
    temp15 = a15prob[test2[repp,15],]
    probs = rbind(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12,temp13,temp14,temp15)
    
    probclass = matrix(,ncol=2)
    for (cll in 1:2){
      probclass[,cll] = probs[1,cll] * probs[2,cll] * probs[3,cll] * probs[4,cll] * probs[5,cll] * probs[6,cll] *
        probs[7,cll] * probs[8,cll] * probs[9,cll] * probs[10,cll] * probs[11,cll] * probs[12,cll] *
        probs[13,cll] * probs[14,cll] * probs[15,cll]
    }
    colnames(probclass) = c("-", "+")
    prediction = which.max(probclass)
    prediction = if (prediction == 1) {"-"} else {"+"}
    predictiontable[repp] = prediction
    temperror[repp] = predictiontable[repp] == test2[repp,16]
  }
  
  error = 1-(sum(temperror)/length(temperror))
  
  return(list(predictiontable=predictiontable,error=error))
}

credit1 = matt_NB_credit(creditdata$train1,creditdata$test1)
credit2 = matt_NB_credit(creditdata$train2,creditdata$test2)
credit3 = matt_NB_credit(creditdata$train3,creditdata$test3)
credit4 = matt_NB_credit(creditdata$train4,creditdata$test4)
credit5 = matt_NB_credit(creditdata$train5,creditdata$test5)

credit_err = as.matrix(cbind(credit1$error, credit2$error, credit3$error, credit4$error, credit5$error))
credit_error = barplot(credit_err, main="Misclassification Rate \n Credit Data Set", xlab="Testing Set Number", 
                       ylab="Error", names.arg=c("1","2","3","4","5"), border="red", 
                       density=c(90, 70, 50, 40, 30, 20, 10), ylim = c(0,.25))

credit_avg_error = (credit1$error + credit2$error + credit3$error + credit4$error + credit5$error)/5
# Average Error = 0.1654257

###################################################################################################################

###################################################################################################################

matt_NB_iono = function(train,test){
  
  trainrows = nrow(train)
  testrows = nrow(test)
  
  iono = rbind(train,test)
  
  v1iono=median(iono[,1])
  iono[,1][iono[,1]<v1iono]=0
  iono[,1][iono[,1]>=v1iono]=1
  iono[,1] = as.factor(iono[,1])
  
  v2iono=median(iono[,2])
  iono[,2][iono[,2]<v2iono]=0
  iono[,2][iono[,2]>=v2iono]=1
  iono[,2] = as.factor(iono[,2])
  
  v3iono=median(iono[,3])
  iono[,3][iono[,3]<v3iono]=0
  iono[,3][iono[,3]>=v3iono]=1
  iono[,3] = as.factor(iono[,3])
  
  v4iono=median(iono[,4])
  iono[,4][iono[,4]<v4iono]=0
  iono[,4][iono[,4]>=v4iono]=1
  iono[,4] = as.factor(iono[,4])
  
  v5iono=median(iono[,5])
  iono[,5][iono[,5]<v5iono]=0
  iono[,5][iono[,5]>=v5iono]=1
  iono[,5] = as.factor(iono[,5])
  
  v6iono=median(iono[,6])
  iono[,6][iono[,6]<v6iono]=0
  iono[,6][iono[,6]>=v6iono]=1
  iono[,6] = as.factor(iono[,6])
  
  v7iono=median(iono[,7])
  iono[,7][iono[,7]<v7iono]=0
  iono[,7][iono[,7]>=v7iono]=1
  iono[,7] = as.factor(iono[,7])
  
  v8iono=median(iono[,8])
  iono[,8][iono[,8]<v8iono]=0
  iono[,8][iono[,8]>=v8iono]=1
  iono[,8] = as.factor(iono[,8])
  
  v9iono=median(iono[,9])
  iono[,9][iono[,9]<v9iono]=0
  iono[,9][iono[,9]>=v9iono]=1
  iono[,9] = as.factor(iono[,9])
  
  v10iono=median(iono[,10])
  iono[,10][iono[,10]<v10iono]=0
  iono[,10][iono[,10]>=v10iono]=1
  iono[,10] = as.factor(iono[,10])
  
  v11iono=median(iono[,11])
  iono[,11][iono[,11]<v11iono]=0
  iono[,11][iono[,11]>=v11iono]=1
  iono[,11] = as.factor(iono[,11])
  
  v12iono=median(iono[,12])
  iono[,12][iono[,12]<v12iono]=0
  iono[,12][iono[,12]>=v12iono]=1
  iono[,12] = as.factor(iono[,12])
  
  v13iono=median(iono[,13])
  iono[,13][iono[,13]<v13iono]=0
  iono[,13][iono[,13]>=v13iono]=1
  iono[,13] = as.factor(iono[,13])
  
  v14iono=median(iono[,14])
  iono[,14][iono[,14]<v14iono]=0
  iono[,14][iono[,14]>=v14iono]=1
  iono[,14] = as.factor(iono[,14])
  
  v15iono=median(iono[,15])
  iono[,15][iono[,15]<v15iono]=0
  iono[,15][iono[,15]>=v15iono]=1
  iono[,15] = as.factor(iono[,15])
  
  v16iono=median(iono[,16])
  iono[,16][iono[,16]<v16iono]=0
  iono[,16][iono[,16]>=v16iono]=1
  iono[,16] = as.factor(iono[,16])
  
  v17iono=median(iono[,17])
  iono[,17][iono[,17]<v17iono]=0
  iono[,17][iono[,17]>=v17iono]=1
  iono[,17] = as.factor(iono[,17])
  
  v18iono=median(iono[,18])
  iono[,18][iono[,18]<v18iono]=0
  iono[,18][iono[,18]>=v18iono]=1
  iono[,18] = as.factor(iono[,18])
  
  v19iono=median(iono[,19])
  iono[,19][iono[,19]<v19iono]=0
  iono[,19][iono[,19]>=v19iono]=1
  iono[,19] = as.factor(iono[,19])
  
  v20iono=median(iono[,20])
  iono[,20][iono[,20]<v20iono]=0
  iono[,20][iono[,20]>=v20iono]=1
  iono[,20] = as.factor(iono[,20])
  
  v21iono=median(iono[,21])
  iono[,21][iono[,21]<v21iono]=0
  iono[,21][iono[,21]>=v21iono]=1
  iono[,21] = as.factor(iono[,21])
  
  v22iono=median(iono[,22])
  iono[,22][iono[,22]<v22iono]=0
  iono[,22][iono[,22]>=v22iono]=1
  iono[,22] = as.factor(iono[,22])
  
  v23iono=median(iono[,23])
  iono[,23][iono[,23]<v23iono]=0
  iono[,23][iono[,23]>=v23iono]=1
  iono[,23] = as.factor(iono[,23])
  
  v24iono=median(iono[,24])
  iono[,24][iono[,24]<v24iono]=0
  iono[,24][iono[,24]>=v24iono]=1
  iono[,24] = as.factor(iono[,24])
  
  v25iono=median(iono[,25])
  iono[,25][iono[,25]<v25iono]=0
  iono[,25][iono[,25]>=v25iono]=1
  iono[,25] = as.factor(iono[,25])
  
  v26iono=median(iono[,26])
  iono[,26][iono[,26]<v26iono]=0
  iono[,26][iono[,26]>=v26iono]=1
  iono[,26] = as.factor(iono[,26])
  
  v27iono=median(iono[,27])
  iono[,27][iono[,27]<v27iono]=0
  iono[,27][iono[,27]>=v27iono]=1
  iono[,27] = as.factor(iono[,27])
  
  v28iono=median(iono[,28])
  iono[,28][iono[,28]<v28iono]=0
  iono[,28][iono[,28]>=v28iono]=1
  iono[,28] = as.factor(iono[,28])
  
  v29iono=median(iono[,29])
  iono[,29][iono[,29]<v29iono]=0
  iono[,29][iono[,29]>=v29iono]=1
  iono[,29] = as.factor(iono[,29])
  
  v30iono=median(iono[,30])
  iono[,30][iono[,30]<v30iono]=0
  iono[,30][iono[,30]>=v30iono]=1
  iono[,30] = as.factor(iono[,30])
  
  v31iono=median(iono[,31])
  iono[,31][iono[,31]<v31iono]=0
  iono[,31][iono[,31]>=v31iono]=1
  iono[,31] = as.factor(iono[,31])
  
  v32iono=median(iono[,32])
  iono[,32][iono[,32]<v32iono]=0
  iono[,32][iono[,32]>=v32iono]=1
  iono[,32] = as.factor(iono[,32])
  
  v33iono=median(iono[,33])
  iono[,33][iono[,33]<v33iono]=0
  iono[,33][iono[,33]>=v33iono]=1
  iono[,33] = as.factor(iono[,33])
  
  
train2 = iono[1:trainrows,]
test2 = iono[(1+trainrows):(trainrows+testrows),]  

  a1 = table(train2[,1],train2[,34])
  a1dim = as.matrix(dim(a1))
  for (a in 1:a1dim[1]) {
    for (aa in 1:a1dim[2]){
      a1[a,aa] = ifelse(a1[a,aa]==0, 1, a1[a,aa])
    }
  }
  a1prob = a1
  for (a in 1:a1dim[1]) {
    for (aa in 1:a1dim[2]){
      a1prob[a,aa] = a1[a,aa]/sum(a1[,aa])
    }
  }
  
  a2 = table(train2[,2],train2[,34])
  a2dim = as.matrix(dim(a2))
  for (a in 1:a2dim[1]) {
    for (aa in 1:a2dim[2]){
      a2[a,aa] = ifelse(a2[a,aa]==0, 1, a2[a,aa])
    }
  }
  a2prob = a2
  for (a in 1:a2dim[1]) {
    for (aa in 1:a2dim[2]){
      a2prob[a,aa] = a2[a,aa]/sum(a2[,aa])
    }
  }
  
  a3 = table(train2[,3],train2[,34])
  a3dim = as.matrix(dim(a3))
  for (a in 1:a3dim[1]) {
    for (aa in 1:a3dim[2]){
      a3[a,aa] = ifelse(a3[a,aa]==0, 1, a3[a,aa])
    }
  }
  a3prob = a3
  for (a in 1:a3dim[1]) {
    for (aa in 1:a3dim[2]){
      a3prob[a,aa] = a3[a,aa]/sum(a3[,aa])
    }
  }
  
  a4 = table(train2[,4],train2[,34])
  a4dim = as.matrix(dim(a4))
  for (a in 1:a4dim[1]) {
    for (aa in 1:a4dim[2]){
      a4[a,aa] = ifelse(a4[a,aa]==0, 1, a4[a,aa])
    }
  }
  a4prob = a4
  for (a in 1:a4dim[1]) {
    for (aa in 1:a4dim[2]){
      a4prob[a,aa] = a4[a,aa]/sum(a4[,aa])
    }
  }
  
  a5 = table(train2[,5],train2[,34])
  a5dim = as.matrix(dim(a5))
  for (a in 1:a5dim[1]) {
    for (aa in 1:a5dim[2]){
      a5[a,aa] = ifelse(a5[a,aa]==0, 1, a5[a,aa])
    }
  }
  a5prob = a5
  for (a in 1:a5dim[1]) {
    for (aa in 1:a5dim[2]){
      a5prob[a,aa] = a5[a,aa]/sum(a5[,aa])
    }
  }
  
  a6 = table(train2[,6],train2[,34])
  a6dim = as.matrix(dim(a6))
  for (a in 1:a6dim[1]) {
    for (aa in 1:a6dim[2]){
      a6[a,aa] = ifelse(a6[a,aa]==0, 1, a6[a,aa])
    }
  }
  a6prob = a6
  for (a in 1:a6dim[1]) {
    for (aa in 1:a6dim[2]){
      a6prob[a,aa] = a6[a,aa]/sum(a6[,aa])
    }
  }
  
  a7 = table(train2[,7],train2[,34])
  a7dim = as.matrix(dim(a7))
  for (a in 1:a7dim[1]) {
    for (aa in 1:a7dim[2]){
      a7[a,aa] = ifelse(a7[a,aa]==0, 1, a7[a,aa])
    }
  }
  a7prob = a7
  for (a in 1:a7dim[1]) {
    for (aa in 1:a7dim[2]){
      a7prob[a,aa] = a7[a,aa]/sum(a7[,aa])
    }
  }
  
  a8 = table(train2[,8],train2[,34])
  a8dim = as.matrix(dim(a8))
  for (a in 1:a8dim[1]) {
    for (aa in 1:a8dim[2]){
      a8[a,aa] = ifelse(a8[a,aa]==0, 1, a8[a,aa])
    }
  }
  a8prob = a8
  for (a in 1:a8dim[1]) {
    for (aa in 1:a8dim[2]){
      a8prob[a,aa] = a8[a,aa]/sum(a8[,aa])
    }
  }
  
  a9 = table(train2[,9],train2[,34])
  a9dim = as.matrix(dim(a9))
  for (a in 1:a9dim[1]) {
    for (aa in 1:a9dim[2]){
      a9[a,aa] = ifelse(a9[a,aa]==0, 1, a9[a,aa])
    }
  }
  a9prob = a9
  for (a in 1:a9dim[1]) {
    for (aa in 1:a9dim[2]){
      a9prob[a,aa] = a9[a,aa]/sum(a9[,aa])
    }
  }
  
  a10 = table(train2[,10],train2[,34])
  a10dim = as.matrix(dim(a10))
  for (a in 1:a10dim[1]) {
    for (aa in 1:a10dim[2]){
      a10[a,aa] = ifelse(a10[a,aa]==0, 1, a10[a,aa])
    }
  }
  a10prob = a10
  for (a in 1:a10dim[1]) {
    for (aa in 1:a10dim[2]){
      a10prob[a,aa] = a10[a,aa]/sum(a10[,aa])
    }
  }
  
  a11 = table(train2[,11],train2[,34])
  a11dim = as.matrix(dim(a11))
  for (a in 1:a11dim[1]) {
    for (aa in 1:a11dim[2]){
      a11[a,aa] = ifelse(a11[a,aa]==0, 1, a11[a,aa])
    }
  }
  a11prob = a11
  for (a in 1:a11dim[1]) {
    for (aa in 1:a11dim[2]){
      a11prob[a,aa] = a11[a,aa]/sum(a11[,aa])
    }
  }  
  
  a12 = table(train2[,12],train2[,34])
  a12dim = as.matrix(dim(a12))
  for (a in 1:a12dim[1]) {
    for (aa in 1:a12dim[2]){
      a12[a,aa] = ifelse(a12[a,aa]==0, 1, a12[a,aa])
    }
  }
  a12prob = a12
  for (a in 1:a12dim[1]) {
    for (aa in 1:a12dim[2]){
      a12prob[a,aa] = a12[a,aa]/sum(a12[,aa])
    }
  }
  
  a13 = table(train2[,13],train2[,34])
  a13dim = as.matrix(dim(a13))
  for (a in 1:a13dim[1]) {
    for (aa in 1:a13dim[2]){
      a13[a,aa] = ifelse(a13[a,aa]==0, 1, a13[a,aa])
    }
  }
  a13prob = a13
  for (a in 1:a13dim[1]) {
    for (aa in 1:a13dim[2]){
      a13prob[a,aa] = a13[a,aa]/sum(a13[,aa])
    }
  }
  
  a14 = table(train2[,14],train2[,34])
  a14dim = as.matrix(dim(a14))
  for (a in 1:a14dim[1]) {
    for (aa in 1:a14dim[2]){
      a14[a,aa] = ifelse(a14[a,aa]==0, 1, a14[a,aa])
    }
  }
  a14prob = a14
  for (a in 1:a14dim[1]) {
    for (aa in 1:a14dim[2]){
      a14prob[a,aa] = a14[a,aa]/sum(a14[,aa])
    }
  }
  
  a15 = table(train2[,15],train2[,34])
  a15dim = as.matrix(dim(a15))
  for (a in 1:a15dim[1]) {
    for (aa in 1:a15dim[2]){
      a15[a,aa] = ifelse(a15[a,aa]==0, 1, a15[a,aa])
    }
  }
  a15prob = a15
  for (a in 1:a15dim[1]) {
    for (aa in 1:a15dim[2]){
      a15prob[a,aa] = a15[a,aa]/sum(a15[,aa])
    }
  }
  
  a16 = table(train2[,16],train2[,34])
  a16dim = as.matrix(dim(a16))
  for (a in 1:a16dim[1]) {
    for (aa in 1:a16dim[2]){
      a16[a,aa] = ifelse(a16[a,aa]==0, 1, a16[a,aa])
    }
  }
  a16prob = a16
  for (a in 1:a16dim[1]) {
    for (aa in 1:a16dim[2]){
      a16prob[a,aa] = a16[a,aa]/sum(a16[,aa])
    }
  }
  
  a17 = table(train2[,17],train2[,34])
  a17dim = as.matrix(dim(a17))
  for (a in 1:a17dim[1]) {
    for (aa in 1:a17dim[2]){
      a17[a,aa] = ifelse(a17[a,aa]==0, 1, a17[a,aa])
    }
  }
  a17prob = a17
  for (a in 1:a17dim[1]) {
    for (aa in 1:a17dim[2]){
      a17prob[a,aa] = a17[a,aa]/sum(a17[,aa])
    }
  }
  
  a18 = table(train2[,18],train2[,34])
  a18dim = as.matrix(dim(a18))
  for (a in 1:a18dim[1]) {
    for (aa in 1:a18dim[2]){
      a18[a,aa] = ifelse(a18[a,aa]==0, 1, a18[a,aa])
    }
  }
  a18prob = a18
  for (a in 1:a18dim[1]) {
    for (aa in 1:a18dim[2]){
      a18prob[a,aa] = a18[a,aa]/sum(a18[,aa])
    }
  }
  
  a19 = table(train2[,19],train2[,34])
  a19dim = as.matrix(dim(a19))
  for (a in 1:a19dim[1]) {
    for (aa in 1:a19dim[2]){
      a19[a,aa] = ifelse(a19[a,aa]==0, 1, a19[a,aa])
    }
  }
  a19prob = a19
  for (a in 1:a19dim[1]) {
    for (aa in 1:a19dim[2]){
      a19prob[a,aa] = a19[a,aa]/sum(a19[,aa])
    }
  }
  
  a20 = table(train2[,20],train2[,34])
  a20dim = as.matrix(dim(a20))
  for (a in 1:a20dim[1]) {
    for (aa in 1:a20dim[2]){
      a20[a,aa] = ifelse(a20[a,aa]==0, 1, a20[a,aa])
    }
  }
  a20prob = a20
  for (a in 1:a20dim[1]) {
    for (aa in 1:a20dim[2]){
      a20prob[a,aa] = a20[a,aa]/sum(a20[,aa])
    }
  }
  
  a21 = table(train2[,21],train2[,34])
  a21dim = as.matrix(dim(a21))
  for (a in 1:a21dim[1]) {
    for (aa in 1:a21dim[2]){
      a21[a,aa] = ifelse(a21[a,aa]==0, 1, a21[a,aa])
    }
  }
  a21prob = a21
  for (a in 1:a21dim[1]) {
    for (aa in 1:a21dim[2]){
      a21prob[a,aa] = a21[a,aa]/sum(a21[,aa])
    }
  }
  
  a22 = table(train2[,22],train2[,34])
  a22dim = as.matrix(dim(a22))
  for (a in 1:a22dim[1]) {
    for (aa in 1:a22dim[2]){
      a22[a,aa] = ifelse(a22[a,aa]==0, 1, a22[a,aa])
    }
  }
  a22prob = a22
  for (a in 1:a22dim[1]) {
    for (aa in 1:a22dim[2]){
      a22prob[a,aa] = a22[a,aa]/sum(a22[,aa])
    }
  }
  
  a23 = table(train2[,23],train2[,34])
  a23dim = as.matrix(dim(a23))
  for (a in 1:a23dim[1]) {
    for (aa in 1:a23dim[2]){
      a23[a,aa] = ifelse(a23[a,aa]==0, 1, a23[a,aa])
    }
  }
  a23prob = a23
  for (a in 1:a23dim[1]) {
    for (aa in 1:a23dim[2]){
      a23prob[a,aa] = a23[a,aa]/sum(a23[,aa])
    }
  }
  
  a24 = table(train2[,24],train2[,34])
  a24dim = as.matrix(dim(a24))
  for (a in 1:a24dim[1]) {
    for (aa in 1:a24dim[2]){
      a24[a,aa] = ifelse(a24[a,aa]==0, 1, a24[a,aa])
    }
  }
  a24prob = a24
  for (a in 1:a24dim[1]) {
    for (aa in 1:a24dim[2]){
      a24prob[a,aa] = a24[a,aa]/sum(a24[,aa])
    }
  }
  
  a25 = table(train2[,25],train2[,34])
  a25dim = as.matrix(dim(a25))
  for (a in 1:a25dim[1]) {
    for (aa in 1:a25dim[2]){
      a25[a,aa] = ifelse(a25[a,aa]==0, 1, a25[a,aa])
    }
  }
  a25prob = a25
  for (a in 1:a25dim[1]) {
    for (aa in 1:a25dim[2]){
      a25prob[a,aa] = a25[a,aa]/sum(a25[,aa])
    }
  }
  
  a26 = table(train2[,26],train2[,34])
  a26dim = as.matrix(dim(a26))
  for (a in 1:a26dim[1]) {
    for (aa in 1:a26dim[2]){
      a26[a,aa] = ifelse(a26[a,aa]==0, 1, a26[a,aa])
    }
  }
  a26prob = a26
  for (a in 1:a26dim[1]) {
    for (aa in 1:a26dim[2]){
      a26prob[a,aa] = a26[a,aa]/sum(a26[,aa])
    }
  }
  
  a27 = table(train2[,27],train2[,34])
  a27dim = as.matrix(dim(a27))
  for (a in 1:a27dim[1]) {
    for (aa in 1:a27dim[2]){
      a27[a,aa] = ifelse(a27[a,aa]==0, 1, a27[a,aa])
    }
  }
  a27prob = a27
  for (a in 1:a27dim[1]) {
    for (aa in 1:a27dim[2]){
      a27prob[a,aa] = a27[a,aa]/sum(a27[,aa])
    }
  }
  
  a28 = table(train2[,28],train2[,34])
  a28dim = as.matrix(dim(a28))
  for (a in 1:a28dim[1]) {
    for (aa in 1:a28dim[2]){
      a28[a,aa] = ifelse(a28[a,aa]==0, 1, a28[a,aa])
    }
  }
  a28prob = a28
  for (a in 1:a28dim[1]) {
    for (aa in 1:a28dim[2]){
      a28prob[a,aa] = a28[a,aa]/sum(a28[,aa])
    }
  }
  
  a29 = table(train2[,29],train2[,34])
  a29dim = as.matrix(dim(a29))
  for (a in 1:a29dim[1]) {
    for (aa in 1:a29dim[2]){
      a29[a,aa] = ifelse(a29[a,aa]==0, 1, a29[a,aa])
    }
  }
  a29prob = a29
  for (a in 1:a29dim[1]) {
    for (aa in 1:a29dim[2]){
      a29prob[a,aa] = a29[a,aa]/sum(a29[,aa])
    }
  }
  
  a30 = table(train2[,30],train2[,34])
  a30dim = as.matrix(dim(a30))
  for (a in 1:a30dim[1]) {
    for (aa in 1:a30dim[2]){
      a30[a,aa] = ifelse(a30[a,aa]==0, 1, a30[a,aa])
    }
  }
  a30prob = a30
  for (a in 1:a30dim[1]) {
    for (aa in 1:a30dim[2]){
      a30prob[a,aa] = a30[a,aa]/sum(a30[,aa])
    }
  }
  
  a31 = table(train2[,31],train2[,34])
  a31dim = as.matrix(dim(a31))
  for (a in 1:a31dim[1]) {
    for (aa in 1:a31dim[2]){
      a31[a,aa] = ifelse(a31[a,aa]==0, 1, a31[a,aa])
    }
  }
  a31prob = a31
  for (a in 1:a31dim[1]) {
    for (aa in 1:a31dim[2]){
      a31prob[a,aa] = a31[a,aa]/sum(a31[,aa])
    }
  }
  
  a32 = table(train2[,32],train2[,34])
  a32dim = as.matrix(dim(a32))
  for (a in 1:a32dim[1]) {
    for (aa in 1:a32dim[2]){
      a32[a,aa] = ifelse(a32[a,aa]==0, 1, a32[a,aa])
    }
  }
  a32prob = a32
  for (a in 1:a32dim[1]) {
    for (aa in 1:a32dim[2]){
      a32prob[a,aa] = a32[a,aa]/sum(a32[,aa])
    }
  }
  
  a33 = table(train2[,33],train2[,34])
  a33dim = as.matrix(dim(a33))
  for (a in 1:a33dim[1]) {
    for (aa in 1:a33dim[2]){
      a33[a,aa] = ifelse(a33[a,aa]==0, 1, a33[a,aa])
    }
  }
  a33prob = a33
  for (a in 1:a33dim[1]) {
    for (aa in 1:a33dim[2]){
      a33prob[a,aa] = a33[a,aa]/sum(a33[,aa])
    }
  }
  testdim = as.matrix(dim(test2))
  predictiontable = matrix(,nrow=testdim[1])
  temperror = matrix(,nrow=testdim[1])
  for (repp in 1:testdim[1]){
    temp1 = a1prob[test2[repp,1],]
    temp2 = a2prob[test2[repp,2],]
    temp3 = a3prob[test2[repp,3],]
    temp4 = a4prob[test2[repp,4],]
    temp5 = a5prob[test2[repp,5],]
    temp6 = a6prob[test2[repp,6],]
    temp7 = a7prob[test2[repp,7],]
    temp8 = a8prob[test2[repp,8],]
    temp9 = a9prob[test2[repp,9],]
    temp10 = a10prob[test2[repp,10],]
    temp11 = a11prob[test2[repp,11],]
    temp12 = a12prob[test2[repp,12],]
    temp13 = a13prob[test2[repp,13],]
    temp14 = a14prob[test2[repp,14],]
    temp15 = a15prob[test2[repp,15],]
    temp16 = a16prob[test2[repp,16],]
    temp17 = a17prob[test2[repp,17],]
    temp18 = a18prob[test2[repp,18],]
    temp19 = a19prob[test2[repp,19],]
    temp20 = a20prob[test2[repp,20],]
    temp21 = a21prob[test2[repp,21],]
    temp22 = a22prob[test2[repp,22],]
    temp23 = a23prob[test2[repp,23],]
    temp24 = a24prob[test2[repp,24],]
    temp25 = a25prob[test2[repp,25],]
    temp26 = a26prob[test2[repp,26],]
    temp27 = a27prob[test2[repp,27],]
    temp28 = a28prob[test2[repp,28],]
    temp29 = a29prob[test2[repp,29],]
    temp30 = a30prob[test2[repp,30],]
    temp31 = a31prob[test2[repp,31],]
    temp32 = a32prob[test2[repp,32],]
    temp33 = a33prob[test2[repp,33],]
    probs = rbind(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12,temp13,temp14,temp15,
                  temp16,temp17,temp18,temp19,temp20,temp21,temp22,temp23,temp24,temp25,temp26,temp27,temp28,temp29,
                  temp30,temp31,temp32,temp33)
    
    probclass = matrix(,ncol=2)
    for (cll in 1:2){
      probclass[,cll] = probs[1,cll] * probs[2,cll] * probs[3,cll] * probs[4,cll] * probs[5,cll] * probs[6,cll] *
        probs[7,cll] * probs[8,cll] * probs[9,cll] * probs[10,cll] * probs[11,cll] * probs[12,cll] *
        probs[13,cll] * probs[14,cll] * probs[15,cll] * probs[16,cll] * probs[17,cll] * probs[18,cll] *  
        probs[19,cll] * probs[20,cll] * probs[21,cll] * probs[22,cll] * probs[23,cll] * probs[24,cll] * 
        probs[25,cll] * probs[26,cll] * probs[27,cll] * probs[28,cll] * probs[29,cll] * probs[30,cll] * 
        probs[31,cll] * probs[32,cll] * probs[33,cll]
    }
    colnames(probclass) = c("-", "+")
    prediction = which.max(probclass)
    prediction = if (prediction == 1) {"b"} else {"g"}
    predictiontable[repp] = prediction
    temperror[repp] = predictiontable[repp] == test[repp,34]
  }
  
  error = 1-(sum(temperror)/length(temperror))
  
  return(list(predictiontable=predictiontable,error=error))
}

iono1 = matt_NB_iono(ionodata$train1,ionodata$test1)
iono2 = matt_NB_iono(ionodata$train2,ionodata$test2)
iono3 = matt_NB_iono(ionodata$train3,ionodata$test3)
iono4 = matt_NB_iono(ionodata$train4,ionodata$test4)
iono5 = matt_NB_iono(ionodata$train5,ionodata$test5)

iono_err = as.matrix(cbind(iono1$error, iono2$error, iono3$error, iono4$error, iono5$error))
iono_error = barplot(iono_err, main="Misclassification Rate \n Ionosphere Data Set", xlab="Testing Set Number", 
                       ylab="Error", names.arg=c("1","2","3","4","5"), border="red", 
                       density=c(90, 70, 50, 40, 30, 20, 10), ylim = c(0,.35))

iono_avg_error = (iono1$error + iono2$error + iono3$error + iono4$error + iono5$error)/5
# Average error = 0.270664


total_avg = cbind(car_avg_error,credit_avg_error,iono_avg_error)

error_avg_plot = barplot(total_avg, main="Misclassification Rate", xlab="Data Set", 
                     ylab="Error", names.arg=c("Car","Credit","Ionoshphere"), border="red", 
                     density=c(90, 70, 50, 40, 30, 20, 10), ylim = c(0,.30))

a4 = as.matrix(cbind(0.1990919,0.1886621))
a4plot = barplot(a4, main="KNN VS Naive Bayes \nCar Data Set", xlab="Algorithm", 
                 ylab="Error", names.arg=c("Naive Bayes","KNN, Euclidian k=5"), border="red", 
                 density=c(90, 70, 50, 40, 30, 20, 10), ylim = c(0,.20))
b4 = as.matrix(cbind(0.1654257,0.3017029))
b4plot = barplot(b4, main="KNN VS Naive Bayes \nCredit Data Set", xlab="Algorithm", 
                 ylab="Error", names.arg=c("Naive Bayes","KNN, Manhattan k=3"), border="red", 
                 density=c(90, 70, 50, 40, 30, 20, 10), ylim = c(0,.30))
c4 = as.matrix(cbind(0.270664,0.09963783))
c4plot = barplot(c4, main="KNN VS Naive Bayes \nCredit Ionosphere Data Set", xlab="Algorithm", 
                 ylab="Error", names.arg=c("Naive Bayes","KNN, Manhattan k=1"), border="red", 
                 density=c(90, 70, 50, 40, 30, 20, 10), ylim = c(0,.30))

