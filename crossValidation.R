# Matthew Schwartzer

kCrossValidation = function(data) {
  colnum = ncol(data) + 2
  k = 5
  folds = data.frame(kFold = sample(rep(x=1:k, length.out=nrow(data))), Row=1:nrow(data))
  tempdata = cbind(folds,data)
  temp1 = tempdata[which(tempdata$kFold=='1' | tempdata$kFold=='2' | tempdata$kFold=='3' | tempdata$kFold=='4'), ]
  train1 = temp1[,3:colnum]
  temp2 = tempdata[which(tempdata$kFold=='1' | tempdata$kFold=='2' | tempdata$kFold=='3' | tempdata$kFold=='5'), ]
  train2 = temp2[,3:colnum]
  temp3 = tempdata[which(tempdata$kFold=='1' | tempdata$kFold=='2' | tempdata$kFold=='5' | tempdata$kFold=='4'), ]
  train3 = temp3[,3:colnum]
  temp4 = tempdata[which(tempdata$kFold=='1' | tempdata$kFold=='5' | tempdata$kFold=='3' | tempdata$kFold=='4'), ]
  train4 = temp4[,3:colnum]
  temp5 = tempdata[which(tempdata$kFold=='5' | tempdata$kFold=='2' | tempdata$kFold=='3' | tempdata$kFold=='4'), ]
  train5 = temp5[,3:colnum]
  temp6 = tempdata[which(tempdata$kFold=='5'),]
  test1 = temp6[,3:colnum]
  temp7 = tempdata[which(tempdata$kFold=='4'),]
  test2 = temp7[,3:colnum]
  temp8 = tempdata[which(tempdata$kFold=='3'),]
  test3 = temp8[,3:colnum]
  temp9 = tempdata[which(tempdata$kFold=='2'),]
  test4 = temp9[,3:colnum]
  temp10 = tempdata[which(tempdata$kFold=='1'),]
  test5 = temp10[,3:colnum]
  return(list(test1=test1,test2=test2,test3=test3,test4=test4,test5=test5,train1=train1,train2=train2,train3=train3,
              train4=train4,train5=train5))
}



