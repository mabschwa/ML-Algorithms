#Matthew Schwartzer

# Packaged solution to compare manual implementation
glmsolution=glm(mpg01~cylinders+displacement+horsepower+weight,data=complete.Auto,family = "binomial")

#####################################################################################################
# Taken from newAuto.R
#####################################################################################################
require("ISLR")

# Pulls Auto data set from ISLR
Auto

# Finds median and classifies MPG as a binary (0,1) variable
mpg01=Auto[,1]
mpgmedian=median(mpg01)
mpg01[Auto[,1]>=mpgmedian]=1
mpg01[Auto[,1]<mpgmedian]=0
mpg01=as.matrix(mpg01)

# Pull cylinders, displacement, horsepower, and weight from Auto data set
new.Auto=as.data.frame(Auto[,2:5])

#Feature scaling, mean normilization
for(scale in 1:4){
  new.Auto[,scale]=(new.Auto[,scale]-mean(new.Auto[,scale]))/sd(new.Auto[,scale])
}

# Column binds scaled inputs with a column of 1's 
new.Auto=cbind(1,new.Auto)

# Comple data set with input and output variables
complete.Auto=cbind(new.Auto,mpg01)

# Used to calculate error rate
auto.error=(as.matrix(Auto[,2:5]))

#####################################################################################################
# End code from newAuto.R
#####################################################################################################

# Problem 3.1
sigmoid = function(x){
  (exp(1)^x)/((exp(1)^x)+1)
}
x=matrix(seq(from=-5,to=5,by=.01))
signums=sapply(x,sigmoid)

sigmoidplot=ggplot(as.data.frame(signums),aes(x=seq(from=-5,to=5,by=.01),y=as.data.frame(signums))) + geom_line() +
  xlab("z") + ylab("g(z)") +ggtitle("Sigmoid Function Between -5 and 5") + theme_economist_white()

# Problem 3.2
# grad.descent.logistic(new.Auto,mpg01,.01,500000)
grad.descent.logistic = function(X, Y, alpha, itnum){
  X=as.matrix(X)
  Y=as.matrix(Y)
  m=nrow(X)
  f=ncol(X)
  theta=matrix(0,nrow=f)
  sig.x=matrix(,nrow = itnum)
  costlog=matrix(,nrow = itnum)
  for (i in 1:itnum){
    theta = theta - alpha*(1/m)*(t(X) %*% (1/(1+exp(-X %*% theta)) - Y))
    sig.x = sigmoid(X %*% theta)
    costlog[i]=(1/m)*sum((-Y*log(sig.x)) - ((1-Y)*log(1-sig.x))) 
  }
  return(list(theta=theta,costlog=costlog))
}
# Estimated parameters 
# intercept    -0.99318158
# cylinders    -0.02205884
# displacement -1.35848094
# horsepower   -1.62171152
# weight       -1.65273583

# Problem 3.3
# Calculates the final thetas based on the below function
result.logistic=grad.descent.logistic(new.Auto,mpg01,.01,500000)
thetas=as.matrix(result.logistic$theta[,1],nrow=5)

# Below calcuates the hypothesis of mpg (0:1)
probabilitytable=(1/(1+exp(as.matrix(-1*new.Auto) %*% thetas)))

# Converts the probability into a 0 or depending on +- .5, probabiliies >= .5 are classified as 1's and 
# probabilities < .5 are classified as 0's.
forcasetedmpg=matrix(0,nrow=392)
forcasetedmpg[probabilitytable>=.5]=1
forcasetedmpg[probabilitytable<.5]=0

# Makes a matrix to compare estimated mpg to actual mpg classification
comparison=cbind(forcasetedmpg,mpg01)
error=matrix(,nrow=392)
for (er in 1:392){
  error[er]=comparison[er,1]==comparison[er,2]
}
errorrate=length(which(error == FALSE))/392
# Misclassification rate = 10.20408%

# Problem 3.4
# Normalize inputs
inputvals=matrix(c(8, 340, 200, 3500),nrow=4)
for(scal in 1:4){
  inputvals[scal]=(inputvals[scal]-mean(auto.error[,scal]))/sd(auto.error[,scal])
}
inputvals=rbind(1,inputvals)
inputvals=inputvals*-1
input=(1/(1+exp(t(as.matrix(inputvals)) %*% thetas)))
# 0.0003499799
# Given (8, 340, 200, 3500) these inputs, MPG should be classified as a 0.

# Problem 3.5
# The below aplha's all produce monotonically decreasing scatter plots
lg3=grad.descent.logistic(new.Auto,mpg01,3,100)
costlg3=ggplot(as.data.frame(lg3$costlog),aes(x=seq(from=1,to=100),y=as.data.frame(lg3$costlog))) + geom_line() +
  xlab("Iterations") + ylab("Cost J0") +ggtitle("Cost = 3") + theme_economist_white()

lg.3=grad.descent.logistic(new.Auto,mpg01,.3,100)
costlg.3=ggplot(as.data.frame(lg.3$costlog),aes(x=seq(from=1,to=100),y=as.data.frame(lg.3$costlog))) + geom_line() +
  xlab("Iterations") + ylab("Cost J0") +ggtitle("Cost = .3") + theme_economist_white()

lg.03=grad.descent.logistic(new.Auto,mpg01,.03,100)
costlg.03=ggplot(as.data.frame(lg.03$costlog),aes(x=seq(from=1,to=100),y=as.data.frame(lg.03$costlog))) + geom_line() +
  xlab("Iterations") + ylab("Cost J0") +ggtitle("Cost = .03") + theme_economist_white()

lg.00003=grad.descent.logistic(new.Auto,mpg01,.00003,100)
costlg.00003=ggplot(as.data.frame(lg.00003$costlog),aes(x=seq(from=1,to=100),y=as.data.frame(lg.00003$costlog))) + geom_line() +
  xlab("Iterations") + ylab("Cost J0") +ggtitle("Cost = .00003") + theme_economist_white()
# When alpha is .00003, the algorithm does not converge fast enought, but when the alpha is 3, the alogirthm
# converages fast making it the most optimzed option out of the 4 plots above. 
