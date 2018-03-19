#Matthew Schwartzer
#Applied Machine Learning Homework 3, Questions 1.6-1.9
#Oct 27, 2017

require("ISLR")
require("ggplot2")
require("ggthemes")

# Pulls Auto data set from ISLR
Auto

# Packaged linear regression to compare answers
lm(mpg~cylinders + displacement + horsepower + weight + acceleration + year + origin,data.auto)

# Problem 1.6
# Mean normalization
data.auto=as.data.frame(Auto[,-9])
data.norm=as.matrix(data.auto[,-1])
data.Y=as.matrix(Auto[,1])
for(j in 1:8){
data.auto[,j]=(data.auto[,j]-mean(data.auto[,j]))/sd(data.auto[,j])
}
data.auto=as.matrix(cbind(1,data.auto))
X2=as.matrix(data.auto[,-2])
Y2=as.matrix(data.auto[,2],ncol=1)

# grad.descent2(X2,Y2,.0001,1000000)
grad.descent2 = function(X, Y, alpha, itnum){
  X=as.matrix(X)
  Y=as.matrix(Y)
  m=nrow(X)
  f=ncol(X)
  cost1=matrix(,nrow=itnum)
  theta=matrix(0,nrow=f)
  for (i in 1:itnum){
    theta = theta - alpha*(1/m)*(t(X) %*% (X %*% theta - Y))
    cost1[i]=(1/2*m)*sum(((X %*% theta - Y)^2))
  }
  return(list(theta=theta,cost1=cost1))
}
# Estimated parameter estimates
# intercept     5.759240e-16
# cylinders    -1.011388e-01
# displacement  2.537031e-01
# horsepower   -8.179415e-02
# weight       -7.007069e-01
# acceleration  2.785648e-02
# year          3.541238e-01
# origin        1.461881e-01
# As year increases, gas mileage gets better. This positive relationship between year and gas efficiency could be
# attributed to a number of factors including the demand for my environmentally friendly cars, the rise of gas 
# prices causing a demand for more efficient cars, and government regulations demanding more efficient cars.
# This relationship is reflected in theory and real-world observations.


# Problem 1.7
# calc.grad.descent2(4,300,200,3500,11,70,2,data.Y)
# With the above inputs, MPG = 16.93241
# Y is the unormalized column vector of MPG
calc.grad.descent2 = function(x1,x2,x3,x4,x5,x6,x7,Y){
  coefvalues=matrix(c(x1,x2,x3,x4,x5,x6,x7),nrow = 7)
  for(norm in 1:7){
   coefvalues[norm]=(coefvalues[norm]-mean(data.norm[,norm]))/sd(data.norm[,norm])
}
  midanswer=(5.759240e-16 + -1.011388e-01*(coefvalues[1]) + 2.537031e-01*(coefvalues[2]) +
  -8.179415e-02*(coefvalues[3]) + -7.007069e-01*(coefvalues[4])+ 2.785648e-02*(coefvalues[5]) + 
  3.541238e-01*(coefvalues[6]) + 1.461881e-01*(coefvalues[7]))
  mpganswer=midanswer*sd(Y)+mean(Y)
  return(mpganswer)
}

# Problem 1.8
alpha3=grad.descent2(X2,Y2,3,100)
cost3=ggplot(as.data.frame(alpha3$cost1),aes(x=seq(from=1,to=100),y=as.data.frame(alpha3$cost1))) + geom_line()
# ^^ above function does not converge becasue the learning rate is too, big and the cost increases to infinite, thus;
# the data is not representable in a graph
cost3g=ggplot(as.data.frame(alpha3$cost1[1:25]),aes(x=seq(from=1,to=25),y=as.data.frame(alpha3$cost1[1:25]))) + geom_line() +
  labs(x="Iterations", y="Cost J(0)",title="Cost = 3",subtitle="Alpha is too large and increases towards infinite") + theme_economist_white()
# ^^ Above graph shows J(0) increasing to infinite

# The below aplha's all produce monotonically decreasing scatter plots
alpha.3=grad.descent2(X2,Y2,.3,100)
cost.3=ggplot(as.data.frame(alpha.3$cost1),aes(x=seq(from=1,to=100),y=as.data.frame(alpha.3$cost1))) + geom_line() +
  xlab("Iterations") + ylab("Cost J(0)") +ggtitle("Cost = .3") + theme_economist_white()

alpha.03=grad.descent2(X2,Y2,.03,100)
cost.03=ggplot(as.data.frame(alpha.03$cost1),aes(x=seq(from=1,to=100),y=as.data.frame(alpha.03$cost1))) + geom_line() +
  xlab("Iterations") + ylab("Cost J(0)") +ggtitle("Cost = .03") + theme_economist_white()

alpha.00003=grad.descent2(X2,Y2,.00003,100)
cost.00003=ggplot(as.data.frame(alpha.00003$cost1),aes(x=seq(from=1,to=100),y=as.data.frame(alpha.00003$cost1))) + geom_line() +
  xlab("Iterations") + ylab("Cost J(0)") +ggtitle("Cost = .00003") + theme_economist_white()
# The graph with an alpha of 0.3 reduces J(0) the fastest and is monitonically decreasing as it should, therefore;
# I would choose this alpha to optimize the gradient descent algorithm.

# Problem 1.9
# normallm(X2,Y2)
normallm = function(X,Y){
  simpletheta=solve(crossprod(X), crossprod(X,Y))
  return(simpletheta)
}
# Normal equations parameter estimates
# intercept    -2.161397e-16
# cylinders    -1.078273e-01
# displacement  2.667467e-01
# horsepower   -8.359623e-02
# weight       -7.045565e-01
# acceleration  2.848143e-02
# year          3.543429e-01
# origin        1.471853e-01