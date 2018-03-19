# Matthew Schwartzer

require("ISLR")
require("ggplot2")
require("ggthemes")
require("plotly")

# Pulls Auto data set from ISLR
Auto

# Data Set
X1=Auto[,4]
X1=as.matrix(cbind(1,X1))
Y1=as.matrix(Auto[,1],ncol=1)

# Problems 1.1 & 1.2
# grad.descent1(X1,Y1,.0001,400000)
# x0 =  39.5971919
# x1 = -0.1549895
# Plot with regression line is returned
grad.descent1 = function(X, Y, alpha, itnum){
  X=as.matrix(X)
  Y=as.matrix(Y)
  m=nrow(X)
  f=ncol(X)
  theta=matrix(0,nrow=f)
  theta.his=matrix(0,nrow=f,ncol=itnum)
  cost=matrix(,nrow=itnum)
  for (i in 1:itnum){
theta = theta - alpha*(1/m)*(t(X) %*% (X %*% theta - Y))
  theta.his[,i]=theta
  cost[i]=(1/2*m)*sum(((X %*% theta - Y)^2))
  }
  return(list(theta=theta,cost=cost,theta.his=theta.his))
}

# Save algorithm into a object
result = grad.descent1(X1,Y1,.0001,400000)

# Scatter plot and line of best fit from the above parameters
lmplot=ggplot(Auto,aes(x=horsepower,y=mpg))+geom_point()+
  geom_abline(intercept = result$theta[1],slope = result$theta[2],colour='#E41A1C')+theme_economist_white()+
  labs(title="Horsepower's Effect on MPG")+xlab("Horsepower")+ylab("MPG")

# Cost as iterations increase towards 400,000
costplot=ggplot(as.data.frame(result$cost),aes(x=seq(from=1,to=400000),y=as.data.frame(result$cost))) + geom_line() +
  theme_economist_white()+labs(title="Cost as Iterations Increase")+xlab("Iterations")+ylab("Cost J(0)")

# Problem 1.3
# calc.grad.descent1(220)
# When horsepower = 220, MPG = 5.499502
calc.grad.descent1 = function (X){
  answer = 39.5971919 + (X*-0.1549895)
  return(answer)
}

# Problem 1.4
# Makes sure the data is in the correct form to plot a contour plot
contour.data=as.matrix(cbind(as.matrix(result$theta.his[1,]),as.matrix(result$theta.his[2,]),as.matrix(result$cost,ncol=1)))
colnames(contour.data)=(c("X","Y","Z"))
contour.data=as.data.frame(contour.data)
contour.data=signif(contour.data,4)
contour.plot=ggplot(contour.data, aes(X,Y))+geom_density_2d()+xlim(34,41)+ylim(-.16,-.1) + labs(title="Decreasing Cost Towards Optimal Theta 1 and Theta 0",
        x="Theta0",y="Theta1")+theme_economist_white()


# Problem 1.5
# normallm(X1,Y1) 
# x0 = 39.9358610 
# x1 = -0.1578447
normallm = function(X,Y){
simpletheta=solve(crossprod(X), crossprod(X,Y))
return(simpletheta)
}


