library(readr)
library(mvtnorm)
library(mnormt)
library(plyr)

# em.matt('Dataset', 'Cluster Size')
# example = em.matt('Dataset','Cluster Size')

# to return elements of funciton em.matt call,
# example$Wij or round(example$Wij) for the weighted matrix
# example$mean_matrix for the final mean matrix
# example$cov_matrix for the final covaraince matrix
# example$t for the number of iterations for convergance

# Manually change tau and maximum iterations within the function
em.matt <-function(X, K, tau=1e-4, iter.max=50){

# Finds dimensionality
X = as.matrix(X)  
n = nrow(X)
X_cols = ncol(X)

#start iteration counter
t=0

#prior mean matrix - new matrix
d <- function(x,y) sum( (x-y)^2) 

#initalization
cov_matrix = array(diag(X_cols),c(X_cols,X_cols,K)) #identity matrix, covariance

#set.seed(527)
mean_matrix = matrix(sample(X,K*X_cols,replace = FALSE,prob = NULL), nrow = K, ncol = X_cols) #random sample means
priors = matrix(1/K,nrow = 1, ncol = K) #inital priors, all equal
Wij = matrix(0,ncol=K,nrow = n) #inital probability matrix 

#main loop
while (t<iter.max)  {
  mean_matrix_old = mean_matrix #store previous mean matrix
  
#e-step
  #with the for loop, finds the prior of cluster * the pdf of a multivate Guassian
  #outside the for loop, divides the updated Wij by the the row sums of Wij
  for (Cluster in 1:K){
    Wij[,Cluster] = priors[Cluster]* dmvnorm(X,mean_matrix[Cluster,],cov_matrix[,,Cluster])
  }
  Wij = Wij/rowSums(Wij)
  
#m-step
  
  #update priors  
  Wj = matrix(colSums(Wij),nrow = 1, ncol = K) #cluster membership size = sum of cluster(i)
  priors = Wj/n #cluster membership size/number of data points
  
  #update maen
  #t(Wij[C,]) used instead of Wj[C] for dimensonality
  #the weighted number of points assigned to cluster C multiplied by observed data divided by C cluster membership size
  #create an empty matrix
  mean_matrix = matrix(,ncol = X_cols) 
  for (C in 1:K){
    mean_matrix = na.omit(rbind(mean_matrix,crossprod(t(Wij)[C,],X)/Wj[C])) 
  }
  
  #update covairance matrix 
  #use diag(Wij)[,clust] to keep dimensonality and find each data points cluster probability
  for(clust in 1:K){
    cov_matrix[,,clust] = (t(X-mean_matrix[clust,]) %*% diag(Wij[,clust]) %*% (X-mean_matrix[clust,]))/Wj[clust]
  }  
  cov_matrix = array(diag(.01,X_cols,X_cols),c(X_cols,X_cols,K)) + cov_matrix #so Guassian k does not collaspe
  
  #stops when previous mean and current mean is under the threshold
  if(d(mean_matrix_old,mean_matrix) <= tau*K)  {
    
    #Returns number of  iterations to converge and a snapchat of Wij matrix
    return(list(t=t,mean_matrix=mean_matrix,cov_matrix=cov_matrix,Wij=Wij))
    break
  }
  #adds 1 to the iteration counter
  t = t + 1
}
}



