################################################################################
#Program: PortfolioFunctions Class: MA386 - Statistical Programming 
#Description: a logistic regression was implemented to predict the possible failure 
# of an upcoming space shuttle luanch
#Author: Ruinan(Victor) Zhang Date: 09/19/2016 Modified: 09/19/2016
#
#Notes: This task requires a implementation of logistic regression and test it on a sample 
# space shuttle launching data 

################################################################################

# load libraries and packages
library(Matrix)
require(MASS)
library(plot3D)

# logistic regression
# function lrm takes a design matrix and response vector
lrm <- function(X_origin,y){
  X <- cbind(rep(1,nrow(X_origin)),X_origin)
  n = ncol(X)
  k = 1    # count for number of iterations
  
  p <- list()   # all intermediate result was stored in a list
  beta = list()
  beta[[k]] <- rep(0,n) # betas are started with zeros
  p[[k]] <- exp(X %*% beta[[k]])/(1+exp(X %*% beta[[k]])) # initialize
  
  # this function calculates beta  though maximum likelihood 
  update_beta <- function(k,beta,p){
    
    if (k >250){  # if the iteration is beyond 250, it's probablity not going to converge
      # raise error if not converge
      stop("algorithm does not converge after 250 iteration")
    }
    D <- Diagonal(length(as.vector(p[[k]])),as.vector(p[[k]]))
    tem <- t(X) %*% D %*% X  # this is a temperary vector variable
    tem <- matrix(tem,n,n)
    delta <- ginv(tem) %*% t(X) %*% (y-p[[k]]) # update step
    
    criteria<- sum(delta^2)/sum(beta[[k]]^2)  # converging criteria
    if (criteria < 10^(-7)){
      return(beta[[k]])
    }
    beta[[k+1]] <- beta[[k]]+delta
    p[[k+1]] <- exp(X %*% beta[[k+1]])/(1+exp(X %*% beta[[k+1]]))
    k=k+1
    update_beta(k,beta,p)
  }
  beta_final <- update_beta(k,beta,p)
  
  return(beta_final)
}

number_failure <- c(0,1,rep(0,6),1,1,1,0,0,1,rep(0,6),1,0,1)
launch_temp <- c(66,70,69,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58)
leak_check_pressure <- c(rep(50,6),rep(100,2),rep(200,15))
X_train <- cbind(launch_temp, leak_check_pressure)  # set training design matrix

beta <- lrm(X_train,number_failure) # calculate beta
x<-c(1,31,200)
exp(sum(beta * x))/(1+exp(sum(beta*x))) # calcualte output response

temp = seq(65,84)
pres = seq(50,200,length.out = 20)
intercept <- rep(1,20)
x<- cbind(intercept,temp,pres)
z <- exp((x %*% beta))/(1+exp((x %*% beta)))

# visualization of regression model
lines3D(temp,pres,z,xlab="temperature",ylab="pressure", zlab="failure") 


