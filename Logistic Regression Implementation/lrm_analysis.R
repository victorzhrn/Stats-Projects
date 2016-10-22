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
source("LogisticRegressionFunc.R")

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


