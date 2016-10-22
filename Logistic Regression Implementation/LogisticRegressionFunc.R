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