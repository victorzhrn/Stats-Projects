library(ggplot2)
n = 12800            # number of donations
ratio = 0.011        # estimate infection rate
iterations = 100     # number of trial for each possible k

# this function calculates integer factors of a number
factorize <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  factors <- div[x %% div == 0L]
  return(factors)
}

# this function calculates
calculate_times <- function(list,k){
  sum = 0
  for (i in 1:length(list)){
    if(sum(unlist(list[i]))==0){     # if the blood sample passes test
      sum = sum+1
    }else{                           # if the blood sample is infected
      sum = sum+k
    }
  }
  return(sum)
}

ks <- factorize(n)   # generate possible factors of n

min= n
output_k = c()
output_trial = c()
for(k in ks){
  print(k)
  pieces = n/k     # calculate number of groups based on k
  group = rep(1:pieces,each=k) # factors for sample split
  count = c()      # vector to contain all intermediate result
  for (l in 1:iterations){
    sample <- rbinom(n,1,ratio)   # generate random samples 
    sample_splits <- split(sample,group) # split sample
    count = c(count, calculate_times(sample_splits,k)) # calculate number of trails 
  }
  if(median(count)<min){   # median
    min = median(count)
    min_k = k
  }
  output_k <- c(output_k,k)
  output_trial <- c(output_trial,median(count))
}

# plot number of trials for different possible value of k
# notice x-axis as k -(number of people in each group) is factorized to make plot proportional
g <- ggplot()+geom_line(aes(x=as.factor(output_k),y=output_trial,group=1))
g <- g+xlab("number of people in each testing group")+ylab("number of trails needed")
g





