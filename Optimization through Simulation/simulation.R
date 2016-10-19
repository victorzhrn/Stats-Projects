library(ggplot2)

n = 12800            # number of donations
ratio = 0.011        # estimate infection rate
iterations = 200     # number of trial for each possible k

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

ks <- factorize(n)   # generate possible factors of n as possible k

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
  if(mean(count)<min){   # median
    min = mean(count)
    min_k = k
  }
  output_k <- c(output_k,k)
  output_trial <- c(output_trial,median(count))
}


# plot number of trials for different possible value of k
# notice x-axis as k -(number of people in each group) is factorized to make plot proportional
g <- ggplot()+geom_line(aes(x=as.factor(output_k),y=output_trial,group=1))
g <- g+xlab("number of people in each testing group")+ylab("number of trails needed")g <- g+xlab("number of people in each testing group")+ylab("number of trails needed")+ggtitle("number of tests vs. k")
g


optimal_k = c()
df = data.frame()
# second approach calculate what is the best possible k for different samples
for (i in 1:iterations){
  print(i)
  sample <- rbinom(n,1,ratio)   # generate random samples 
  count = c()      # vector to contain all intermediate result
  for(k in ks){
    pieces = n/k     # calculate number of groups based on k
    group = rep(1:pieces,each=k) # factors for sample split
    sample_splits <- split(sample,group) # split sample
    count = c(count, calculate_times(sample_splits,k)) # calculate number of trials needed for each k
  }
  local_best_n = min(count) # get the best k for each possible sample
  optimal_k <- c(optimal_k,ks[count==local_best_n])  # record the best k into vector
  df <- rbind(df,count)
}
names(df) <- ks

# line plot of # of tests vs. k
gg<- ggplot()+geom_line(aes(x=as.factor(ks),y=apply(df,2,mean)),group=1)
gg <- gg+xlab("number of people in each testing group")+ylab("number of trails needed")+ggtitle("number of tests vs. k")
gg

df_k <- as.data.frame(table(optimal_k))
names(df_k) <- c("k","occurance")

# pie plot on optimal k occurance
ggg <- ggplot(data = df_k,aes(x='',y=occurance,fill=k)) + geom_bar(width = 1,stat = 'identity')
ggg <- ggg + coord_polar("y", start=0)+xlab("")+ylab("")+ggtitle("occurance of optimal k")
ggg


