library(ggplot2)

voted_favor = 3208
voted_oppose = 3143


test_favor = 730
test_oppose = 942
test_total = test_favor+test_oppose
test_favor_ratio = test_favor/test_total


n=10000   # set number of permutations
data = c(rep(1,voted_favor),rep(0,voted_oppose))

ratio_distrib = c()
for (i in 1:n){
  perm = sample(data,test_total)    # sample from counted votes
  perm_favor = sum(perm)            
  perm_favor_ratio = perm_favor/test_total   # calculate simulated favor rate
  ratio_distrib <- c(ratio_distrib,perm_favor_ratio)
}

ratio_min = min(ratio_distrib)
ratio_max = max(ratio_distrib)
step = (ratio_max-ratio_min)/10

range_distrib = cut(ratio_distrib,breaks=seq(ratio_min-step,ratio_max+step,step))
df <- as.data.frame(table(range_distrib))
names(df) <- c("range","count")

# construct distribution plot
ggplot(data=df,aes(x=range,y=count))+geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle=45))

sum(ratio_distrib<test_favor_ratio)
