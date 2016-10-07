library(MASS)
library(ISLR)
library(graphics)

names(Boston)

lm.fit = lm(data = Boston, medv~lstat)
abline(lm.fit,lwd=3,col = "red")

par(mfrow = c(2,2))
plot(lm.fit)

par(mfrow = c(1,1))
plot(predict(lm.fit),residuals(lm.fit)) # plot residuals againts predicted
plot(predict(lm.fit),rstudent(lm.fit))  # plot studentized residuals

hat_values <- hatvalues(lm.fit) # hatvalue() compute the Leverage Statistics
plot(hat_values) #visualize the Leverage Statistics
which.max(hat_values) # identify the index has the highest Leverage

