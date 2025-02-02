#Bootstrap Example
temp = c(202.2,203.4,200.5,202.5,206.3,198.0,203.7,200.8,201.3,199.0)
library(boot)
median.fun = function(dat, idx) median(dat[idx],na.rm = TRUE)
(boot.out.median = boot(data = temp, statistic = median.fun, R = 10000))

x = matrix(nrow = 10,ncol = 10)
B = 10
m = matrix(nrow = 10)
for (i in 1:10){
  x[i,] = sample(temp,length(temp),replace = T)
  m[i,] = median(x[i,])
}
x #the 10 resamples
m # the 10 medians
mean(m) #the mean of the medians

(boot2 = boot(data = temp,statistic = median.fun, R = 10000))
(ci = boot.ci(boot2))

#Normal
201.75+0.018 + c(-1,1)*qnorm(1.95/2)*sd(boot2$t)

#Percentile
quantile(boot2$t,.025,type = 5)
quantile(boot2$t,.975,type = 5)

#Basic
2*201.75 - quantile(boot2$t,0.975,type = 5)
2*201.75 - quantile(boot2$t,0.025,type = 5)


(ci.median = boot.ci(boot.out.median))
hist(boot.out.median$t)
#Normal
201.75 +0.032815 + c(-1,1)*qnorm(1.95/2)*sd(boot.out.median$t)
#Percentile
quantile(boot.out.median$t,0.025)
quantile(boot.out.median$t,.975)


#Baisc
2*mean(boot.out.median$t) - quantile(boot.out.median$t,0.975)
2*mean(boot.out.median$t) - quantile(boot.out.median$t,0.025)

#Example 1
library(ISLR)
alpha.fn = function(data,index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)
dim(Portfolio)
set.seed(10)
alpha.fn(Portfolio,sample(100,100,replace = TRUE))

alpha.boot = boot(Portfolio,alpha.fn,R = 1000)
alpha.boot
mean(alpha.boot$t)

sd(alpha.boot$t)

boot.ci(alpha.boot)

#Example 1
#library(ISLR)
boot.fn = function(data,index)
  return(coef(lm(mpg~horsepower,data = data,subset = index)))
boot.fn(Auto,1:392)

boot.fn(Auto,sample(392,392,replace = TRUE))

boot.out = boot(Auto,boot.fn,1000)
boot.out
head(boot.out$t)
sd(boot.out$t[,1])
sd(boot.out$t[,2])
summary(lm(mpg~horsepower,data = Auto))$coef
