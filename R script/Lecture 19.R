#Heart data set is in the ISLR library
library(ISLR)
library(tree)
n = nrow(Hitters)
included.vars = names(Hitters[-c(3,8:15,20)])
Hitters2 = na.omit(Hitters) #take out missing value


hitters.test = NA
yhat = NA
hit.mse = NA
par(mfrow = c(1,2))

for (i in 1:2) {
  train = sample(1:n, 0.5*n)
  tree.model = tree(log(Salary) ~., 
                    data = Hitters2[,included.vars],
                    subset = train)
  prune.model = prune.tree(tree.model,best = 5)
  plot(prune.model); text(prune.model,pretty = T)
  hitters.test = Hitters2[-train,"Salary"]
  yhat = predict(prune.model,newdata = Hitters2[-train,included.vars])
  hit.mse[i] = mean((yhat - log(hitters.test))^2)
}
hit.mse

par(mfrow = c(1,1))


set.seed(1)
for (i in 1:2) {
  train = sample(1:n, .5*n)
  lm.model = lm(log(Salary) ~., 
                data = Hitters2[,included.vars],
                subset = train)
  hitters.test = Hitters2[-train,"Salary"]
  yhat = predict(lm.model,newdata = Hitters2[-train,included.vars])
  hit.mse[i] = mean((yhat - log(hitters.test))^2)
  print(lm.model$coeff)
}
hit.mse
predict(lm.model,Hitters2)

#Example of Bagging
#regression
#install.packages('randomForest')
library(randomForest)
set.seed(1)
p = ncol(Hitters2) - 4 #No. of predictors
B = 100 #No. of bootstrap trees
bag.model.p = randomForest(log(Salary) ~. -NewLeague-League-Division,
                           data = Hitters2,
                           ntree = B,
                           mtry = p) #how many predictor at one tree does you wanna see that work
bag.model.p #To get an outline of bagging results.
bag.model.p$predicted


#Example of Classification Tree
#Using Heart data
#Heart <- read.csv("E:/Lectures uh/Heart.csv")
Heart = na.omit(Heart); Heart$X = NULL 
Heart$AHD = as.factor(Heart$AHD)
Heart$ChestPain = as.factor(Heart$ChestPain)
Heart$Sex = as.factor(Heart$Sex)
Heart$Thal = as.factor(Heart$Thal)


train = sample(1:nrow(Heart),nrow(Heart)/2+0.5)
tree.heart = tree(AHD ~ ., data = Heart,subset =  train)
Heart.test = Heart[-train,]
tree.pred = predict(tree.heart,Heart.test,type = "class")
(conf.matrix = table(tree.pred,Heart.test$AHD))
(conf.matrix[1,2]+conf.matrix[2,1])/sum(conf.matrix) #test error rate

#bagging
n = nrow(Heart); p = ncol(Heart)-1 #-1 because AHD is not a predictor
B = 1000 #number of bootstrap tree
Heart$AHD = as.factor(Heart$AHD)
bag.model = randomForest(AHD ~., data = Heart,
                         ntree = B,
                         mtry = p,
                         importance = TRUE)
bag.model$predicted
bag.model

varImpPlot(bag.model)

#Random Forest Model  -> a little better than bagging
Heart = na.exclude(Heart)
AHD.test = as.factor(Heart[-train,"AHD"])
X.test = Heart[-train,-14] #Take away the AHD column
rf.model = randomForest(AHD ~., data = Heart,
                        subset = train,
                        xtest = Heart.test[,-14], ytest = Heart.test$AHD,
                        ntree = B,
                        mtry = sqrt(p),
                        importance = TRUE)
rf.model


varImpPlot(rf.model)

#RF for Hitters data
p = ncol(Hitters2) - 4 #No. of predictors
B = 100 #No. of bootstrap trees
bag.model.rf = randomForest(log(Salary) ~. -NewLeague-League-Division,
                           data = Hitters2,
                           ntree = B,
                           mtry = p/3,
                           importance = TRUE)
bag.model.rf #To get an outline of bagging results.
bag.model.rf$predicted
varImpPlot(bag.model.rf)



#Boosting
#install.packages("gbm")
library(gbm)
set.seed(1)
boost.hitters = gbm(log(Salary) ~. -NewLeague-League-Division,
                    data = Hitters2,
                   distribution = "gaussian") #for regression
summary(boost.hitters)


#Check MSE

yhat.boost = predict(boost.hitters,newdata = Hitters2[-train,], 
                     n.trees = 100)
mean((yhat.boost- log(Hitters2[-train, "Salary"]))^2)

#Change shrinkage to 0.2
boost.hitters = gbm(log(Salary) ~. -NewLeague-League-Division,
                    data = Hitters2,
                    distribution = "gaussian",
                    shrinkage=0.2,verbose=F)
summary(boost.hitters)
yhat.boost=predict(boost.hitters,newdata=Hitters2[-train,],n.trees=100)
mean((yhat.boost- log(Hitters2[-train, "Salary"]))^2)

