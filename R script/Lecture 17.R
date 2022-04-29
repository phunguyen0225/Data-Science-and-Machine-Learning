#Import mower data
library(ggplot2)
ggplot(mower,aes(x = Income, y = Size,color = Own,shape = Own)) +
  geom_point()

#fitting the tree
library(tree)
mower$Own = as.factor(mower$Own)
tree.mower = tree(Own ~ Income + Size,data = mower)
summary(tree.mower)
plot(tree.mower)
text(tree.mower)
tree.mower



#Gini Index
tree.mower.gini = tree(Own ~ Income + Size, data = mower, split = "gini")
summary(tree.mower.gini)
plot(tree.mower.gini)
text(tree.mower.gini)
tree.mower.gini

set.seed(3)
cv.mower = cv.tree(tree.mower,FUN = prune.misclass)
cv.mower
plot(cv.mower$size,cv.mower$dev,type = "b")
prune.mower = prune.misclass(tree.mower,best = 3)
plot(prune.mower)
text(prune.mower)

#Example 2: Heart Data
#Import Heart.csv
Heart <- read.csv("E:/Lectures uh/Heart.csv")
head(Heart)
summary(Heart)
set.seed(100)
train = sample(1:nrow(Heart),nrow(Heart)/2+0.5)
Heart$AHD = as.factor(Heart$AHD)
Heart$ChestPain = as.factor(Heart$ChestPain)
Heart$Sex = as.factor(Heart$Sex)
Heart$Thal = as.factor(Heart$Thal)
summary(Heart)
tree.heart = tree(AHD ~ ., Heart,subset =  train)
plot(tree.heart)
text(tree.heart,pretty = 0)
summary(tree.heart)

#Checking Test Error Rate
Heart.test = Heart[-train,]
tree.pred = predict(tree.heart,Heart.test,type = "class")
table(tree.pred,Heart.test$AHD)
(18+13)/(60+18+13+18+60)

#Pruining
set.seed(3)
cv.heart = cv.tree(tree.heart,FUN = prune.misclass)
par(mfrow = c(1,2))
plot(cv.heart$size,cv.heart$dev,type = "b")
plot(cv.heart$k,cv.heart$dev,type = "b")

prune.heart = prune.misclass(tree.heart,best = 4)
plot(prune.heart)
text(prune.heart,pretty = 0)
levels(Heart$ChestPain)
levels(Heart$Thal)
summary(prune.heart)

#Test Error for Pruned Tree
tree.pred = predict(prune.heart,Heart.test,type = "class")
table(tree.pred,Heart.test$AHD)
(40)/(63+10+30+48)

#rpart package
install.packages("rpart")
library(rpart)
rtree.mower = rpart(Own ~ Income + Size,data = mower,
                    parms = list(prior = c(.5,.5), split = "information"))
plot(rtree.mower)
text(rtree.mower)
rtree.mower
