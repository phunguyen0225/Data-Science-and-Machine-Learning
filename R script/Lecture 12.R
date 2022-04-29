                     #LINEAR DISCRIMNINANT ANALYSIS (LDA)

library(ISLR)
data(Smarket)
cor(Smarket) #error because it have to be numeric
summary(Smarket)
cor(Smarket[,-9])

attach(Smarket)
train = (Year < 2005) #gets the observation before 2005
Smarket.2005 = Smarket[!train,] # this is the 2005 observations
#test data that is not '!' in the training test 
#taking away the rows that is in the training set

dim(Smarket.2005) #the dimension of test dataset [1] 252 9 -> 252 observation, 9 variables
Direction.2005 = Smarket.2005$Direction #separate out the direction variable, this is the response

library(MASS) #package for lda, lda is for more then 2 categorical output
#model
lda.fit = lda(Direction ~ Lag1 + Lag2 ,data = Smarket, subset = train)
lda.fit #call the function

#prediction function
lda.pred = predict(lda.fit, Smarket.2005) #predict in the testing, for the 2005
lda.pred
lda.class = lda.pred$class #create a class
table(lda.class, Direction.2005)
(75+35)/252 #error rate
mean(lda.class == Direction.2005) #acceptance rate, percent of up and down

#draw the discrimination lines
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
ggplot(Smarket, aes(x = Lag1, y = Lag2)) + geom_point((aes(color = Direction)))

ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point(aes(color = as.factor(cyl)))

#mtcars example
cars.lda = lda(as.factor(cyl) ~ mpg + hp, data = mtcars)
cars.lda

#predict
cars.pred = predict(cars.lda)
table(as.factor(mtcars$cyl), cars.pred$class)
dim(mtcars)
(3/32) #error rate



                      #QUADRATIC DISCRIMINANT ANALYSIS (QDA)
# the quadratic discriminant analysis assumes that the variance - covaraince matrix
# is not the same for all K classes
trainQDA = (Year < 2005)
Smarket.2005.QDA = Smarket[!trainQDA,]
Direction.2005.QDA = Smarket.2005.QDA$Direction
#model
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = trainQDA)
qda.fit

qda.class = predict(qda.fit , Smarket.2005.QDA)$class #getting the class
table(qda.class, Direction.2005.QDA) #error rate
(81 + 20)/252 
mean(qda.class == Direction.2005.QDA) #acceptance rate


#Boston dataset
data("Boston")
#Separate the data between training and test
set.seed(10)
sample = sample.int(n = nrow(Boston), size = floor(0.75*nrow(Boston)), replace = F)
train = Boston[sample,]
test = Boston[-sample,]

#create a new variable crim01 that is 1 if above the median, 0 if below the median
train$crim01 = (train$crim > median(train$crim))
test$crim01 = (test$crim > median(test$crim))

#logistic regression
fit.glm = glm(crim01 ~ age + medv, data = train, family = "binomial")
glm.pred = predict.glm(fit.glm, test, type = "response")
yHat = glm.pred > 0.5
table(test$crim01, yHat)
(13+15)/(49+15+13+50) #error rate


#lda results
fit.lda = lda(crim01 ~ age + medv, data = train)
table(test$crim01, predict(fit.lda,test)$class)
(13+19)/(45+19+13+50) #error rate


#qda results
fit.qda = qda(crim01 ~ age + medv, data = train)
table(test$crim01, predict(fit.qda, test)$class)
(11+15)/(49+15+11+52) #error rate 

