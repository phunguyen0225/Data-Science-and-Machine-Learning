library(ISLR)
data("Smarket")

#Exploring the data
summary(Smarket)
cor(Smarket)
cor(Smarket[,-9])

#Splitting into training and test
train = (Smarket$Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Smarket.2005$Direction

#The Model
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

#The Predictions
lda.pred = predict(lda.fit,Smarket.2005)
lda.class = lda.pred$class
table(lda.class,Direction.2005)
141/252
mean(lda.class == Direction.2005)

#Using mtcars
cars.lda = lda(cyl.fac ~mtcars$mpg +mtcars$hp)
cars.lda
cars.pred = predict(cars.lda)
table(cyl.fac,cars.pred$class)
29/32
