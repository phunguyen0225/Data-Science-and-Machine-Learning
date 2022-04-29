#Input the data
Year <- c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016)
Month <- c(12, 11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1)
Interest_Rate <- c(2.75,2.5,2.5,2.5,2.5,2.5,2.5,2.25,2.25,2.25,2,2,2,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75)
Unemployment_Rate <- c(5.3,5.3,5.3,5.3,5.4,5.6,5.5,5.5,5.5,5.6,5.7,5.9,6,5.9,5.8,6.1,6.2,6.1,6.1,6.1,5.9,6.2,6.2,6.1)
Stock_Index_Price <- c(1464,1394,1357,1293,1256,1254,1234,1195,1159,1167,1130,1075,1047,965,943,958,971,949,884,866,876,822,704,719) 

#combine the vectors into one data frame
stock_price <-cbind(Year,Month,Interest_Rate,Unemployment_Rate,Stock_Index_Price)

#Save the data as a .csv file.
write.csv(stock_price,file = "E:\\Lectures uh\\stock_price.csv")
View(stock_price) #view the data in the script window

stock_price<-data.frame(stock_price) #converted as a data frame

#Simple Linear Regression (lm function)
stock.lm <- lm(Stock_Index_Price~Interest_Rate,data = stock_price)
summary(stock.lm) 

#Scatterplot with regression line
plot(Interest_Rate,Stock_Index_Price,pch = 16, col = "red",xlab = "",ylab = "")
abline(stock.lm)

#Get plots for checking assumptions
par(mfrow=c(2,2)) #puts the plots into 2 rows and 2 columns
plot(stock.lm) 

#Critical Value for confidence interval
qt(.975,22)
564.20-2.0739*45.32 #lower limit
564.20+2.0739*45.32 #upper limit

#Confidence Interval of the slope
confint(stock.lm,"Interest_Rate")
#For each 1% increase in the interest rate, the stock index price will increase
#between $470.22 and $658.19 with 95% confidence.

#Hypothesis test:
#RH0
#There is very strong evidence of a relationship between the interest rate
#and the stock index price.

#R^2 = 0.8757
#About 87.57% of the variation in the stock index price can be explained
#by this model.


#Lab 2
#Install the packages (only have to do  once)
install.packages("MASS") 
install.packages("ISLR")
#Load the packages (Have to do everytime you open R)
library(MASS)
library(ISLR)
lm.fit = lm(medv~lstat,data = Boston)
summary(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat = c(5,10,15)),
        interval = "confidence")

par(mfrow=c(2,2))
plot(lm.fit)

plot(Boston$lstat ,Boston$medv,
     xlab="lstat",ylab = "medv")
abline (lm.fit)
abline(lm.fit,lwd=3,col = "red")
plot(Boston$lstat,Boston$medv,col="red")
plot(Boston$lstat,Boston$medv,pch = 20)
plot(Boston$lstat,Boston$medv,pch = "+")
plot(1:20,1:20,pch=1:20) #shows the different plotting characters

