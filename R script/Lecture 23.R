install.packages("NeuralNetTools")
library(NeuralNetTools)
library(nnet)

#Import dataset ResturantTips
attach(RestaurantTips)
names(RestaurantTips)


#Train the model based on output from imput
RestaurantTips$CustomerWillTip = as.factor(RestaurantTips$CustomerWillTip)

model = nnet(CustomerWillTip ~ Service + Ambience + Food,
             data = RestaurantTips,
             size = 5,
             rang = 0.1,
             decay = 5e-2,
             maxit = 5000)
print(model)
summary(model$residuals)
model$fitted.values

pred_nnet<-predict(model,RestaurantTips,type = "class")
(mtab<-table(RestaurantTips$CustomerWillTip,pred_nnet))

plotnet(model)
garson(model)
