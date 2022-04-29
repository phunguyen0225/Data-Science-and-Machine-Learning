#install.packages("neuralnet")
library(neuralnet)
input = c(0,1,2,3,4,5,6,7,8,9,10)
output = input^2
mydata = data.frame(cbind(input,output))
names(mydata) = c("input","output")
mydata

set.seed(1)
model = neuralnet(output ~ input,  data = mydata, hidden = 10, 
                  threshold = 0.01) #how much error we are willing to give
model
plot(model)


yhat = as.data.frame(model$net.result)
final_output = cbind(input, output,yhat)
colnames(final_output) = c("Input","Expected output", "Neural Net Output")
print(final_output)

sum((output - as.data.frame(model$net.result))^2/2) #Mean Squared Error "MSE"
