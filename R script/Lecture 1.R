#Hashtag for notes in Script

x = rnorm(50, mean = 75, sd = 10) #50 samples mean = 75, sd = 10
y = rnorm(50, mean = 100, sd = 5) #50 samples mean = 100, sd = 5
plot(x,y) #scatterplot
xy.lm = lm(y ~ x) #linear model y = b0 + b1x
summary(xy.lm) #summary of linear model
abline(xy.lm) #attached line to plot
hist(x) #histogram of x
qqnorm(x) #Q-Q plot to determine if Normal
qqline(x) #Attach a 45-degreee line


