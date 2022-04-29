?mtcars #help function on mtcars
dim(mtcars) #dimension  first: #of observation; second: # of variables
head(mtcars) #show the first 6 rows of that data set, to observe how the data look like
plot(wt, mpg) #plot(weight, mpg), this should give an error, it doesn't recognize where the data set is coming from
#or we can do attach(mtcars) to make the variables in this data fram available by name and we can
#detach once we're done

#attach(mtcars)
#plot(wt,mpg)
#detach(mtcars)

#rm(mpg) is to remove a global variable

plot(mtcars$wt, mtcars$mpg) #scatter plot, negative linear relationship

summary(mtcars) #show the variables min, 1st quatile, median, mean, 3rd quatile and max,
#however the cyl suppose to be categorical values

cyl = as.factor(mtcars$cyl) #set the variable name "cyl" to as.factor() to a categorical values
#so we can perform opperation 

plot(cyl, mtcars$mpg) 

summary(mtcars$mpg) #summary of mpg


