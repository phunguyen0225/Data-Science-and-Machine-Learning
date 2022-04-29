?mtcars
dim(mtcars)
head(mtcars)
plot(wt,mpg)
#rm(mpg)
plot(mtcars$wt,mtcars$mpg)

cyl = as.factor(mtcars$cyl)
plot(cyl,mtcars$mpg)
pairs(~mpg+disp+hp+wt,mtcars)

summary(mtcars$mpg)
