#Data Analysis Week 7 Quiz
#########################################################################
###Question 2####
set.seed(53535)
xValues = seq(0,2*pi,length=100)
yValues = rnorm(100) + sin(xValues)

plot(xValues, yValues)
library(splines)
library(devtools)
install_github("medley", "mewo2")
library(medley)

xSpline <- ns(xValues, df=10)
head(xSpline)

lm <- lm(yValues ~ xSpline)
summary(lm)
names(lm)
lm$residuals
rmse(yValues, lm$residuals)

lapply(1:10, function(i){
     rmse(yValues, lm(yValues ~ (ns(xValues, df = i)))$fitted)
} )

#########################################################################
###Question 3####

library(simpleboot) 
data(airquality)
attach(airquality)
summary(Wind)
set.seed(883833)
quantile(Wind, 0.75)

Func <- function(x) quantile(x, 0.75)
boot.output <- one.boot(Wind, Func, R=1000)
names(boot.output)
apply(boot.output$t, 2, sd)
?apply
#########################################################################
###Question 4####
data(Cars93,package="MASS")
set.seed(7363)
tree.output1 <- tree(DriveTrain ~ Price + Type, Cars93[sample(1:dim(Cars93)[1], replace = TRUE),])
tree.output2 <- tree(DriveTrain ~ Price + Type, Cars93[sample(1:dim(Cars93)[1], replace = TRUE),])
tree.output3 <- tree(DriveTrain ~ Price + Type, Cars93[sample(1:dim(Cars93)[1], replace = TRUE),])
newdata = data.frame(Type = "Large",Price = 20)
summary(predict(tree.output1, data=newdata, type="class"))
summary(predict(tree.output2, data=newdata, type="class"))
summary(predict(tree.output3, data=newdata, type="class"))
plot(tree.output1)
text(tree.output1)
#########################################################################
###Question 5####
library(ElemStatLearn)
library(randomForest)
library(e1071)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
rf.output <- randomForest(y ~ ., data=vowel.train)
rf.predict <- predict(rf.output, vowel.test)
length(which(vowel.test$y != rf.predict)) / nrow(vowel.test)

svm.output <- svm(y ~ ., data=vowel.train)
svm.predict <- predict(svm.output, vowel.test)
length(which(vowel.test$y != svm.predict)) / nrow(vowel.test)

length(which(vowel.test$y[rf.predict == svm.predict] != svm.predict[rf.predict == svm.predict])) / nrow(vowel.test[rf.predict == svm.predict,])
