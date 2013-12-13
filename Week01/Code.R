######################################################
#Question 3
set.seed(31);
heightsCM = rnorm(30,mean=188, sd=5);
weightsK = rnorm(30,mean=84,sd=3); 
hasDaughter = sample(c(TRUE,FALSE),size=30,replace=T); 
dataFrame = data.frame(heightsCM,weightsK,hasDaughter); 
dataFrameSubset <- dataFrame[heightsCM > 188, c(1:3)]
mean(dataFrameSubset$weightsK)


########################################################
#Question 4
set.seed(41)
cauchyValues<-rcauchy(100)
set.seed(415)
sample(cauchyValues,10,replace=TRUE)[1:3]
########################################################
