# Week 8 Quiz has four parts

################################################################################################
####Question 1####
# Theory Question
#False Discovery Rate
5/25
#False Positive Rate
5/55
################################################################################################
###Question 2####
set.seed(1010093) 
pValues <- rep(NA,1000)
for(i in 1:1000){
  y <- rnorm(20)
  x <- rnorm(20)
  pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
class(pValues)
# Controls false positive rate
sum(pValues < 0.05)
################################################################################################
###Question 3####
set.seed(3343)
pValues = rep(NA,100)
for(i in 1:100){
  z = rnorm(20)
  x = rnorm(20)
  y = rnorm(20,mean=0.5*x)
  pValues[i] = summary(lm(y ~ x))$coef[2,4]
}
############################

# Controls false positive rate
sum(pValues < 0.1)

# Controls FWER
sum(p.adjust(pValues,method="bonferroni") < 0.1)

# Controls FDR
sum(p.adjust(pValues,method="BH") < 0.1)
################################################################################################
###Question 4####
set.seed(44333)
betaNorm <- betaNormModifiedX <- betaNormModifiedY <- rlmModifiedX <-rlmModifiedY <- rep(NA,1000) 
for(i in 1:1000){
  x <- rnorm(50); e <- rnorm(50); b0 <- 1; b1 <- 2
  y<- b0+b1*x+e;
  xModifiedX <- x[which(x<0.781)]
  yModifiedX <- y[which(x<0.781)]
  xModifiedY <- x[which(y<3.09)]
  yModifiedY <- y[which(y<3.09)]
  betaNorm[i] <- lm(y ~ x)$coeff[2]; 
  betaNormModifiedX[i] <- lm(yModifiedX ~ xModifiedX)$coeff[2];
  betaNormModifiedY[i] <- lm(yModifiedY ~ xModifiedY)$coeff[2];
  rlmModifiedX[i] <- rlm(yModifiedX ~ xModifiedX)$coeff[2];
  rlmModifiedY[i] <- rlm(yModifiedY ~ xModifiedY)$coeff[2];
}

quantile(xModified)
quantile (x)
quantile (y)
par(mfrow=c(1,1))

plot(betaNorm)
quantile(betaNorm)
quantile(betaNormModifiedX)
quantile(betaNormModifiedY)
quantile(rlmModifiedX)
quantile(rlmModifiedY)

quantile(betaNorm2)
xModified <- x[which(x<2.085341)]
xModified

lm1 = lm(y ~ xModified + e)
lm1

yModified <- y[which(y<1.0932)]
lm2 = lm(yModified ~ x + e)
lm2
