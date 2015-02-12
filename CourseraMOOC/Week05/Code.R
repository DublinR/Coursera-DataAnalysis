###Question 1####
data(warpbreaks)
wb<- warpbreaks
head(wb)
aovObject <- aov(wb$breaks ~ wb$wool + wb$tension) 
aovObject
summary(aovObject)

###Question 2####
log(2/8)
log(.2/(1-.2))

###Question 3####
library(glm2)
data(crabs)
head(crabs)
?crabs
glm1 <- glm(Satellites ~ Width, data=crabs, family="poisson")
summary(glm1)
glm1
exp(-3.305+0.164*(0+1))/(exp(-3.305+0.164*(0)))
exp(-3.305+0.164*(2+1))/(exp(-3.305+0.164*(2)))
exp(glm1$coefficients[1]+glm1$coefficients[2]*(0+1))/exp(glm1$coefficients[1]+glm1$coefficients[2]*(0))

###Question 4####
exp(glm1$coefficients[1]+glm1$coefficients[2]*(22))

###Question 5####
library(MASS)
data(quine) 
head(quine)
lm1 = lm(log(Days + 2.5) ~.,data=quine)
summary(lm1)
aicFormula <- step(lm1)
aicFormula
lm1 = lm(log(Days + 2.5) ~.,data=quine)
