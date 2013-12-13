## Download and load data
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda"
              , "data/assignment1_orig.rda")
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf"
              , "doc/LoansCodeBook.pdf", mode="wb")
load("data/assignment1_orig.rda")
##################################################################
## Check it out
summary(loansData)
str(loansData)
head(loansData)
##################################################################
# read data
df <- read.csv("data/raw/loansData.csv")

# preprocessing
# remove percents
df$Interest.Rate <- as.numeric(gsub("%$","",df$Interest.Rate))
# remove months
df$Loan.Length.Num <- as.numeric(gsub(" months","",df$Loan.Length))
# FICO: 640-644, 645-649, ... 830-834
# converting to 1, 2, 3, ..., 38
levels(df$FICO.Range) = seq(38)
# convert factor to numerical
df$FICO.Range <- as.numeric(df$FICO.Range)


######################################################################


## Convert fields to appropriate formats
loansData$Interest.Rate <- as.numeric(gsub("%","",loansData$Interest.Rate))/100
loansData$Debt.To.Income.Ratio <- as.numeric(
  gsub("%","",loansData$Debt.To.Income.Ratio))/100
loansData$Amount.Requested <- as.numeric(loansData$Amount.Requested)
loansData$Revolving.CREDIT.Balance <- as.numeric(loansData$Revolving.CREDIT.Balance)

hist(loansData$Interest.Rate)
##################################################################
## Quick visual inspection of all variables against Interest Rate
par(mfrow=c(3,5)) # Plot all graphs in one panel
for (col in 1:ncol(loansData)) {
  if (names(loansData)[col] != "Interest.Rate") {
    plot(loansData[,col], loansData$Interest.Rate, xlab=names(loansData)[col])
  }
  rm(col)
}



This repository
Explore
Gist
Blog
Help
DublinR
1  Watch
Star 0 Fork 1PUBLIC gmarv / Data_Analysis_Coursera
 branch: master  Data_Analysis_Coursera / GM_Assignment1.R 
 gmarv 10 months ago test_changes
1 contributor
 file 146 lines (117 sloc) 6.273 kb  Open EditRawBlameHistory Delete
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
64
65
66
67
68
69
70
71
72
73
74
75
76
77
78
79
80
81
82
83
84
85
86
87
88
89
90
91
92
93
94
95
96
97
98
99
100
101
102
103
104
105
106
107
108
109
110
111
112
113
114
115
116
117
118
119
120
121
122
123
124
125
126
127
128
129
130
131
132
133
134
135
136
137
138
139
140
141
142
143
144
145
#      **Coursera Data Analysis Course**  
# ===================================================

# Data Analysis Assignment 1  
# ------------------------------
# Giles Martin, February 2013
# Would have liked to have done .rdm file but what can you do?

library(stringr)
library(psych)


# read in loansData.rda
  
setwd("C:/Users/giles.martin/Desktop/R")
fileUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda"
download.file(fileUrl,destfile="./loans_data.rda")
load("C:/Users/giles.martin/Desktop/R/loans_data.rda")

# remove periods from column names
names(loansData) <- gsub("\\.","_",names(loansData))

# get basic metadata
dim(loansData) #  2500 x 14 dataset
summary(loansData)
# check for missing values
barplot(colSums(is.na(loansData)), main = "Number of NAs by field", las = 2)
# axis(1, 1:14, names(loansData[1:14]), cex.axis = 0.5, las = 2)
# this data is actually revealed within the summary function
sapply(loansData,class) # get column classes

# we have some tricky classes here:
# Interest rate - factor
# Debt to income ratio - factor
# FICO Range - factor - take the average of the range and convert to numeric
# We will likely get more from the analysis if these are all (in some way) numeric

# These two are probably OK for now
# Loan length - factor - I think it only has two values
unique(loansData$Loan_Length)
###################################################################
# Employment length - factor - contains '<' and '+' characters. Keep as factor for now

# Convert factors to numeric
loansData$Interest_Rate <- as.numeric(gsub("%","",loansData$Interest_Rate))
loansData$Debt_To_Income_Ratio <- as.numeric(gsub("%","",loansData$Debt_To_Income_Ratio))
loansData$Loan_Length <- as.numeric(gsub(" months","",loansData$Loan_Length))

hist(loansData$Amount_Requested, breaks=50) # Right-skewed distribution
hist(loansData$Interest_Rate, breaks=100) # Right-skewed distribution (mild)
loansData$FICO_Range <- as.numeric((str_sub(loansData$FICO_Range,1,3)))
loansData$FICO_Range <- loansData$FICO_Range + 2 # this needs to be split into two rows
###################################################################
# here's another way to transform FICO
#loansData <- data.frame(loansData[,-10], FICO_Range = as.numeric(str_sub(loansData$FICO_Range,1,3))) 

hist(loansData$FICO_Range, breaks=50) #  right skewed
hist(loansData$Open_CREDIT_Lines, breaks=50) #  right skewed
hist(loansData$Revolving_CREDIT_Balance, breaks=50) #  right skewed
hist(loansData$Inquiries_in_the_Last_6_Months, breaks=10) #  right skewed
###################################################################
# boxplot of interest rate by FICO Range
boxplot(loansData$Interest_Rate ~ loansData$FICO_Range)
# grid of scatterplots (numeric / integer variables)
pairs(~Interest_Rate+FICO_Range+Open_CREDIT_Lines+Revolving_CREDIT_Balance+Amount_Requested+Debt_To_Income_Ratio,data=loansData)
# boxplots to look at non-numeric variables
par(mfrow = c(2,3))
boxplot(loansData$Interest_Rate ~ loansData$State)
# opportunity to cluster by state
boxplot(loansData$Interest_Rate ~ loansData$Loan_Length,
        main = "Figure 2. Interest Rate by Length of Loan", 
        xlab = "Lenght of Loan (Months)",
        ylab = "Interest Rate (%)") # 60 month rate is higher than 36 month rate
boxplot(loansData$Interest_Rate ~ loansData$Loan_Purpose)
# although there is some variability in interest rate by loan purpose, it turns out to be
# not a particularly significant variable in the model
boxplot(loansData$Interest_Rate ~ loansData$Home_Ownership)
boxplot(loansData$Interest_Rate ~ loansData$Employment_Length)

###################################################################      
# create exploratory model
# first create training set w/ 70% of the data
set.seed(61076)
index <- rbinom(nrow(loansData),1,.7)
trainingSet <- loansData[index==1,]
testSet <- loansData[index==0,]
model1 <- lm(Interest_Rate ~ FICO_Range + Open_CREDIT_Lines + 
    Amount_Requested + Debt_To_Income_Ratio + Loan_Length + Monthly_Income + 
        Revolving_CREDIT_Balance + Inquiries_in_the_Last_6_Months, data = trainingSet)
print(summary(model1))
###################################################################
# Summary of model 1
# D-T-I ratio and revolving credit balance are not significant
# Monthly income significant at 95% level
# All other variables highly significant
# this model is promising - adj r-sq of .
# create a data frame of predictions vs actuals, chart, and calculate total error
# For training set
pred1 <- cbind(data.frame(predict(model1, trainingSet)), trainingSet[,3])
colnames(pred1) <- cbind("Predicted_Interest_Rate", "Actual_Interest_Rate")
plot(pred1$Predicted_Interest_Rate, pred1$Actual_Interest_Rate, 
     xlim=c(0,25),ylim=c(0,25), main = "1. Training Set, Predicted vs Actual Interest Rates",
     xlab = "Predicted Interest Rate (%)", ylab = "Actual Interest Rate (%)")
abline(0,1, col = 2)
err1.0 <- as.data.frame(pred1$Predicted_Interest_Rate - pred1$Actual_Interest_Rate)
err1.0 <- as.data.frame(err1.0[complete.cases(err1.0),])
err1.1 <- err1.0^2
trainingError <- sqrt(colSums(err1.1))

###################################################################
# create a data frame of predictions vs actuals, chart, and calculate total error
# For test set
pred2 <- cbind(data.frame(predict(model1, testSet)), testSet[,3])
colnames(pred2) <- cbind("Predicted_Interest_Rate", "Actual_Interest_Rate")
plot(pred2$Predicted_Interest_Rate, pred2$Actual_Interest_Rate, 
     xlim=c(0,25),ylim=c(0,25), main = "Figure 1. Predicted vs Actual Interest Rates (Test Set)",
     xlab = "Predicted Interest Rate (%)", ylab = "Actual Interest Rate (%)")
abline(0,1, col = 2)
err2.0 <- as.data.frame(pred2$Predicted_Interest_Rate - pred2$Actual_Interest_Rate)
err2.0 <- as.data.frame(err2.0[complete.cases(err2.0),])
err2.1 <- err2.0^2
testError <- sqrt(colSums(err2.1))

# Two additional steps recommended
# 1. Standardize variables to get an even view of scale of variable importance
# (otherwise coefficients are hard to interpret)

###################################################################
# 1. Generate z-scores for variable A using the scale() function
# First remove non-numeric variables
nums <- loansData[,sapply(loansData, is.numeric)]
ints <- loansData[,sapply(loansData, is.integer)]
loansDataNum <- cbind(nums, ints)
# Now scale data
loansDataNum <- na.omit(loansDataNum)
loansDataScaled <- as.data.frame(scale(loansDataNum, center = TRUE, scale = TRUE))
model3 <- lm(Interest_Rate ~ FICO_Range + Open_CREDIT_Lines + 
               Amount_Requested + Loan_Length + Monthly_Income + 
               Inquiries_in_the_Last_6_Months, data = loansDataScaled)
summary(model3)



###################################################################


# using ggplot2
require(ggplot2)

# colorize Loan.Length: correlation is seen: 60 months gets higher percents
qplot(FICO.Range, Interest.Rate, data=df, colour=Loan.Length)
qplot(FICO.Range, Interest.Rate, data=df, colour=Amount.Requested, xlab="FICO score", ylab="Interest rate (%)")
qplot(FICO.Range, Interest.Rate, data=df, colour=Amount.Funded.By.Investors)
qplot(FICO.Range, Interest.Rate, data=df, xlab="FICO score", ylab="Interest rate (%)")
##################################################################################################
# Fitting linear regression
lm(formula = df$Interest.Rate ~ df$FICO.Range)
summary(lm1)
anova(lm1)

lm(formula = df$Interest.Rate ~ as.factor(df$FICO.Range) + df$Loan.Length.Num)
summary(lm1)
anova(lm1)

lm(formula = df$FICO.Range ~ df$Amount.Requested)
summary(lm1)

lm(formula = df$FICO.Range ~ df$Amount.Funded.By.Investors)
summary(lm1)
##################################################################
