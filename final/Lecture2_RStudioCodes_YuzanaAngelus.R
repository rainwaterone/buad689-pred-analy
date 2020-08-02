#Lecture2

# Install packages for today Lecture Data Modeling,Transformation and modeling 

install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages('colorspace')
library(colorspace)
library(tidyverse)
library(ggplot2)
install.packages('caret')
library(caret)
install.packages('caretEnsemble')
library(caretEnsemble)
install.packages('psych')
library(psych)
install.packages('Amelia')
library(Amelia)
install.packages('mice')
library(mice)
install.packages('GGally')
library(GGally)
install.packages('rpart')
library(rpart)
install.packages('randomForest')
library(randomForest)
#install Rattle
install.packages('RGtk2')
library(RGtk2)
install.packages('rattle')
library(rattle)


#start Rattle
rattle()


#Start Rcmdr
install.packages('Rcmdr')
library(Rcmdr)


# Data Modeling, House Price data

housePrice<- read.csv("C:/Users/rainwater-e/OneDrive - Texas A&M University/Summer-2020/BUAD 689 Predictive Analytics/class-07/Table 2.1_HousePrices.csv")
summary(housePrice)
str(housePrice)
head(housePrice)
describe(housePrice)

#lets do linear regression models using housePrice data
model1 <- lm(Price~SqFt, data=housePrice)
summary(model1)
model2 <- lm(Price~SqFt+Bedrooms+Bathrooms+Offers+Brick+factor(Neighborhood),data=housePrice)
summary(model2)

#Prediction Exercise Using Regression Equation
#What is the estimated price of a home with 2,000 sqft, 2 bedrooms, 1 bathroom, 2 offers, built without brick in the East Neighborhood?
HousepricePred <- (598.919 +(52.994*2000)+(4246.794*2)+(7883.278*1)+(-8267.488*2)+(17297.350*0)+(1560.579*0)+(22241.616*0))
HousepricePred




# Data Transformation, DirectMarketing Data

DMkt<- read.csv("C:/Users/rainwater-e/OneDrive - Texas A&M University/Summer-2020/BUAD 689 Predictive Analytics/class-07/DirectMarketing.data.csv")
model3 <- lm(AmountSpent~Salary+Location+Children+Catalogs,data=DMkt)
summary(model3)



#import data
DMkt<- read.csv("C:/Users/rainwater-e/OneDrive - Texas A&M University/Summer-2020/BUAD 689 Predictive Analytics/class-07/DirectMarketing.data.csv")

# Data Exploration, Visualization and summaries 

summary(DMkt)
str(DMkt)
head(DMkt)
describe(DMkt)

ggplot(DMkt, aes(AmountSpent, colour = Gender)) + geom_freqpoly(binwidth = 100) + labs(title="Spend Distribution by Gender")

attach(DMkt)
plot(Salary,AmountSpent,main="Scatterplot in R Studio", xlab="Salary ", ylab="AmountSpend", pch=8)
abline(lm(AmountSpent~Salary), col="red")
pairs(~AmountSpent+Catalogs+Salary,data=DMkt,
      main="Simple Scatterplot Matrix")


# Modeling regression without addressing heteroscedasticity

Non.LogModel <- lm(AmountSpent~Salary+Catalogs+Children, data=DMkt)
summary(Non.LogModel)


# Data Transformation, Log Transform

Log.model <- lm(log(AmountSpent)~log(Salary)+Location+Children+Catalogs, data=DMkt)
summary(Log.model)

#Log Transformed Regression Model 
Log.modelAll <- lm(log(AmountSpent)~Location+Children+log(Salary)+log(Catalogs)+Married+Age+Gender+OwnHome, data=DMkt)
summary(Log.modelAll)

#Prediction Exercise with log-transforemd Regression Equation
#What is the estimated AmountSpend of a homeowner,married, middle age man with no kids, no stores nearby his house, 
#he earns $150,000 salary when he received 22 catalogs in total?

#Tranform salary, catalogs values into Log 
logSalary.150K <- log(150000)
logSalary.150K

logCalatog.22 <- log(22)
logCalatog.22

# Forecast log value of AmountSpend 
# Log-transformed Regression Equation
# Log.AmountSpend = (Intercept)+ Coef.log(Salary)*logSalary.150K + Coef.Children* no.of.Children + Coef.log(Catalogs)*logCalatog.22 
#                    + Coef.Married[T.Single]* (Marital.status:1;0) + Coef.Age[T.Old]* (Age.old: 1;0) + Coef.Age[T.Young]* (Age.Young: 1;0)
#                    + Coef.Gender[T.Male]* (Gender.M : 1;0) + Coef.OwnHome[T.Rent]* (Home.Rent:1;0)


Log.AmountSpendPred <- (-5.105728 +(0.992199 *logSalary.150K)+(0.414199 *1)+(-0.235019 *0)+(0.508940 *logCalatog.22)+(0.036268 *0)+(-0.016793*0)+(0.0549686*0)+(-0.008519*1)+(-0.046933*0))
Log.AmountSpendPred

# Forecast of actual value on AmountSpend
AmountSpend.pred <- (2.718^Log.AmountSpendPred)
AmountSpend.pred



# Calculating Prediction Intervals for 95% confidence 
# 95% Upper and lower prediction interval with Residual standard error: 0.3652
LogSE <- 0.3652
Log.AmountSpendPred <- 8.698522



Log.UpperPredInterval <-Log.AmountSpendPred +(2*LogSE)
Log.lowerPredInterval <- Log.AmountSpendPred -(2*LogSE) 

#Actual value of Upper prediction interval 
UpperPredInterval <- 2.718^Log.UpperPredInterval
UpperPredInterval

# Actual Value of Lower prediction interval 
lowerPredInterval <- 2.718^Log.lowerPredInterval
lowerPredInterval
