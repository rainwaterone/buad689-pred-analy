#Lecture 3

#Load libraries required for today lecture 

library(colorspace)
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)

#Import your data file called "Table 4.1 Gender Discrimination.csv" from your data file directory
# and name that data file "GenderDiscrim"
GenderDiscrim <- read.csv("/Users/aangelus/Documents/R.data/Table 4.1 Gender Discrimination.csv")


# View your data file that you just imported
View(GenderDiscrim)


# Run your first linear regression model named "Sal.using.gender"
# to predict "Salary" using "Gender" as predictor 

Sal.using.gender <-lm(Salary~factor(Gender), data=GenderDiscrim)
summary(Sal.using.gender)



# Now add Experience into your first linear regression model and 
# named as "Sal.using.Gen.Exp" to predict "Salary" using two variables:  "Gender" and "Experience" as predictors 

Sal.using.Gen.Exp <- lm(Salary~ factor(Gender)+ Experience, data=GenderDiscrim)
summary(Sal.using.Gen.Exp)


# Run your third linear regression model with interaction term named "Sal.int.Gen.Exp"
# to predict "Salary" using interaction term between "Gender" * "Experience" as predictor 

Sal.int.Gen.Exp <- lm(Salary~ Gender*Experience, data=GenderDiscrim)
summary(Sal.int.Gen.Exp)



# Curing Multicollinearity

#Import your data file called "Experience_Age.csv" from your data file directory
# and name that data file "Exp.Age"
Exp.Age <- read.csv("/Users/aangelus/Documents/R.data/Experience_Age.csv")


# View your data file that you just imported
View(Exp.Age)

#Let's do the scatterplot Matrix & Correlation in Rcmdr
library(Rcmdr)

#Load Rattle to study the Correlation plot visualization

library(RGtk2)
library(rattle)

#start Rattle
rattle()

# Run your linear regression model named "Age.Exp"
# to predict "Wage" using "AGE + EDUCATION + EXPERIENCE + MARR + SECTOR" as predictors 

Age.Exp.lm1 <-lm(WAGE ~ AGE + EDUCATION + EXPERIENCE + MARR + SECTOR, data = ExpAge)
summary(Age.Exp.lm1)

# Run your linear regression model named "Wage.Exp.NoAge"
# to predict "Wage" without "Age", use the rest of "EDUCATION + EXPERIENCE + MARR + SECTOR" as predictors 

Wage.Exp <-lm(WAGE ~ EDUCATION + EXPERIENCE + MARR + SECTOR, data = ExpAge)
summary(Wage.Exp)


#Financial Indicators 

#Import your data file called "Lec3.Financial.Indicators.csv" from your data file directory
# and name that data file "Fin.Indt "
Fin.Indt <- read.csv("/Users/aangelus/Documents/R.data/Lec3.Financial.Indicators.csv")

# View your data file that you just imported
View(Fin.Indt)

#View summary of your data file 
summary(Fin.Indt)

# Remove NA/Missing values from your data file that you just imported
Remove.NA.Fin.Indt <- na.omit(Fin.Indt)


# Run regression on "Stock.Price" using all predictor variables 
# use data file called "Remove.NA.Fin.Indt" from previous step

Fin.Indt.lm1 <-lm(Stock.Price ~.,data=Remove.NA.Fin.Indt)
summary(Fin.Indt.lm1)

# Run Stepwise regression model to perform variable selection
Step.Reg.Model <- step(Fin.Indt.lm1, direction = "both", trace = FALSE)
Step.Reg.Model
summary(Step.Reg.Model)

# Check AIC value of your Stepwise model
AIC(Step.Reg.Model)
