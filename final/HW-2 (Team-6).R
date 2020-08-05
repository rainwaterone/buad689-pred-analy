#### Libraries ----
library(tidyverse)
library(ggplot2)
library(colorspace)
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(car)
library(rpart)
library(randomForest)
library(RGtk2)
library(lme4)
library(magrittr) # for pipe %>%
library(dplyr)
library(colorspace)
library(corrplot)
library(readr)
library(tseries)
library(forecast)
library(fpp2)
library(fma)
library(expsmooth)
library(astsa)
library(stats)
library(MARSS)
library(datasets)
library(tseries)
library(rattle)
#start Rattle
rattle()
#Start Rcmdr
library(Rcmdr)

#### Interaction Terms and Indicator Variables ----

# Loading Catalogs.csv file and reviewing data
Catalogs <- read.csv("Catalogs.csv", header = TRUE)
str(Catalogs)
summary(Catalogs)

### Question #1 ----

## Regressing AmountSpent with Salary and Children
model.1 <- lm(AmountSpent ~ Salary + Children, data=Catalogs)
summary(model.1)
AIC(model.1)
# Analyzing Residuals
hist(model.1$res)
qqnorm(model.1$res)
qqline(model.1$res, col = "red", lwd = 3)

plot(Catalogs$AmountSpent, model.1$residuals, col = "blue")
abline(0, 0)

plot(predict(model.1), residuals(model.1))
abline(0, 0, col = "Red")

# Analyzing VIF Values
vif_values <- vif(model.1)
vif_values
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 1.0, lwd = 3, lty = 2)


### Question #2 ----

## Log Transforming Salary and AmountSpent
Catalogs$Log_Salary <- with(Catalogs, log(Salary))
Catalogs$Log_AmountSpent <- with(Catalogs, log(AmountSpent))
str(Catalogs)

#scatter plot of Log_AmountSpent vs. Log_Salary
scatterplot(Log_AmountSpent~Log_Salary | Location, regLine=TRUE, 
            smooth=FALSE, boxplots=FALSE, by.groups=TRUE,
            legend=list(coords="topleft"), data=Catalogs)
model.10 <- lm(Log_AmountSpent ~ Log_Salary*Location, data=Catalogs)
summary(model.10)


model.11 <- lmList(Log_AmountSpent ~ Log_Salary | Location, 
                   data=Catalogs)
summary(model.11)

### Question #3 ----

## Regressing AmountSpent with History
model.2 <- lm(AmountSpent ~ History, data=Catalogs)
summary(model.2)

# Analyzing Residuals
hist(model.2$res)
qqnorm(model.2$res)
qqline(model.2$res, col = "red", lwd = 3)

plot(Catalogs$AmountSpent, model.2$residuals, col = "blue")
abline(0, 0)

plot(predict(model.2), residuals(model.2))
abline(0, 0, col = "Red")


## Regressing Log_AmountSpent with History
model.3 <- lm(Log_AmountSpent ~ History, data=Catalogs)
summary(model.3)

# Analyzing Residuals
hist(model.3$res)
qqnorm(model.3$res)
qqline(model.3$res, col = "red", lwd = 3)

plot(Catalogs$Log_AmountSpent, model.3$residuals, col = "blue")
abline(0, 0)

plot(predict(model.3), residuals(model.3))
abline(0, 0, col = "Red")


## Regressing AmountSpent with Salary, History and Interaction Term

model.4 <- lm(AmountSpent ~ Salary*History, data=Catalogs)
summary(model.4)

# Analyzing Residuals
hist(model.4$res)
qqnorm(model.4$res)
qqline(model.4$res, col = "red", lwd = 3)

plot(Catalogs$AmountSpent, model.4$residuals, col = "blue")
abline(0, 0)

plot(predict(model.4), residuals(model.4))
abline(0, 0, col = "Red")

# Analyzing VIF Values
vif_values <- vif(model.4)
vif_values
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 10.0, lwd = 3, lty = 2)


## Regressing Log_AmountSpent with Log_Salary, History and Interaction Term

model.5 <- lm(Log_AmountSpent ~ Log_Salary*History, data=Catalogs)
summary(model.5)

# Analyzing Residuals
hist(model.5$res)
qqnorm(model.5$res)
qqline(model.5$res, col = "red", lwd = 3)

plot(Catalogs$Log_AmountSpent, model.5$residuals, col = "blue")
abline(0, 0)

plot(predict(model.5), residuals(model.5))
abline(0, 0, col = "Red")

# Analyzing VIF Values
vif_values <- vif(model.5)
vif_values
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 10.0, lwd = 3, lty = 2)


#### Handling Multicollinearity ----

# Loading GrpHW2.StockPrice.csv file and reviewing data
StockPrice <- read.csv("GrpHW2.StockPrice.csv", header = TRUE)
str(StockPrice)
summary(StockPrice)

# Removing observations with NA
Clean_StockPrice <- na.omit(StockPrice)
str(Clean_StockPrice)
summary(Clean_StockPrice)


### Question #1 ----

# Correlation Matrix
cor(Clean_StockPrice)

### Question #2 ----

## Regression for Stock.Price with all the Predictors
model.6 <- lm(Stock.Price ~., data = Clean_StockPrice)
summary(model.6)
AIC(model.6)

# Analyzing Residuals
hist(model.6$res)
qqnorm(model.6$res)
qqline(model.6$res, col = "red", lwd = 3)

plot(Clean_StockPrice$Stock.Price, model.6$residuals, col = "blue")
abline(0, 0)

plot(predict(model.6), residuals(model.6))
abline(0, 0, col = "Red")

# Analyzing VIF Values
vif_values <- vif(model.6)
vif_values
par(mar=c(6,15,4,4), las=2)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue", axis.lty=1)
abline(v = 10.0, lwd = 3, lty = 2)

### Question #3 ----

## Running stepwise model to remove unnecessary variables
model.7 <- step(model.6, direction = "both", trace = FALSE)
summary(model.7)
AIC(model.7)

# Analyzing Residuals
hist(model.7$res)
qqnorm(model.7$res)
qqline(model.7$res, col = "red", lwd = 3)

plot(Clean_StockPrice$Stock.Price, model.7$residuals, col = "blue")
abline(0, 0)

plot(predict(model.7), residuals(model.7))
abline(0, 0, col = "Red")

# Analyzing VIF Values
vif_values <- vif(model.7)
vif_values
par(mar=c(6,15,4,4), las=2)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue", axis.lty=1)
abline(v = 10.0, lwd = 3, lty = 2)

# Analyzing relationships through scatterplot matrix
scatterplotMatrix(~Book.Debt.to.Capital+BV.of.Assets+Cash+
                      Cash.as...of.Firm.Value+EBITDA+Fixed.Assets.Total.Assets+
                      Invested.Capital.1+Market.Debt.to.Capital+Net.Income+
                      Net.Margin+ROE+Stock.Price+Total.Debt,
                  regLine=TRUE, smooth=FALSE, diagonal=list(method="histogram"), 
                  data=Clean_StockPrice)

# Log Transforming some variables  
Clean_StockPrice$Log_Stock.Price <- with(Clean_StockPrice, log(Stock.Price+1))
Clean_StockPrice$Log_Total.Debt <- with(Clean_StockPrice, log(Total.Debt+1))
Clean_StockPrice$Log_Cash <- with(Clean_StockPrice, log(Cash+1))
Clean_StockPrice$Log_Invested.Capital.1 <- with(Clean_StockPrice, log(Invested.Capital.1))
Clean_StockPrice$Log_BV.of.Assets <- with(Clean_StockPrice, log(BV.of.Assets))

## Running regression model with transformed variables
model.8 <- lm(Log_Stock.Price ~ Log_Total.Debt + Log_Cash + ROE + 
                        Net.Margin + Log_Invested.Capital.1 + Log_BV.of.Assets + Net.Income + 
                        EBITDA + Cash.as...of.Firm.Value + Fixed.Assets.Total.Assets + 
                        Market.Debt.to.Capital + Book.Debt.to.Capital, data=Clean_StockPrice)
summary(model.8)
AIC(model.8)

# Analyzing Residuals
hist(model.8$res)
qqnorm(model.8$res)
qqline(model.8$res, col = "red", lwd = 3)

plot(Clean_StockPrice$Log_Stock.Price, model.8$residuals, col = "blue")
abline(0, 0)

plot(predict(model.8), residuals(model.8))
abline(0, 0, col = "Red")

# Analyzing VIF Values
vif_values <- vif(model.8)
vif_values
par(mar=c(6,15,4,4), las=2)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue", axis.lty=1)
abline(v = 10.0, lwd = 3, lty = 2)

## Regression model with non-significant variables removed

model.9 <- lm(Log_Stock.Price ~ Log_Total.Debt + ROE + Net.Margin 
                +Log_BV.of.Assets + Net.Income + EBITDA + Cash.as...of.Firm.Value
                +Fixed.Assets.Total.Assets + Market.Debt.to.Capital + Book.Debt.to.Capital, 
                data=Clean_StockPrice)
summary(model.9)
AIC(model.9)

# Analyzing Residuals
hist(model.9$res)
qqnorm(model.9$res)
qqline(model.9$res, col = "red", lwd = 3)

plot(Clean_StockPrice$Log_Stock.Price, model.9$residuals, col = "blue")
abline(0, 0)

plot(predict(model.9), residuals(model.9))
abline(0, 0, col = "Red")

# Analyzing VIF Values
vif_values <- vif(model.9)
vif_values
par(mar=c(6,15,4,4), las=2)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue", axis.lty=1)
abline(v = 5.0, lwd = 3, lty = 2)

### Question #4 ----

Eval_Vector = c(1, log(9.5 + 1), 0.08, 0.01, log(45.9), 0.5, 7.31, 0.02, 0.38, 0.59, 0.62)

Coef_Vector = summary(model.9)$coefficient[1:11]


Transformed_Point_Prediction = crossprod(Coef_Vector, Eval_Vector)
Transformed_Se = 0.6548 #from model.9

Transformed_Upper_Pred_Interval = Transformed_Point_Prediction + 2 * Transformed_Se
Transformed_Lower_Pred_Interval = Transformed_Point_Prediction - 2 * Transformed_Se

# Actual values
Point_Prediction = exp(Transformed_Point_Prediction) - 1
Upper_Pred_Interval = exp(Transformed_Upper_Pred_Interval) - 1
Lower_Pred_Interval = exp(Transformed_Lower_Pred_Interval) - 1

# Printing Output
Point_Prediction
Upper_Pred_Interval
Lower_Pred_Interval


#### Time Series Forecasting ----

#### Loading GrpHW2_ts.csv file and exploring data
GrpHW2 <- read.csv("GrpHW2_ts.csv")

# creating time series data with monthly frequency
# data starts from first month of year 2000
# naming the file "GrpHW2.ts"
GrpHW2.ts <- ts (GrpHW2, start=c(2000,1), frequency=12)

# Printing time series data
GrpHW2.ts

# Plotting time series data 
autoplot(GrpHW2.ts) + xlab("Year") + ylab("Monthly change")

# Plotting time series data with regression line this time
plot(GrpHW2.ts)
abline(reg = lm(GrpHW2.ts~time(GrpHW2.ts)))

# Decomposing Time Series for Seasonality and Trend
plot(decompose(GrpHW2.ts))
autoplot(decompose(GrpHW2.ts))

# ACF and PACF help you determine which arima model to use for forecast
ggAcf(GrpHW2.ts,lag.max = 48) 
ggPacf(GrpHW2.ts,lag.max = 48) 
# ACF plot clearly shows trend & non-stationarity

# Checking for Stationarity with Augmented Dickey-Fuller (adf) test
adf.test(GrpHW2.ts, alternative = "stationary")

# Augmented Dickey-Fuller Test
# data:  GrpHW2.ts
# Dickey-Fuller = -2.5108, Lag order = 6, p-value = 0.3611
# alternative hypothesis: stationary
# Since p-Value is greater than 0.05, time series is not Stationary

# running linear regression to check the p-value, to see if there is a trend
# p-value on slope "Time.COef" is siginificant or not
GrpHW2.ts.Reg <- lm(GrpHW2.ts ~time(GrpHW2.ts))
summary(GrpHW2.ts.Reg)
# p-value on slope "Time.Coef" is significant with p-value 2e-16
# This implies that there is a significant trend in time series data

### Question #1 ----

# Creating new time series with the first difference
Diff1GrpHW2<-diff(GrpHW2.ts, differences=1)

# Printing a nd plotting differenced time series
Diff1GrpHW2
autoplot(Diff1GrpHW2) + xlab("Year") + ylab("First order difference")

# Plotting time series data with regression line this time
plot(Diff1GrpHW2)
abline(reg = lm(Diff1GrpHW2~time(Diff1GrpHW2)))

# Checking Stationarity for differenced times series data
adf.test(Diff1GrpHW2, alternative = "stationary")
# Augmented Dickey-Fuller Test
# 
# data:  Diff1GrpHW2
# Dickey-Fuller = -6.2599, Lag order = 6, p-value = 0.01
# alternative hypothesis: stationary
# Since p-value = 0.01 < 0.05, series is stationary with a difference of 1 
# d value for AIRMA model is 1

# Generating ACF and PACF plots
acf(as.numeric(Diff1GrpHW2, lag.max=48))
pacf(as.numeric(Diff1GrpHW2, lag.max=48))
ggAcf(Diff1GrpHW2,lag.max = 48) 
ggPacf(Diff1GrpHW2,lag.max = 48)

# Decomposing Time Series for Seasonality and Trend
plot(decompose(Diff1GrpHW2))
autoplot(decompose(Diff1GrpHW2))

# running linear regression to check if there is a trend
Diff1GrpHW2.Reg <- lm(Diff1GrpHW2 ~ time(Diff1GrpHW2))
summary(Diff1GrpHW2.Reg)
# p-value on slope "Time.Coef" is non-significant with p-value .274
# This implies that there is no significant trend in time series data

### Question #2 ----

# Fitting auto ARIMA model on original time-series to get an estimate on (p,d,q) & (P,D,Q) values
GrpHW2.fit <- auto.arima(GrpHW2.ts, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)
summary(GrpHW2.fit)
checkresiduals(GrpHW2.fit)

### Question #3 ----

# ARIMA model (2,1,2)(0,0,1)
GrpHW2.fit1 <- Arima(GrpHW2.ts, order = c(2,1,2),
                     seasonal =list(order = c(0,0,1), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit1)
checkresiduals(GrpHW2.fit1)
ggAcf(residuals(GrpHW2.fit1), lag = 48)
ggPacf(residuals(GrpHW2.fit1), lag = 48)

# ARIMA model (3,1,2)(0,0,1)
GrpHW2.fit2 <- Arima(GrpHW2.ts, order = c(3,1,2),
                     seasonal =list(order = c(0,0,1), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit2)
checkresiduals(GrpHW2.fit2)
ggAcf(residuals(GrpHW2.fit2), lag = 48)
ggPacf(residuals(GrpHW2.fit2), lag = 48)

# ARIMA model (3,1,3)(0,0,1)
GrpHW2.fit3 <- Arima(GrpHW2.ts, order = c(3,1,3),
                     seasonal =list(order = c(0,0,1), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit3)
checkresiduals(GrpHW2.fit3)
ggAcf(residuals(GrpHW2.fit3), lag = 48)
ggPacf(residuals(GrpHW2.fit3), lag = 48)

# ARIMA model (2,1,1)(0,0,1)
GrpHW2.fit4 <- Arima(GrpHW2.ts, order = c(2,1,1),
                     seasonal =list(order = c(0,0,1), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit4)
checkresiduals(GrpHW2.fit4)
ggAcf(residuals(GrpHW2.fit4), lag = 48)
ggPacf(residuals(GrpHW2.fit4), lag = 48)

# ARIMA model (1,1,2)(0,0,1)
GrpHW2.fit5 <- Arima(GrpHW2.ts, order = c(1,1,2),
                     seasonal =list(order = c(0,0,1), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit5)
checkresiduals(GrpHW2.fit5)
ggAcf(residuals(GrpHW2.fit5), lag = 48)
ggPacf(residuals(GrpHW2.fit5), lag = 48)

# ARIMA model (1,1,1)(0,0,1)
GrpHW2.fit6 <- Arima(GrpHW2.ts, order = c(1,1,1),
                     seasonal =list(order = c(0,0,1), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit6)
checkresiduals(GrpHW2.fit6)
ggAcf(residuals(GrpHW2.fit6), lag = 48)
ggPacf(residuals(GrpHW2.fit6), lag = 48)

# ARIMA model (2,1,2)(1,0,1)
GrpHW2.fit7 <- Arima(GrpHW2.ts, order = c(2,1,2),
                     seasonal =list(order = c(1,0,1), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit7)
checkresiduals(GrpHW2.fit7)
ggAcf(residuals(GrpHW2.fit7), lag = 48)
ggPacf(residuals(GrpHW2.fit7), lag = 48)

# ARIMA model (2,1,2)(1,1,1)
GrpHW2.fit8 <- Arima(GrpHW2.ts, order = c(2,1,2),
                     seasonal =list(order = c(1,1,1), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit8)
checkresiduals(GrpHW2.fit8)
ggAcf(residuals(GrpHW2.fit8), lag = 48)
ggPacf(residuals(GrpHW2.fit8), lag = 48)

# ARIMA model (3,1,3)(0,0,0)
GrpHW2.fit9 <- Arima(GrpHW2.ts, order = c(3,1,3),
                     seasonal =list(order = c(0,0,0), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit9)
checkresiduals(GrpHW2.fit9)
ggAcf(residuals(GrpHW2.fit9), lag = 48)
ggPacf(residuals(GrpHW2.fit9), lag = 48)

# ARIMA model (3,1,3)(1,0,1)
GrpHW2.fit10 <- Arima(GrpHW2.ts, order = c(3,1,3),
                     seasonal =list(order = c(1,0,1), period = 12),
                     include.drift = TRUE)
summary(GrpHW2.fit10)
checkresiduals(GrpHW2.fit10)
ggAcf(residuals(GrpHW2.fit10), lag = 48)
ggPacf(residuals(GrpHW2.fit10), lag = 48)

# ARIMA model (3,1,3)(1,1,1)
GrpHW2.fit11 <- Arima(GrpHW2.ts, order = c(3,1,3),
                      seasonal =list(order = c(1,1,1), period = 12),
                      include.drift = TRUE)
summary(GrpHW2.fit11)
checkresiduals(GrpHW2.fit11)
ggAcf(residuals(GrpHW2.fit11), lag = 48)
ggPacf(residuals(GrpHW2.fit11), lag = 48)

# ARIMA model (3,1,3)(0,0,2)
GrpHW2.fit12 <- Arima(GrpHW2.ts, order = c(3,1,3),
                      seasonal =list(order = c(0,0,2), period = 12),
                      include.drift = TRUE)
summary(GrpHW2.fit12)
checkresiduals(GrpHW2.fit12)
ggAcf(residuals(GrpHW2.fit12), lag = 48)
ggPacf(residuals(GrpHW2.fit12), lag = 48)

# ARIMA model (3,1,3)(1,0,2)
GrpHW2.fit13 <- Arima(GrpHW2.ts, order = c(3,1,3),
                      seasonal =list(order = c(1,0,2), period = 12),
                      include.drift = TRUE)
summary(GrpHW2.fit13)
checkresiduals(GrpHW2.fit13)
ggAcf(residuals(GrpHW2.fit13), lag = 48)
ggPacf(residuals(GrpHW2.fit13), lag = 48)

# ARIMA model (3,1,3)(2,0,1)
GrpHW2.fit14 <- Arima(GrpHW2.ts, order = c(3,1,3),
                      seasonal =list(order = c(2,0,1), period = 12),
                      include.drift = TRUE)
summary(GrpHW2.fit14)
checkresiduals(GrpHW2.fit14)
ggAcf(residuals(GrpHW2.fit14), lag = 48)
ggPacf(residuals(GrpHW2.fit14), lag = 48)

# ARIMA model (3,1,3)(2,1,1)
GrpHW2.fit15 <- Arima(GrpHW2.ts, order = c(3,1,3),
                      seasonal =list(order = c(2,1,1), period = 12),
                      include.drift = TRUE)
summary(GrpHW2.fit15)
checkresiduals(GrpHW2.fit15)
ggAcf(residuals(GrpHW2.fit15), lag = 48)
ggPacf(residuals(GrpHW2.fit15), lag = 48)

### Question #4 ----
# Forecast for next 12 Months using Arima model (3,1,3)(0,0,1)
GrpHW2.fit2 %>% forecast(h=12) %>% autoplot(include=80)
GrpHW2.fit2 %>% forecast(h=12) %>% print()


# forecast the next 12 Months using predict and forecast function
predict(GrpHW2.fit2, n.ahead=12)
GrpHW2.fit %>% predict()
