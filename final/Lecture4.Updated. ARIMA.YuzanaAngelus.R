# Lecture 4 ARIMA modeling 

#Load libraries and install two new packages for today class

install.packages("astsa",dependencies = TRUE)
install.packages("fpp2",dependencies = TRUE)
install.packages("magrittr",dependencies = TRUE)
install.packages("dplyr",dependencies = TRUE)
library(magrittr) # for pipe %>%
library(dplyr)
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


# Import the data file that has quarterly percentage changes in US consumption expenditure data 
# and name it as "us.perchange"
us.perchange <- read.csv("/Users/aangelus/Documents/R.data/US.perchange.csv")

# create time series data with quarterly data starts from 1970 name the file "uschange.ts"
uschange.ts <- ts (us.perchange, start=c(1970,1), frequency=4)

# check your time series data and its format
print(uschange.ts)

# Plot your time series data 
autoplot(uschange.ts) + xlab("Year") + ylab("Quarterly percentage change")
plot(uschange.ts)

# draw a regression line using time over ts data
abline(reg = lm(uschange.ts~time(uschange.ts)))


#Time series decomposition involves thinking of a series as a combination of level, 
#...trend, seasonality,and noise components.

#Level: The average value in the series.
#Trend: The increasing or decreasing value in the series.
#Seasonality: The repeating short-term cycle in the series.
#Noise: The random variation in the series.

plot(decompose(uschange.ts))
autoplot(decompose(uschange.ts))

# AR I MA(p,d,q)
# p, d, q

# run linear regression to check the p-value significance to see if there is a trend, 
# p-value on slope "Time.COef" is siginificant or not and also the coef sign (+/-)
uschange.Reg <- lm(uschange.ts~time(uschange.ts))
summary(uschange.Reg )
# notice that p-value on slope "Time.COef" is siginificant with the value 0.0310 * 

# build your first auto ARIMA model to get an estimate on p,d,q values
uschange.fit <- auto.arima(uschange.ts, seasonal=FALSE)
summary(uschange.fit)



# ACF and PACF help you determine which arima model to use for forecast
ggAcf(uschange.ts) # order 3 is auto-regressive, and ACF dies out gradually. So its AR signature at lag 3 
ggPacf(uschange.ts) # none of the significant lag are negative, so your MA'q' term is zero

# Run your second ARIMA model using (p,d,q) = (3,0,0)
uschange.fit2 <- (Arima(uschange.ts, order=c(3,0,0)))
summary(uschange.fit2)
checkresiduals(uschange.fit2)

# Plot your forecast with 95% and 80% CI
autoplot(forecast(uschange.fit2))

#There is a lot of uncertainty in the data, typically AR model reverts to the mean,
# so after a few periods, the forecast becomes the mean


# Tune your Auto.arima function to work harder
uschange.fit3 <- (auto.arima(uschange.ts, seasonal=FALSE,
                             stepwise=FALSE, approximation=FALSE))
summary(uschange.fit3)
checkresiduals(uschange.fit3)

#This model produces slightly better than the first fit model identified by auto.arima() 
#(with an AICc value of 340.67 compared to 342.08). The auto.arima() function 
#did not find this model because it does not consider all possible models in its search. 
#You can make it work harder by using the arguments stepwise=FALSE and approximation=FALSE

#Pipe (%>%) Operator will forward a value, or the result of an expression, into the next function call/expression.
# Perform your forecast for the next 8 qtr using your Auto ARIMA model, you need to load library(magrittr)to use pipe operator
uschange.fit %>% forecast(h=8) %>% autoplot(include=80)
print(uschange.fit)

#-------------------------------------------------------------------------------------------

# Seasonal ARIMA models



# Import the data file that has quarterly European retail trade with 64 data points
# and name it as "euretail"
euretail <- read.csv("/Users/aangelus/Documents/R.data/EU.Retail.csv")



# create "euretail" time series data with quarterly data starts from 1996 
# name the file MQ.ts
euretail.ts <- ts (euretail, start=c(1996,1), frequency=4)

# check your data
print(euretail.ts )

# Plot your time series data 
autoplot(euretail.ts) + xlab("Year") + ylab("Retail Index")
plot(euretail.ts)
abline(reg = lm(euretail.ts~time(euretail.ts)))


#Decomposed time series to check on trend, seasonality,and noise components

#Level: The average value in the series.
#Trend: The increasing or decreasing value in the series.
#Seasonality: The repeating short-term cycle in the series.
#Noise: The random variation in the series.

plot(decompose(euretail.ts))
autoplot(decompose(euretail.ts))



# run linear regression to check the p-value, significance to see if there is a trend
# p-value on slope "Time.COef" is siginificant or not
euretail.Reg <- lm(euretail.ts ~time(euretail.ts))
summary(euretail.Reg )
# p-value on slope "Time.COef" is siginificant with pvalue 1.44e-12 ***

# build AUTO ARIMA model to get estimate on p,d,q values, adding Seasonality
euretail.fit <-(auto.arima(euretail.ts, seasonal=TRUE,
                           stepwise=FALSE, approximation=FALSE))
summary(euretail.fit)

# ACF and PACF help you determine which arima model to use for forecast
ggAcf(euretail.ts,lag.max = 36) 
ggPacf(euretail.ts) 



# Take the difference to determine ACF (AR,p) , d(difference,d) and PACF(MA,q)
F1.euretail.ts <- ts (euretail.ts, frequency=1) # Frequency is set to 1, to demo the correct count on lags
acf(diff(F1.euretail.ts))
pacf(diff(F1.euretail.ts)) 
plot(diff(F1.euretail.ts))

# Take the first difference at Lag=4 for to capture Seasonally differenced European retail trade index
euretail.ts %>% diff(lag=4) %>% ggtsdisplay()


# Differenced difference at Lag=4 for to capture the trend Double differenced
euretail.ts %>% diff(lag=4) %>% diff() %>% ggtsdisplay()


euretail.ts %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()
#Residuals from the fitted ARIMA(0,1,1)(0,1,1)4 model 


euretail.fit3 <- Arima(euretail.ts, order=c(0,1,3), 
                       seasonal = list(order =c(0,1,1),period =4))
summary(euretail.fit3)
checkresiduals(euretail.fit3)
#Residuals from the fitted ARIMA(0,1,3)(0,1,1)4 model 

euretail.fit2 <- Arima(euretail.ts, order=c(0,1,2), 
                       seasonal = list(order =c(0,1,1),period =4))
summary(euretail.fit2)
checkresiduals(euretail.fit2)
#Residuals from the fitted ARIMA(0,1,2)(0,1,1)4 model 

#All the spikes are now within the significance limits, so the residuals appear 
#to be white noise. The Ljung-Box test also shows that the residuals have 
#no remaining autocorrelations.

# Forecast for next 12 Qtr using Arima model
euretail.fit3 %>% forecast(h=12) %>% autoplot()

# forecast the next 12 Qtr using predict and forecast function
predict(euretail.fit3,n.ahead=12)
euretail.forecast12 <- forecast(euretail.fit3, h = 12)

# to get Point Forecast    Lo 80    Hi 80    Lo 95     Hi 95 values
print(euretail.forecast12)
