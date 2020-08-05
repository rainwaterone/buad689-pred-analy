#' ---
#' title: "BUAD 689 Final"
#' author: "E. Lee Rainwater"
#' date: "8/4/2020"
#' output: pdf_document
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

requiredPackages = c(
  "naivebayes", "magrittr", "colorspace",
  "tidyverse", "ggplot2",  "e1071",
  "caretEnsemble",  "psych", "Amelia",
  "Rcpp", "mice", "GGally",
  "rpart", "corrplot", "readr",
  "tseries", "forecast", "fpp2",
  "fma", "expsmooth", "astsa",
  "stats", "MARSS", "datasets",
  "rsample", "dplyr", "ggplot2",
  "caret", "naivebayes",
  "klaR", 'cairoDevice', 'car',
  'caret', 'EnvStats', 'randomForest',
  'rattle', 'RGtk2', 'rpart', 'tidytable',
  'tidyverse', 'MASS', 'ISwR',
  "astsa", "magrittr", "Hmisc",
  "tidyverse", "psych", "sarima"
)
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# workingDir = 'C:/Users/rainwater-e/OneDrive - Texas A&M University/Summer-2020/buad689-pred-analy/final/'
# setwd(workingDir)


#' 
#' 
#' 
#' Import the time series data file:
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
us.perchange <- 
  read.csv('US.perchange.csv')

#' 
#' ## Format your data into time series
#' Create time series data with quarterly data starting from January, 1970:
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
uschange.ts <- ts(us.perchange, start=c(1970,1), frequency = 4)
head(uschange.ts)

#' ## Time Series Decomposition
#' 
#' Time series decomposition involves considering the series as an additive combination of level, trend, seasonality, and noise components.
#' 
#' - Level: The mean of the series
#' - Trend: The increase or decrease in the local mean over time
#' - Seasonality: The repeating short-term cycle in the series
#' - Noise: The random variation in the series
#' 
## ---- echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------
autoplot(decompose(uschange.ts))

#' 
#' ## Slope of `Time.Coef`
#' 
#' - Run a linear regression and check the *p*-value significance to see if there is a trend
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
uschange.Reg <- lm(uschange.ts~time(uschange.ts))
summary(uschange.Reg)

#' 
#' From the above, we can see that the coefficient (slope) is near zero, and the test value is significant.
#' 
#' ## Auto ARIMA model
#' Here, we will build an auto ARIMA model to get an estimate of the *p, d,* and *q* values:
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
uschange.fit <- auto.arima(uschange.ts, seasonal=FALSE)
summary(uschange.fit)

#' 
#' The values of *p, d,* and *q* are returned as a tuple, denoted above as `ARIMA(1,0,3)`.
#' 
#' ## Determining Lags using ACF and PACF
#' 
#' - ACF - AutoCorrelation Function - determines the number of lags that are significant indicators of autocorrelation. This value, *p* is the order of autocorrelation.
#' - PACF - Partial AutoCorrelation function - determines the number correlation of observations that are exactly *k* time steps apart. Here, we are looking for the number of lags for which there are negative PACF values outside the 2-$\sigma$ range.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggAcf(uschange.ts)

#' 
#' It can be seen in the ACF plot above that lags 1, 2, and 3 are autogressive, therefore, the order is $p = 3$. Now, the PACF plot will be generated:
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggPacf(uschange.ts)

#' In the PACF plot above, we are looking for lags in which the PACF value is both significant and negative. Since there are none, the moving average term, $q = 0$.
#' 
#' Now, we will run an ARIMA model using $(p, d, q) = (3, 0, 0)$ as determined above:
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
uschange.fit2 <- (Arima(uschange.ts, order=c(3,0,0)))
summary(uschange.fit2)

#' We need to do some reading up to determine how to best interpret the above. 
#' 
#' ## Residuals of the ARIMA Model
#' 
#' Verifying the above model involves generating ACF and PACF plots of the residuals to verify that none of them have significant ACF or PACF values.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
checkresiduals(uschange.fit2)

#' The large *p*-value indicates that the autocorrelation of the residuals is not significant.
#' 
#' ## Use a model fit to forcast the next 8 quarters
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
uschange.fit %>% forecast(h=8) %>% autoplot(include=80)
print(uschange.fit)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::purl('rainwater-buad689-final.Rmd', documentation=2) # Generate a standard R script file

