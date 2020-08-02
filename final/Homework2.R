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
  "astsa", "magrittr", "Hmisc"
)
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

#Display Regssion model statistics and anova table
display_regression_metrics <- function(model) {
  print(summary(model))
  print(Anova(model))
  print("VIFs")
  print(vif(model))
  print("AIC")
  print(AIC(model))
  print("BIC")
  print(BIC(model))
  #plot residuals
  par(mfrow = c(2, 2))
  plot(model)
}


#start Rattle
rattle()
#Start Rcmdr
library("Rcmdr")

Catalogs <-
  read.csv("C:/Users/coleman-c/Desktop/Summer 2020/Applied Analytics/R/Catalogs.csv")
Stocks <-
  read.csv(
    "C:/Users/coleman-c/Desktop/Summer 2020/Applied Analytics/R/GrpHW2.StockPrice.csv"
  )
TS <-
  read.csv("C:/Users/coleman-c/Desktop/Summer 2020/Applied Analytics/R/GrpHW2_ts.csv")
Catalogs <- mutate_if(Catalogs, is.character, as.factor)


#Question1

Catalogs <- mutate_if(Catalogs, is.character, as.factor)
Columns = list("Salary", "AmountSpent", "Children")
getColStats(columns = Columns, csvData = Catalogs)

display_regression_metrics(Catalogs)

model.AmtSpent <-
  lm(AmountSpent ~ Salary + Children, data = Catalogs)
display_regression_metrics(model.AmtSpent)

model.History <- lm(AmountSpent ~ History, data = Catalogs)
display_regression_metrics(model.History)

model.History_Salary <-
  lm(AmountSpent ~ History * Salary, data = Catalogs)
display_regression_metrics(model.History_Salary)

model.History_LOGSalary <-
  lm(log(AmountSpent) ~ History * log(Salary), data = Catalogs)
display_regression_metrics(model.History_LOGSalary)

print(2.718 ^ 0.55875)
print(2.718 ^ -0.21708)

#Question2

stocks <- na.omit(Stocks)

colnames(stocks)

model.StocksAll <- lm(Stock.Price ~ ., stocks)
display_regression_metrics(model.StocksAll)

Step.Stocks <-
  stepAIC(model.StocksAll, direction = "both", trace = FALSE)
display_regression_metrics(Step.Stocks)

model.StocksStep <-
  lm(
    log1p(Stock.Price) ~
      Total.Debt +
      #Cash +
      #ROE +
      Net.Margin +
      #log(Invested.Capital.1) +
      log(BV.of.Assets) +
      Net.Income +
      #EBITDA +
      Cash.as...of.Firm.Value +
      Fixed.Assets.Total.Assets +
      Market.Debt.to.Capital +
      Book.Debt.to.Capital,
    stocks
  )

display_regression_metrics(model.StocksStep)

PredictioData = data.frame(
  Stock.Price = 4.55,
  Total.Debt = 9.5,
  Cash = 0.3,
  Revenues..Last.yr = 61,
  Invested.Capital = 1.05,
  Reinvestment.Rate = 0.67,
  ROE = 0.08,
  ROC = 0.12,
  Net.Margin = 0.01,
  Invested.Capital.1 = 15.1,
  BV.of.Assets = 45.9,
  Net.Income = 0.5,
  EBIT = 4.41,
  EBITDA = 7.31,
  FCFF = 0.59,
  Cash.as...of.Firm.Value = 0.02,
  Cash.as...of.Revenues = 0,
  Cash.as...of.Total.Assets = 0.01,
  Capital.Expenditures = 2.2,
  Depreciation = 2.9,
  Trailing.Revenues = 76.7,
  Trailing.Net.Income = 0,
  Intangible.Assets.Total.Assets = 0,
  Fixed.Assets.Total.Assets = 0.38,
  Market.D.E = 1.44,
  Market.Debt.to.Capital = 0.59,
  Book.Debt.to.Capital = 0.62
)

pred1 <-
  predict(model.StocksStep, PredictioData, interval = "prediction")
pred1


#Question3

data.ts <- ts (TS, start = c(1970, 1), frequency = 12)

reg = lm(data.ts ~ time(data.ts))
summary(reg)

autoplot(data.ts)
#yes there is a trend

ggAcf(data.ts, lag.max = 36)
ggPacf(data.ts)

autoplot(decompose(data.ts))

diff.data.ts <- diff(data.ts)

diff12.data.ts <- diff(diff.data.ts, lag= 12)

ggAcf(diff12.data.ts, lag.max = 36)
ggPacf(diff12.data.ts)


autoplot((diff.data.ts))

autoplot(decompose(diff.data.ts))

ggAcf(diff.data.ts, lag.max = 36)
ggPacf(diff.data.ts)

Sfit_arima <-
  auto.arima(
    data.ts,
    seasonal = TRUE,
    stepwise = FALSE,
    approximation = FALSE,
    trace = TRUE
  )

summary(Sfit_arima)
checkresiduals(Sfit_arima)

Sfit_arima.fit1 <- Arima(
  data.ts,
  order = c(3, 1, 2),
  seasonal = list(order = c(1, 0, 1), period = 12),
  include.drift = TRUE
)

summary(Sfit_arima.fit1)
checkresiduals(Sfit_arima.fit1)

Sfit_arima.fit2 <- Arima(
  data.ts,
  order = c(3, 1, 2),
  seasonal = list(order = c(0, 0, 1), period = 12),
  include.drift = TRUE
)

summary(Sfit_arima.fit2)
checkresiduals(Sfit_arima.fit2)


fcst <- forecast(Sfit_arima.fit2, h = 12)
autoplot(fcst, include = 60)
print(fcst)
