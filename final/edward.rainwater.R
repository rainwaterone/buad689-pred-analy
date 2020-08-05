## ----GlobalOptions,echo=FALSE-------------------------------------------------
options(knitr.duplicate.label = 'allow')


## ----setup, include=FALSE-----------------------------------------------------


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



## -----------------------------------------------------------------------------
fname         <- "LinearRegression.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")



## ----echo=FALSE, message=FALSE------------------------------------------------
set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("Price", "Mileage", "Make", "Model", "Type",
                   "Cylinder", "Liter", "Doors")

crs$numeric   <- c("Price", "Mileage", "Cylinder", "Liter",
                   "Doors")

crs$categoric <- c("Make", "Model", "Type")

crs$target    <- "Leather"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL


## -----------------------------------------------------------------------------
p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Make=as.factor(Make)) %>%
  dplyr::select(Price, Make) %>%
  ggplot2::ggplot(ggplot2::aes(x=Price)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Make, colour=Make), alpha=0.55) +
  ggplot2::xlab("Price\n\nRattle 2020-Aug-04 18:23:57 rainwater-e") +
  ggplot2::ggtitle("Distribution of Price (sample)\nby Make") +
  ggplot2::labs(fill="Make", y="Density")

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Make=as.factor(Make)) %>%
  dplyr::select(Mileage, Make) %>%
  ggplot2::ggplot(ggplot2::aes(x=Mileage)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Make, colour=Make), alpha=0.55) +
  ggplot2::xlab("Mileage\n\nRattle 2020-Aug-04 18:23:58 rainwater-e") +
  ggplot2::ggtitle("Distribution of Mileage (sample)\nby Make") +
  ggplot2::labs(fill="Make", y="Density")

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Make=as.factor(Make)) %>%
  dplyr::select(Cylinder, Make) %>%
  ggplot2::ggplot(ggplot2::aes(x=Cylinder)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Make, colour=Make), alpha=0.55) +
  ggplot2::xlab("Cylinder\n\nRattle 2020-Aug-04 18:23:58 rainwater-e") +
  ggplot2::ggtitle("Distribution of Cylinder (sample)\nby Make") +
  ggplot2::labs(fill="Make", y="Density")

gridExtra::grid.arrange(p01, p02, p03)


## -----------------------------------------------------------------------------
LinearRegression <- 
  read.table("C:/Users/rainwater-e/OneDrive - Texas A&M University/Summer-2020/buad689-pred-analy/final/LinearRegression.csv",
             header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)



## -----------------------------------------------------------------------------
LinearModel.Type.Price <- lm(Price ~ Type, data=LinearRegression)
summary(LinearModel.Type.Price)


## -----------------------------------------------------------------------------
LinearModel.MileageCylInt <- lm(Price ~ Mileage*Cylinder, data=LinearRegression)
summary(LinearModel.MileageCylInt)


## -----------------------------------------------------------------------------
LinearRegression$Log.price <- with(LinearRegression, log(Price))
LinearRegression$Log.mileage <- with(LinearRegression, log(Mileage))


## -----------------------------------------------------------------------------

scatterplot(Price~Mileage, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  data=LinearRegression)

scatterplot(Mileage~Log.price, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  data=LinearRegression)

scatterplot(Log.mileage~Price, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  data=LinearRegression)

scatterplot(Log.mileage~Log.price, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  data=LinearRegression)




## -----------------------------------------------------------------------------
LinearModel.log.price <- lm(Log.price ~ Cylinder + Doors + Leather + Liter 
  + Mileage + Make + Model + Type, data=LinearRegression)
summary(LinearModel.log.price)


## -----------------------------------------------------------------------------
us.perchange <- read.csv("TimeSeries.csv")
uschange.ts <- ts (us.perchange, frequency=12)


## ---- echo=TRUE---------------------------------------------------------------
autoplot(decompose(uschange.ts))


## -----------------------------------------------------------------------------
uschange.Reg <- lm(uschange.ts~time(uschange.ts))
summary(uschange.Reg)


## -----------------------------------------------------------------------------
ggAcf(uschange.ts, lag.max = 58)
ggPacf(uschange.ts, lag.max = 58)


## -----------------------------------------------------------------------------
uschange.fit <- auto.arima(uschange.ts, seasonal=FALSE)
summary(uschange.fit)


## -----------------------------------------------------------------------------
checkresiduals(uschange.fit)


## -----------------------------------------------------------------------------
hrdata <- read.csv("NaiveBayes.csv")


## -----------------------------------------------------------------------------
head(hrdata)
str(hrdata)
summary(hrdata)


## -----------------------------------------------------------------------------
table(hrdata$status) %>% prop.table()


## -----------------------------------------------------------------------------
hrdata <- hrdata %>%
  mutate(
    hsc_s = factor(hsc_s),
    degree_t = factor(degree_t),
    )


## -----------------------------------------------------------------------------
set.seed(123)
split <- initial_split(hrdata, prop = .8, strata = "status")
train <- training(split)
test  <- testing(split)
table(train$status) %>% prop.table()


## -----------------------------------------------------------------------------
features <- setdiff(names(train), "status") 
x <- train[, features]
y <- train$status



## ---- message=FALSE-----------------------------------------------------------
train_control <- trainControl(
  method = "cv", 
  number = 10
)


## ---- message=FALSE, echo=TRUE, warning=FALSE---------------------------------
nb.m1 <- train(
  x = x, #The names of our feature columns
  y = y, #The name of our target
  method = "nb",
  trControl = train_control #The name of the list to which we assigned our trainControl parameters
)



## -----------------------------------------------------------------------------
confusionMatrix(nb.m1)


## ---- echo = FALSE, message=FALSE---------------------------------------------
knitr::purl('edward.rainwater.Rmd') # Generate a standard R script file

