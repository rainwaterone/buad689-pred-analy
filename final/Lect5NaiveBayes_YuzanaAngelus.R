#Lecture 5---Classification Methods 
#Naive Bayes Classifier

install.packages("naivebayes")
library(naivebayes)
library(magrittr) # for pipe %>%
library(colorspace)
library(tidyverse)
library(ggplot2)
library(e1071)
library(caretEnsemble)
library(psych)
library(Amelia)
library(Rcpp)
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
library(rsample)  # data splitting 
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret

# Import your data file 
hrdata <- read.csv("IBMHRAttrition.csv")

## Data exploration ----
View(hrdata)
head(hrdata)
str(hrdata)
summary(hrdata)

## Data Visualization ---------------------------------------------
ggplot(hrdata, aes(YearsSinceLastPromotion, colour = Quit)) +
  geom_freqpoly(binwidth = 1) + labs(title="Number of years since promotion")


ggplot(hrdata, aes(Age, colour = Quit)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution")


ggplot(hrdata, aes(TotalWorkingYears, colour = Quit)) +
  geom_freqpoly(binwidth = 1) + labs(title="Tenure in years")

ggplot(hrdata, aes(HourlyRate, colour = Quit)) +
  geom_freqpoly(binwidth = 1) + labs(title="HourlyRate")

## Correlation Plot with numeric/integer varaibles
## use Quit data file then filter the variable "Quit" when its equal "Yes",
# then select numeric variables in the Quit data set to produce a Correlation plot using method "square" 
hrdata %>%
  filter(Quit == "Yes") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot(method="square", tl.cex=0.8)

# convert some of the ordinal variables to factor variables using "mutate" function
hrdata <- hrdata %>%
  mutate(
    JobLevel = factor(JobLevel),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
  )

# Now check your data again on the above 3 variables: JobLevel, StockOptionLevel, TrainingTimesLastYear
# they are now factor variables with unique levels from 0,1,2,... 
summary(hrdata)
str(hrdata)


# check in to orginal data, "Quit" variable[Yes: No] response probability distribution

table(hrdata$Quit) %>% prop.table()
##        No       Yes 
## 0.8387755 0.1612245


# Traing and test data---------------



## stratified sampling using rsample package, variable starta is "Quit" to calculate probability distributions 
## 70% of data point in training data "train", 30% in testing data "test"
# Use set.seed for reproducibility,
set.seed(123)
split <- initial_split(hrdata, prop = .7, strata = "Quit")
train <- training(split)
test  <- testing(split)

# Check that 70% (1030)total data points are in "train" data set
summary(train)

# Check that 30% (440)total data points are in "test" data set
summary(test)



# check in to "train" data, "Quit" variable[Yes: No] response probability distribution 
# is same as our orginal data "hrdata"
table(train$Quit) %>% prop.table()

## return results from console 
##       No      Yes 
## 0.838835 0.161165


# check in to "test" data, "Quit" variable[Yes: No] response probability distribution 
# is same as our orginal data "hrdata"

table(test$Quit) %>% prop.table()


## return results from console 
##        No       Yes 
## 0.8386364 0.1613636


#identify target and features (the predictor variables)
##set the target/response and feature names. 
##The target is what we aim to predict (in our case "Quit"). 
##The features (all predictors,x1,x2,..xp in your data set) are what we will use, to model the prediction.
# this is to create response and feature data


# setdiff() returns a list of the names in train that are not 'Quit'. 
# In other words, the names of all the explanatory features
features <- setdiff(names(train), "Quit") 
x <- train[, features]
y <- train$Quit

# Set up 10-fold cross validation procedure. trainControl sets parameters for the train function
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# train your very second Naive Bayes Model
nb.m1 <- train(
  x = x, #The names of our feature columns
  y = y, #The name of our target
  method = "nb",
  trControl = train_control #The name of the list to which we assigned our trainControl parameters
)

# results
confusionMatrix(nb.m1)

## Cross-Validated (10 fold) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction   No  Yes
##        No  76.6  8.2
##        Yes  7.3  8.0
##                             
##  Accuracy (average) : 0.8456

# Set up tuning grid. expand.grid creates a dataframe from all combinations of the supplied vectors or values
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")#normalize with Box Cox, standardize with center-scaling, and reducing with PCA

)

# Produce top 5 models rank with Accuracy 
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))
##   usekernel fL adjust  Accuracy     Kappa AccuracySD   KappaSD
## 1      TRUE  0      2 0.8795512 0.4569701 0.02762358 0.1241459
## 2      TRUE  3      2 0.8776379 0.4319164 0.02902767 0.1329409
## 3      TRUE  1      2 0.8776284 0.4346569 0.02759330 0.1275316
## 4      TRUE  2      2 0.8766670 0.4251419 0.02860853 0.1324080
## 5      TRUE  4      2 0.8757148 0.4132786 0.02695358 0.1252651



# results for best model
confusionMatrix(nb.m2)
## Cross-Validated (10 fold) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##          Reference
## Prediction   No  Yes
##        No  81.4  9.5
##        Yes  2.5  6.6
##                             
##  Accuracy (average) : 0.8796


# prediction with best model nb.m2 using test dataset
nb.m2.pred <- predict(nb.m2, newdata = test)

class(nb.m2)
# convert test$Quit into factor
test <- test %>%
  mutate(Quit = factor(Quit))


confusionMatrix(nb.m2.pred,test$Quit)    

class(nb.m2)


