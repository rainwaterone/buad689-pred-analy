library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)
library(knitr)
library(RGtk2)
library(rattle)



amesHousing <- 
  read.table("C:/Users/rainwater-e/OneDrive - Texas A&M University/Summer-2020/buad689-pred-analy/hw-r01/Grp.Assgn1.data.csv",
             header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# Mutate character variables to factors
amesHousingFactored <- mutate_if(amesHousing, is.character, as.factor)

# This functon calculates the modes
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

Modes(amesHousingFactored[["Lot_Area"]])
Modes(amesHousingFactored[["Sales_Price"]])
Modes(amesHousingFactored[["Second_Flr_SF"]])

# Displays tables of mean & max values of Lot_Area, grouped by Zoning factor
with(amesHousingFactored, tapply(Lot_Area, list(Zoning=Zoning), mean))
with(amesHousingFactored, tapply(Lot_Area, list(Zoning=Zoning), max))


scatterplotMatrix(~Bsmt_SF+First_Flr_SF+Lot_Area+Sales_Price+Second_Flr_SF, 
                  regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), 
                  data=amesHousingFactored)

# Generate the pairs plot
amesHousingFactored %>%
  GGally::ggpairs(columns=c(4,11),
                  mapping=ggplot2::aes(colour=Zoning, alpha=0.5),
                  diag=list(continuous="density",
                            discrete="bar"),
                  upper=list(continuous="cor",
                             combo="box",
                             discrete="ratio"),
                  lower=list(continuous="points",
                             combo="denstrip",
                             discrete="facetbar")) +
  ggplot2::theme(panel.grid.major=ggplot2::element_blank())

# Perform linear regression
amesHousingLinReg <- 
  lm(formula = Sales_Price ~ Bedrooms + Bsmt_SF + Fireplaces + 
       First_Flr_SF + Full_Bath + Garage_Cars + Half_Bath + Kitchen + 
       Lot_Area + Second_Flr_SF + TotalRooms + Alley + Central_Air + Foundation +
       Lot_Config + Overall_Qual + Zoning, data = amesHousingFactored)
summary(amesHousingLinReg)



amesHousingFactored$log_Lot_Area <- with(amesHousingFactored, log10(Lot_Area))
amesHousingFactored$log_Sales_Price <- with(amesHousingFactored, log10(Sales_Price))
amesHousingFactored$log_First_Flr_SF <- with(amesHousingFactored, log10(First_Flr_SF))

scatterplotMatrix(~log_First_Flr_SF+log_Sales_Price+First_Flr_SF+Sales_Price, 
                  regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), 
                  data=amesHousingFactored)

amesHousingLinRegLogLog <- 
  lm(formula = log_Sales_Price ~ Bedrooms + Bsmt_SF + Fireplaces + 
       log_First_Flr_SF + Full_Bath + Garage_Cars + Half_Bath + Kitchen + 
       log_Lot_Area + Second_Flr_SF + Central_Air +
       Overall_Qual, data = amesHousingFactored)
summary(amesHousingLinRegLogLog)


newdata <- data.frame("log_Lot_Area" = log10(20000), "log_First_Flr_SF" = log10(4500), "Garage_Cars" = 3, 
                      "Bedrooms" = 4, "Full_Bath" = 2, "Half_Bath" = 1, "Fireplaces" = 1, 
                      "Bsmt_SF" = 0, "Kitchen" = 1, "Second_Flr_SF" = 0, "Central_Air" = "Y",
                      "Overall_Qual" = "Excellent")

yhat <- predict(amesHousingLinRegLogLog, newdata=newdata)

predSalesPrice = 10^yhat

stdError <- summary(amesHousingLinRegLogLog)$sigma
upperConfValue <- 10^(yhat + 2*stdError)
lowerConfValue <- 10^(yhat - 2*stdError)

#Clay's last-minute model:
model3 <-
  lm(
    log_Sales_Price ~ Zoning + log_Lot_Area + Lot_Config + Overall_Qual + Bsmt_SF + Central_Air +
      log_First_Flr_SF + Second_Flr_SF + Full_Bath + Half_Bath + Bedrooms + Kitchen +
      Fireplaces + Garage_Cars,
    data = amesHousingFactored
  )
summary(model3)

newdata <- data.frame("log_Lot_Area" = log10(20000), "log_First_Flr_SF" = log10(4500), "Garage_Cars" = 3, 
                      "Bedrooms" = 4, "Full_Bath" = 2, "Half_Bath" = 1, "Fireplaces" = 1, 
                      "Bsmt_SF" = 0, "Kitchen" = 1, "Second_Flr_SF" = 0, "Central_Air" = "Y",
                      "Overall_Qual" = "Excellent", "Zoning" = "Medium_Density", "Lot_Config" = "CulDSac")


#Predict from Clay's last-minute model
yhat <- predict(model3, newdata=newdata)

predSalesPrice = 10^yhat

stdError <- summary(model3)$sigma
upperConfValue <- 10^(yhat + 2*stdError)
lowerConfValue <- 10^(yhat - 2*stdError)

cat("Predicted Sales Price: ", predSalesPrice)
cat("Standard Error: ", stdError)
cat("95% Confidence Range: ", upperConfValue, lowerConfValue)
