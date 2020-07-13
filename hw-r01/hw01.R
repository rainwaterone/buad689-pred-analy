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


Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

Modes(amesHousing[["Lot_Area"]])
Modes(amesHousing[["Sales_Price"]])
Modes(amesHousing[["Second_Flr_SF"]])


with(amesHousing, tapply(Lot_Area, list(Zoning=Zoning), mean))
with(amesHousing, tapply(Lot_Area, list(Zoning=Zoning), max))


scatterplotMatrix(~Bsmt_SF+First_Flr_SF+Lot_Area+Sales_Price+Second_Flr_SF, 
                  regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), 
                  data=amesHousing)


amesHousing <- 
  lm(Sales_Price~Bedrooms+Bsmt_SF+Fireplaces+First_Flr_SF+Full_Bath+Garage_Cars+Half_Bath+Kitchen+Lot_Area+Second_Flr_SF+TotalRooms,
     data=amesHousing)
summary(amesHousing)



amesHousing$log_Lot_Area <- with(amesHousing, log10(Lot_Area))
amesHousing$log_Sales_Price <- with(amesHousing, log10(Sales_Price))
mesHousing$log_First_Flr_SF <- with(amesHousing, log10(First_Flr_SF))

scatterplotMatrix(~log_First_Flr_SF+log_Sales_Price+First_Flr_SF+Sales_Price, 
                  regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), 
                  data=amesHousing)

amesHousingLinRegLogLog <- 
  lm(log_Sales_Price~Bedrooms+Bsmt_SF+Fireplaces+log_First_Flr_SF+Full_Bath+Garage_Cars+Half_Bath+Kitchen+Lot_Area+Second_Flr_SF+TotalRooms,
     data=amesHousing)
summary(amesHousingLinRegLogLog)


newdata <- data.frame("Lot_Area" = 20000, "log_First_Flr_SF" = log10(4500), "Garage_Cars" = 3, 
                      "Bedrooms" = 4, "Full_Bath" = 2, "Half_Bath" = 1, "Fireplaces" = 1, 
                      "Bsmt_SF" = 0, "Kitchen" = 1, "Second_Flr_SF" = 0, "TotalRooms" = 6.45)

predict(amesHousingLinRegLogLog, newdata=newdata)
