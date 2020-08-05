
LinearRegression <- 
  read.table("C:/Users/rainwater-e/OneDrive - Texas A&M University/Summer-2020/buad689-pred-analy/final/LinearRegression.csv",
   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
LinearModel.Type.Price <- lm(Price ~ Type, data=LinearRegression)
summary(LinearModel.Type.Price)
LinearModel.MileageCylInt <- lm(Price ~ Mileage*Cylinder, data=LinearRegression)
summary(LinearModel.2)

LinearRegression$Log.price <- with(LinearRegression, log(Price))
LinearRegression$Log.mileage <- with(LinearRegression, log(Mileage))

scatterplot(Price~Mileage, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  data=LinearRegression)

scatterplot(Mileage~Log.price, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  data=LinearRegression)

scatterplot(Log.mileage~Price, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  data=LinearRegression)

scatterplot(Log.mileage~Log.price, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
  data=LinearRegression)

LinearModel.log.price <- glm(Log.price ~ Cylinder + Doors + Leather + Liter 
  + Mileage + Make + Model + Type, data=LinearRegression)
summary(LinearModel.log.price)

