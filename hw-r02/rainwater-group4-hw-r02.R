
catalog <- 
  read.table("C:/Users/edwar/OneDrive - Texas A&M University/Summer-2020/buad689-pred-analy/hw-r02/Catalogs.csv",
   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

LinearModel.1 <- lm(AmountSpent ~ Children + Salary, 
  data=catalog)
summary(LinearModel.1)

catalog$logAmountSpent <- 
  with(catalog, log(AmountSpent))
catalog$logSalary <- 
  with(catalog, log(Salary))

scatterplot(logAmountSpent~logSalary | Location, regLine=TRUE, smooth=FALSE,
   boxplots=FALSE, by.groups=TRUE, data=catalog)

LinearModel.History <- lm(AmountSpent ~ History, data=catalog)
summary(LinearModel.History)


LinearModel.2 <- lm(AmountSpent ~ History +Salary +History*Salary, 
  data=catalog)
summary(LinearModel.2)

