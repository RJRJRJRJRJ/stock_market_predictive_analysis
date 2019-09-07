Daily.Indicator <- read.csv("NEW_DAILY_INDICATORS.csv")
Daily.Indicator.1 <- Daily.Indicator[,c(1,4,5)]
Daily.Indicator.1$NEW.DATE <- as.Date(Daily.Indicator.1$NEW.DATE,format = "%m/%d/%Y")
Daily.Indicator.2 <- Daily.Indicator[,c(6,9,10,11,12)]
Daily.Indicator.2$NEW.DATE.1 <- as.Date(Daily.Indicator.2$NEW.DATE.1,format = "%m/%d/%Y")
Daily.Indicator.3 <- Daily.Indicator[,c(13,16)]
Daily.Indicator.3$NEW.DATE.2 <- as.Date(Daily.Indicator.3$NEW.DATE.2,format = "%m/%d/%Y")


library(xlsx)
Shanghai <- read.xlsx("Daily Chinese Indices.xlsx",1)
Shanghai.major <- data.frame(Shanghai$Date,Shanghai$SSE,Shanghai$SSECBI)
Shanghai.major$Shanghai.Date <- as.Date(Shanghai.major$Shanghai.Date,format="%m/%d/%Y")

Shenzhen <- read.xlsx("Daily Chinese Indices.xlsx",2)
Shenzhen.major <- data.frame(Shenzhen$Date,Shenzhen$SZSEC)
Shenzhen.major$Shenzhen.Date <- as.Date(Shenzhen.major$Shenzhen.Date,format="%m/%d/%Y")

Both <- read.xlsx("Daily Chinese Indices.xlsx",3)
Both.major <- data.frame(Both$Date,Both$CSI300)
Both.major$Both.Date <- as.Date(Both.major$Both.Date,format="%m/%d/%Y")

FTSE <- read.csv("FTSE China 50 USD Historical Data.csv", sep="")
FTSE <- FTSE[,1:2]
FTSE <- FTSE[-n,]

# Data Combination
Data.Combined <- FTSE
Data.Combined$Price <- as.numeric(as.character(Data.Combined$Price))
Data.Combined$Date <- as.Date(Data.Combined$Date,format = "%m/%d/%Y")
n <- length(Data.Combined[,1])

# Add ETF volatility
Data.Combined$ETF.Volatility <- Daily.Indicator.1$ETF_VOLATILITY[match(Data.Combined$Date,Daily.Indicator.1$NEW.DATE)]
ETF.check <- Daily.Indicator.1$ETF_VOLATILITY[match(Data.Combined$Date-1,Daily.Indicator.1$NEW.DATE)]
ETF.check.2 <- Daily.Indicator.1$ETF_VOLATILITY[match(Data.Combined$Date-2,Daily.Indicator.1$NEW.DATE)]
ETF.check.3 <- Daily.Indicator.1$ETF_VOLATILITY[match(Data.Combined$Date-3,Daily.Indicator.1$NEW.DATE)]


for (i in 1:n){
  if(is.na(Data.Combined$ETF.Volatility[i])){
    Data.Combined$ETF.Volatility[i] = ETF.check[i]
  }
}

for (i in 1:n){
  if(is.na(Data.Combined$ETF.Volatility[i])){
    Data.Combined$ETF.Volatility[i] = ETF.check.2[i]
  }
}

for (i in 1:n){
  if(is.na(Data.Combined$ETF.Volatility[i])){
    Data.Combined$ETF.Volatility[i] = ETF.check.3[i]
  }
}

# Add Exchange Rate

Data.Combined$Exchange.Rate <- Daily.Indicator.1$DAILY_EXCH[match(Data.Combined$Date,Daily.Indicator.1$NEW.DATE)]
Exchange.check <- Daily.Indicator.1$DAILY_EXCH[match(Data.Combined$Date-1,Daily.Indicator.1$NEW.DATE)]
Exchange.check.2 <- Daily.Indicator.1$DAILY_EXCH[match(Data.Combined$Date-2,Daily.Indicator.1$NEW.DATE)]
Exchange.check.3 <- Daily.Indicator.1$DAILY_EXCH[match(Data.Combined$Date-3,Daily.Indicator.1$NEW.DATE)]


for (i in 1:n){
  if(is.na(Data.Combined$Exchange.Rate[i])){
    Data.Combined$Exchange.Rate[i] = Exchange.check[i]
  }
}

for (i in 1:n){
  if(is.na(Data.Combined$Exchange.Rate[i])){
    Data.Combined$Exchange.Rate[i] = Exchange.check.2[i]
  }
}

for (i in 1:n){
  if(is.na(Data.Combined$Exchange.Rate[i])){
    Data.Combined$Exchange.Rate[i] = Exchange.check.3[i]
  }
}

# Add Bond YTM

Data.Combined$Bond.YTM <- Daily.Indicator.3$CHINA_BOND_YTM[match(Data.Combined$Date,Daily.Indicator.3$NEW.DATE.2)]
YTM.check <- Daily.Indicator.3$CHINA_BOND_YTM[match(Data.Combined$Date-1,Daily.Indicator.3$NEW.DATE.2)]

for (i in 1:n){
  if(is.na(Data.Combined$Bond.YTM[i])){
    Data.Combined$Bond.YTM[i] = YTM.check[i]
  }
}

# Add SSE
Data.Combined$SSE <- Shanghai.major$Shanghai.SSE[match(Data.Combined$Date,Shanghai.major$Shanghai.Date)]
Data.Combined$SSECBI <- Shanghai.major$Shanghai.SSECBI[match(Data.Combined$Date,Shanghai.major$Shanghai.Date)]

Data.Combined <- na.omit(Data.Combined)

str(Data.Combined)
summary(Data.Combined)

library(lubridate)
Data.Combined$Week.number <- as.factor(as.character(lubridate::isoweek(ymd(Data.Combined$Date))))

# test data
n <- length(Data.Combined[,1])
Data.Combined.Test <- Data.Combined[1:(0.2*n),]
Data.Combined.Training <- Data.Combined[(0.2*n+1):n,]


# simple regression
fit.1 <- lm(Price~.-Date , data=Data.Combined.Training)
summary(fit.1)

predict.1 <- predict(fit.1,Data.Combined.Test[,c(1,3:8)])
plot(predict.1,Data.Combined.Test$Price)

# random forest
# install.packages("randomForest")
library(randomForest)
fit.2 <- randomForest(Price~.-Date , data=Data.Combined.Training)

summary(fit.2)

predict.2 <- predict(fit.2,Data.Combined.Test[,c(1,3:8)])
plot(Data.Combined.Test$Date,Data.Combined.Test$Price,type="l",col ="black")
lines(Data.Combined.Test$Date,predict.2,col="light blue")
lines(Data.Combined.Test$Date,predict.1,col="blue")
