install.packages("rpart")
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
set.seed(2018)

# loading training and testing dataset
training1 <- read.csv("training1.csv")
training2 <- read.csv("training2.csv")
training3 <- read.csv("training3.csv")

testing1 <- read.csv("testing1.csv")
testing2 <- read.csv("testing2.csv")
testing3 <- read.csv("testing3.csv")

fit1 <- rpart(training1$ftse ~ ., method="anova", data=training1 )

plot(fit1, uniform=TRUE, 
     main="Regression Tree")
text(fit1, use.n=TRUE, cex = .2)

printcp(fit1)
fancyRpartPlot(fit1)

Prediction1 <- predict(fit1, testing1, type = "vector")

# function for RMSE
rmse <- function(error){
  sqrt(mean(error^2))
}

# calculate error
residual1 <- testing1$ftse - Prediction1
rmse1 <- rmse(residual1)

rmse1

# plot residuals
qqnorm(residual1,
       ylab="Residuals",
       xlab="Normal Scores",
       main="4-year/1-year split GBM residuals",
       col="black",bg="green",pch=21)
qqline(residual1, col="red")

# generate prediction by testing
plot(testing1$ftse,Prediction1,
     xlab="Observations", ylab="Predictions",
     main="4-year/1-year split GBM predictions",
     col="black",bg="green",pch=21)
abline(0,1,col="red")

rmse1

print(names(fit1$variable.importance))

############################################# MODLE 2 ################################################

fit2 <- rpart(training2$ftse ~ ., method="anova", data=training2 )

plot(fit2, uniform=TRUE, 
     main="Regression Tree")
text(fit2, use.n=TRUE, cex = .2)

printcp(fit2)
fancyRpartPlot(fit2)

Prediction2 <- predict(fit2, testing2, type = "vector")

# function for RMSE
rmse <- function(error){
  sqrt(mean(error^2))
}

# calculate error
residual2 <- testing2$ftse - Prediction2
rmse2 <- rmse(residual2)

rmse2

# plot residuals
qqnorm(residual2,
       ylab="Residuals",
       xlab="Normal Scores",
       main="80/20 random split GBM residuals",
       col="black",bg="green",pch=21)
qqline(residual2, col="red")

# generate prediction by testing
plot(testing2$ftse,Prediction2,
     xlab="Observations", ylab="Predictions",
     main="80/20 random split GBM residuals",
     col="black",bg="green",pch=21)
abline(0,1,col="red")

rmse2
print(names(fit2$variable.importance))

############################################# MODLE 3 ################################################
fit3 <- rpart(training3$ftse ~ ., method="anova", data=training3 )

plot(fit3, uniform=TRUE, 
     main="Regression Tree")
text(fit3, use.n=TRUE, cex = .2)

printcp(fit3)
fancyRpartPlot(fit3)

Prediction3 <- predict(fit3, testing3, type = "vector")

# function for RMSE
rmse <- function(error){
  sqrt(mean(error^2))
}

# calculate error
residual3 <- testing3$ftse - Prediction3
rmse3 <- rmse(residual3)

rmse3

# plot residuals
qqnorm(residual3,
       ylab="Residuals",
       xlab="Normal Scores",
       main="10-fold cv split GBM residuals",
       col="black",bg="green",pch=21)
qqline(residual3, col="red")

# generate prediction by testing
plot(testing3$ftse,Prediction3,
     xlab="Observations", ylab="Predictions",
     main="10-fold cv split GBM residuals",
     col="black",bg="green",pch=21)
abline(0,1,col="red")

rmse3
print(names(fit3$variable.importance))
