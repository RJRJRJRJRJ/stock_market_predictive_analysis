set.seed(2018)
library(randomForest)
# loading training and testing dataset
training1 <- read.csv("training1.csv")
training2 <- read.csv("training2.csv")
training3 <- read.csv("training3.csv")

testing1 <- read.csv("testing1.csv")
testing2 <- read.csv("testing2.csv")
testing3 <- read.csv("testing3.csv")

# function for RMSE
rmse <- function(error){
  sqrt(mean(error^2))
}

####  4 year and 1 year seperation data #########
rf_Model1 = randomForest(ftse ~ ., data=training1, ntree = 500,
                         importance=TRUE)
varImpPlot(rf_Model1)
# generate prediction by training
predict1 <- predict(rf_Model1, testing1)
# calculate error
residual1 <- testing1$ftse - predict1
rmse1 <- rmse(residual1)
# plot residuals
qqnorm(residual1,
       ylab="Residuals",
       xlab="Normal Scores",
       main="4-year/1-year split RF residuals",
       col="black",bg="green",pch=21)
qqline(residual1, col="red")
# generate prediction by testing
plot(testing1$ftse,predict1,
     xlab="Observations", ylab="Predictions",
     main="4-year/1-year split RF predictions",
     col="black",bg="green",pch=21)
abline(0,1,col="red")

#### 80/20 random
rf_Model2 = randomForest(ftse ~ ., data=training2, ntree = 500,
                         importance=TRUE)
varImpPlot(rf_Model2)
# generate prediction by training
predict2 <- predict(rf_Model2, testing2)
# calculate error
residual2 <- testing2$ftse - predict2
rmse2 <- rmse(residual2)
# plot residuals
qqnorm(residual2,
       ylab="Residuals",
       xlab="Normal Scores",
       main="80/20 random split RF residuals",
       col="black",bg="green",pch=21)
qqline(residual2, col="red")
# generate prediction by testing
plot(testing2$ftse,predict2,
     xlab="Observations", ylab="Predictions",
     main="80/20 random split RF predictions",
     col="black",bg="green",pch=21)
abline(0,1,col="red")


## 10-fold cv
rf_Model3 = randomForest(ftse ~ ., data=training3, ntree = 500,
                         importance=TRUE)
varImpPlot(rf_Model3)
# generate prediction by training
predict3 <- predict(rf_Model3, testing3)# calculate error
residual3 <- testing3$ftse - predict3
rmse3 <- rmse(residual3)
# plot residuals
qqnorm(residual3,
       ylab="Residuals",
       xlab="Normal Scores",
       main="10-fold cv split SVR residuals",
       col="black",bg="green",pch=21)
qqline(residual3, col="red")
# generate prediction by testing
plot(testing3$ftse,predict3,
     xlab="Observations", ylab="Predictions",
     main="10-fold cv split SVR predicitons",
     col="black",bg="green",pch=21)
abline(0,1,col="red")
