set.seed(2018)
## Fit SVR model and visualize using scatter plot

# Install Package
# install.packages("e1071",dependencies = T)

#Load Library
library(e1071)
library(rminer)

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

# model
svm.model1 <- fit(ftse~., data=training1, model="svm", kpar=list(sigma=0.10), C=1:100)
#Find out the best model
Importance(svm.model1, data=training1)$value
write.csv(Importance(svm.model1, data=training1)$value,"imp1.csv")
# generate prediction by training
predict1 <- predict(svm.model1, testing1)
# calculate error
residual1 <- testing1$ftse - predict1
rmse1 <- rmse(residual1)
# plot residuals
qqnorm(residual1,
       ylab="Residuals",
       xlab="Normal Scores",
       main="4-year/1-year split SVR residuals",
       col="black",bg="green",pch=21)
qqline(residual1, col="red")
# generate prediction by testing
plot(testing1$ftse,predict1,
     xlab="Observations", ylab="Predictions",
     main="4-year/1-year split SVR predictions",
     col="black",bg="green",pch=21)
abline(0,1,col="red")

# model building
svm.model2 <- fit(ftse~., data=training2, model="svm", kpar=list(sigma=0.10), C=1:100)
#Find out the best model
Importance(svm.model2, data=training2)$value
write.csv(Importance(svm.model2, data=training2)$value,"imp2.csv")
# generate prediction by training
predict2 <- predict(svm.model2, testing2)
# calculate error
residual2 <- testing2$ftse - predict2
rmse2 <- rmse(residual2)
# plot residuals
qqnorm(residual2,
       ylab="Residuals",
       xlab="Normal Scores",
       main="80/20 random split SVR residuals",
       col="black",bg="green",pch=21)
qqline(residual2, col="red")
# generate prediction by testing
plot(testing2$ftse,predict2,
     xlab="Observations", ylab="Predictions",
     main="80/20 random split SVR predictions",
     col="black",bg="green",pch=21)
abline(0,1,col="red")

# model building
svm.model3 <- fit(ftse~., data=training3, model="svm", kpar=list(sigma=0.10), C=1:100)
#Find out the best model
Importance(svm.model3, data=training3)$value
write.csv(Importance(svm.model3, data=training3)$value,"imp3.csv")
# generate prediction by training
predict3 <- predict(svm.model3, testing3)# calculate error
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
