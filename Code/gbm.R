# gradient boosting regression
# install.packages("gbm", dependencies = T)
library(gbm)
library(caret)
set.seed(2018)

# loading training and testing dataset
training1 <- read.csv(" training1.csv")
training2 <- read.csv(" training2.csv")
training3 <- read.csv(" training3.csv")

testing1 <- read.csv(" testing1.csv")
testing2 <- read.csv(" testing2.csv")
testing3 <- read.csv(" testing3.csv")

# function for RMSE
rmse <- function(error){
  sqrt(mean(error^2))
}

# train the model
control <- trainControl(method = "repeatedcv",number=10,repeats=10)
gbm.train1 <- train(ftse~., data=training1, method='gbm',
                    trControl=control)
gbm.train1
gbm.ftse1<-gbm(ftse~.,data=training1,n.trees = 150,interaction.depth = 3,
               shrinkage = 0.1,distribution = 'gaussian')
summary(gbm.ftse1)
# generate prediction by training
predict1 <- predict(gbm.ftse1, newdata = testing1, n.trees = 150)
# calculate error
residual1 <- testing1$ftse - predict1
rmse1 <- rmse(residual1)
# plot residuals
qqnorm(residual1,
       ylab="Residuals",
       xlab="Normal Scores",
       main="4-year/1-year split GBM residuals",
       col="black",bg="green",pch=21)
qqline(residual1, col="red")
# generate prediction by testing
plot(testing1$ftse,predict1,
     xlab="Observations", ylab="Predictions",
     main="4-year/1-year split GBM predictions",
     col="black",bg="green",pch=21)
abline(0,1,col="red")

# model building
gbm.train2 <- train(ftse~., data=training2, method='gbm',
                    trControl=control)
gbm.train2
gbm.ftse2<-gbm(ftse~.,data=training2,n.trees = 150,interaction.depth = 3,
               shrinkage = 0.1,distribution = 'gaussian')
summary(gbm.ftse2)
# generate prediction
predict2 <- predict(gbm.ftse2, newdata = testing2, n.trees = 150)
# calculate error
residual2 <- testing2$ftse - predict2
rmse2 <- rmse(residual2)
# plot residuals
qqnorm(residual2,
       ylab="Residuals",
       xlab="Normal Scores",
       main="80/20 random split GBM residuals",
       col="black",bg="green",pch=21)
qqline(residual2, col="red")
# generate prediction by testing
plot(testing2$ftse,predict2,
     xlab="Observations", ylab="Predictions",
     main="80/20 random split GBM predictions",
     col="black",bg="green",pch=21)
abline(0,1,col="red")

# model building
gbm.train3 <- train(ftse~., data=training3, method='gbm',
                    trControl=control)
gbm.train3
gbm.ftse3<-gbm(ftse~.,data=training3,n.trees = 150,interaction.depth = 3,
               shrinkage = 0.1,distribution = 'gaussian')
summary(gbm.ftse3)
# generate prediction
predict3 <- predict(gbm.ftse3, newdata = testing3, n.trees = 150)
# calculate error
residual3 <- testing3$ftse - predict3
rmse3 <- rmse(residual3)
# plot residuals
qqnorm(residual3,
       ylab="Residuals",
       xlab="Normal Scores",
       main="10-fold cv split GBM residuals",
       col="black",bg="green",pch=21)
qqline(residual3, col="red")
# generate prediction by testing
plot(testing3$ftse,predict3,
     xlab="Observations", ylab="Predictions",
     main="10-fold cv split GBM predicitons",
     col="black",bg="green",pch=21)
abline(0,1,col="red")