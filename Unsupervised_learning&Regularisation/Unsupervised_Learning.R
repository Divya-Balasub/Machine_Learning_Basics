#Unsupervised Learning and Regularization

#Install necessary packages for Problem 6.5

#install.packages("ISLR")
#install.packages("glmnet")
#install.packages("boot")

#Necessary libraries
library(ISLR)
library(boot)
library(glmnet)

#5,1 Load the Data
load("prostate.Rdata")

#5.2 Fit ridge regression models on training set

#Set training data x and y
train_x<- as.matrix(prostate.train)[ ,1:8]
train_y<- prostate.train$lpsa

#Set testing data x and y
test_x <- as.matrix(prostate.test)[,1:8]
test_y <- prostate.test$lpsa

lambdas <- 10^seq(3, -3, by = -.1)
ridge_model <- glmnet(train_x, train_y, alpha=0,lambda = lambdas)
#summary(ridge_model)

#Plot coefficients vs log_lambda
#plot(ridge_model)
par(mfcol = c(2,2))
plot(ridge_model,xvar="lambda",label=TRUE,main="Ridge Regression")

#5.3 10-fold cross validation and best lambda
set.seed (1)
cv.ridge <- cv.glmnet(train_x,train_y, alpha = 0,lambda = lambdas)
#plot(cv.out)
bestlambda <- cv.ridge$lambda.min
bestlambda

#Find MSE for training data
ridge.pred <- predict(cv.ridge,lambda = bestlambda,newx = train_x)
mean((ridge.pred - train_y)^2)

#Find MSE for test data
ridge.pred <- predict(cv.ridge,lambda = bestlambda,newx = test_x)
mean((ridge.pred - test_y)^2)

#5.4 Fit lasso regression model on training set
lasso.mod <- glmnet(train_x, train_y, alpha = 1, lambda = lambdas)
plot(lasso.mod,xvar="lambda",label=TRUE, main = "Lasso")

#5.5 Perform 10-fold cross validation for lasso regression
set.seed (1)
cv.lasso <- cv.glmnet(train_x,train_y, alpha = 1,lambda = lambdas)
#plot(cv.lasso)
bestlambda <- cv.lasso$lambda.min
bestlambda

#Find MSE for training data
lasso.pred <- predict(cv.lasso,lambda = bestlambda,newx = train_x)
mean((lasso.pred - train_y)^2)

#Find MSE for test data
lasso.pred <- predict(cv.lasso,lambda = bestlambda,newx = test_x)
mean((lasso.pred - test_y)^2)
