#Assignment2

#Rifat Rahman Turjo, 7024636
#Divya Balasubramani, 7009404

#Install necessary packages for Problem 6.6

install.packages("ISLR")
install.packages("glmnet")
install.packages("boot")


library(MASS)
library(dplyr)
library(ISLR)
library(boot)
library(glmnet)

#Question1

#Read the phoneme file
phoneme <- read.csv("phoneme.csv")

#Splitting into test and train
for(i in 1: nrow(phoneme))
{
  phoneme$speaker[i] = strsplit(phoneme$speaker[i],split ='\\.')[[1]][1]
}

train_phoneme = phoneme %>% filter(speaker == "train")
test_phoneme = phoneme %>% filter(speaker == "test")


#Removing the columns
train_phoneme = subset(train_phoneme, select = -c(row.names,speaker))
test_phoneme = subset(test_phoneme,select = -c(row.names,speaker) )


#===================================================================================#
#Question2

lda.model<- lda(g~.,train_phoneme)

#Predictions for test data
lda_prediction_test <- predict(lda.model,test_phoneme)
str(lda_prediction_test)

prediction_table_test <- table(lda_prediction_test$class, test_phoneme$g)
prediction_table_test

#Test error
lda_test_error = mean(lda_prediction_test$class != test_phoneme$g)
lda_test_error

#Training error
lda_prediction_train <- predict(lda.model,train_phoneme)
str(lda_prediction_test)

prediction_table_train <- table(lda_prediction_train$class, train_phoneme$g)
prediction_table_train

lda_train_error = mean(lda_prediction_train$class != train_phoneme$g)
lda_train_error

#LDA Accuracy rate of the test data

lda_accuracy <- mean(lda_prediction_test$class == test_phoneme$g)
lda_accuracy

#Plots for lda.model got from training data
plot(lda.model, col=c("red", "blue", "green", "orange","black"))

par(mar=c(1,1,1,1))
ldahist(data = lda_prediction_train$x[,], g=train_phoneme$g)


#Plot for test data
ldahist(data = lda_prediction_test$x[,], g=test_phoneme$g)

#===================================================================================#
#Question3

#Filtering only "aa" and "ao"
train_filtered <- filter(train_phoneme,g=="aa"|g=="ao")
test_filtered <- filter(test_phoneme,g=="aa"|g=="ao")

#Trained lda model for filtered dataset
lda_model_filter  <- lda(g ~ .,train_filtered)
lda_model_filter

#Prediction test for filtered test data
lda_prediction_test_filter <- predict(lda_model_filter,test_filtered)
prediction_table_test_filter <- table(lda_prediction_test_filter$class, test_filtered$g)
prediction_table_test_filter

#test error
test_error_filter <- mean(lda_prediction_test_filter$class != test_filtered$g )
test_error_filter

#LDA Accuracy rate of the filtered test data
lda_accuracy_filter <- mean(lda_prediction_test_filter$class == test_filtered$g)
lda_accuracy_filter

#Prediction test for filtered training data
lda_prediction_train_filter <- predict(lda_model_filter,train_filtered)
prediction_table_train_filter <- table(lda_prediction_train_filter$class, train_filtered$g)
prediction_table_train_filter

#train error
train_error_filter <- mean(lda_prediction_train_filter$class != train_filtered$g )
train_error_filter

#Plots for lda_model_filter got from filtered training data
plot(lda_model_filter, col=c("red", "blue"))

ldahist(data = lda_prediction_train_filter$x[,], g=train_filtered$g)


#Plot for filtered test data
ldahist(data = lda_prediction_test_filter$x[,], g=test_filtered$g)

#===================================================================================#
#Question4

#QDA FOR THE ORIGINAL PHONEME DATA

qda.model<- qda(g~.,train_phoneme)
qda.model

#Predictions for test data
qda_prediction_test <- predict(qda.model,test_phoneme)
str(qda_prediction_test)

qda_prediction_table_test <- table(qda_prediction_test$class, test_phoneme$g)
qda_prediction_table_test

#Test error
qda_test_error = mean(qda_prediction_test$class != test_phoneme$g)
qda_test_error

#Training error
qda_prediction_train <- predict(qda.model,train_phoneme)
str(qda_prediction_test)

qda_prediction_table_train <- table(qda_prediction_train$class, train_phoneme$g)
qda_prediction_table_train

qda_train_error = mean(qda_prediction_train$class != train_phoneme$g)
qda_train_error

#qda Accuracy rate of the test data

qda_accuracy <- mean(qda_prediction_test$class == test_phoneme$g)
qda_accuracy

#-------------------------------------------------------------------
#QDA FOR "aa" and "ao" PHONEME DATA

#Trained qda model for filtered dataset
qda_model_filter  <- qda(g ~ .,train_filtered)
qda_model_filter

#Prediction test for filtered test data
qda_prediction_test_filter <- predict(qda_model_filter,test_filtered)
qda_prediction_table_test_filter <- table(qda_prediction_test_filter$class, test_filtered$g)
qda_prediction_table_test_filter

#test error
qda_test_error_filter <- mean(qda_prediction_test_filter$class != test_filtered$g )
qda_test_error_filter

#qda Accuracy rate of the filtered test data
qda_accuracy_filter <- mean(qda_prediction_test_filter$class == test_filtered$g)
qda_accuracy_filter

#Prediction test for filtered training data
qda_prediction_train_filter <- predict(qda_model_filter,train_filtered)
qda_prediction_table_train_filter <- table(qda_prediction_train_filter$class, train_filtered$g)
qda_prediction_table_train_filter

#train error
qda_train_error_filter <- mean(qda_prediction_train_filter$class != train_filtered$g )
qda_train_error_filter


#===================================================================================#
#Question5

#Confusion matrix for LDA 
conf_LDA_train <- table(lda_prediction_train_filter$class, train_filtered$g)
conf_LDA_train

conf_LDA_test <- table(lda_prediction_test_filter$class, test_filtered$g)
conf_LDA_test

#Confusion matrix for QDA 
conf_QDA_train <- table(qda_prediction_train_filter$class, train_filtered$g)
conf_QDA_train

conf_QDA_test <- table(qda_prediction_test_filter$class, test_filtered$g)
conf_QDA_test

#=====================================================================================
#Question_6

#Set Seed
set.seed(1)

#Perform LOOCV training set 

glm.fit <- glm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = prostate.train)
cv.err <- cv.glm(prostate.train, glm.fit)
cv.err$delta

#Perform LOOCV testing set 

glm.fit <- glm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = prostate.test)
cv.err <- cv.glm(prostate.test, glm.fit)
cv.err$delta

# Perform k-Fold Cross-Validation with k = 5 on training set
cv.error.5 <- rep(0, 5)
for (i in 1:10) {
  glm.fit <- glm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = prostate.train)
  cv.error.5[i] <- cv.glm(prostate.train , glm.fit , K = 5)$delta [1]
}
cv.error.5

# Perform k-Fold Cross-Validation with k = 5 on testing set
cv.error.5 <- rep(0, 5)
for (i in 1:10) {
  glm.fit <- glm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = prostate.test)
  cv.error.5[i] <- cv.glm(prostate.test , glm.fit , K = 5)$delta [1]
}
cv.error.5 

# Perform k-Fold Cross-Validation with k = 10 on training set
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = prostate.train)
  cv.error.10[i] <- cv.glm(prostate.train , glm.fit , K = 10)$delta [1]
}
cv.error.10

# Perform k-Fold Cross-Validation with k = 10 on testing set
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = prostate.test)
  cv.error.10[i] <- cv.glm(prostate.test , glm.fit , K = 10)$delta [1]
}
cv.error.10

#====================================================================================









