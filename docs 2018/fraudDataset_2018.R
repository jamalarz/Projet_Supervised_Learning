library(caret)
library(class)
library(e1071)
library(ROCR)
library(tidyverse)

setwd("D:/M2_MLDS/Apprentissage Supervisé/Projet_App-Sup_MLDS18")

#load data sets
credit <- read.csv("creditcard.csv")

str(credit)
View(credit)

credit$Class <- factor(credit$Class)

credit %>%
  ggplot(aes(x = Class)) +
  geom_bar(color = "grey", fill = "lightgrey") +
  theme_bw()


y <- credit$Class
index <- createDataPartition(credit$Class, p = 0.75, list = F) 
train <- credit[index, ]
test <- credit[-index, ]
y_train <- y[index] 
y_test <- y[-index]

# K-Nearest Neighbors

knn1 <- knn(train = train[,-31], test = test[,-31], cl = train$Class, k = 5)
confusionMatrix(knn1, test$Class, positive = "1")


# Naive Bayes

bayes <- naiveBayes(Class~., data = train, laplace = 1)
bayes$apriori

pred <- predict(bayes, test)
confusionMatrix(pred, test$Class, positive = "1")

#*******************************************************************
#random forest
library(randomForest)
# Model

fit <-randomForest(train ~ ., data = train,ntree=300,importance=T)
# Prediction
p <- predict(fit ,test) 
confusionMatrix(p, test$Class, positive = "1")


#*************************************************************

fit <-svm(class ~ ., data = train)  
summary(fit)
predicted= predict(fit,test)
confusionMatrix(predicted, test$Class, positive = "1")

#**************************************************
library(kernlab)
fit <-ksvm(credit$Class ~ ., data = credit)  
summary(fit)
predicted= predict(fit,test)
confusionMatrix(predicted, test$Class, positive = "1")
