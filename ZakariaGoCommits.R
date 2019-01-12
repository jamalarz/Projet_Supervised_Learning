### UPDATES : LOGISTIC RESGRESSION, .. 

library(FactoMineR) #PCA
library(MASS) #QDA & LNR
library(rpart) #CART
library(e1071) #SVM & NB
library(class) #KNN
library(randomForest) #RF
library(stats)# LGR

####

library(ROSE)  # Library that solves imbalanced data issues.



data[1:10,]

summary(data)

summary(data$Class)

### At this point we see a problem in classes.

#### Over-coming the Problem of Imbalanced Data

data_balanced = ovun.sample(Class~.,data = data, method = "under",
                           p = 0.5,seed = 1)$data


summary(data_balanced$Class)


str(data_balanced)




### Train and test Splitting

## Sampling data 0.75 Training, 0.25 Test

Sampeled_data=sample(nrow(data_balanced),round(nrow(data_balanced)*0.75),replace = FALSE)

Data_train=data_balanced[Sampeled_data,];

Data_test=data_balanced[-Sampeled_data,];

## Fitting LR Model

fit.lm=glm(Class~., data = Data_train, family = binomial)

summary(fit.lm)

fit.pred=predict(fit.lm,newdata = Data_test[,-31],type = "response")

fit.pred=ifelse(fit.pred>0.5,1,0)

lm.cf=length(which(fit.pred==Data_test$Class))

lm.cf=lm.cf/nrow(Data_test)

lm.cf
