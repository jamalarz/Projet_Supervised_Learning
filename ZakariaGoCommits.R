### UPDATES : LOGISTIC RESGRESSION, CART RANDOM FORREST ..

library(FactoMineR) #PCA
library(MASS) #QDA & LNR
library(rpart) #CART
library(e1071) #SVM & NB
library(class) #KNN
library(randomForest) #RF
library(stats)# LGR

library(caret)

####

library(ROSE)  # Library that solves imbalanced data issues.

## Libraries to display Tree Model 

# library(rattle) 
# library(RColorBrewer) 


#### RE TAKE AFTER DATA EXPLORATION IN JUPYTER NOTEBOOK. #### 

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

##### CART RANDOM FORREST #####

trainIndex <- createDataPartition(data$Class,p=0.7,list = FALSE,times = 1)

train.data <- data_balanced[trainIndex, ]

test.data  <- data_balanced[-trainIndex,]


dim(train.data)

dim(test.data)

table(train.data$Class) 

table(test.data$Class)


m1 <- rpart(formula = Class~.,
            data = cart.train[,-c(1,11)],
            method = "class",
            control = r.ctrl
)


printcp(m1) 


##  fancyRpartPlot(m1)   ; ## Display tree model


plotcp(m1) 
