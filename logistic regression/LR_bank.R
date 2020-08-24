library(mlbench)
library(Amelia)
library(e1071)
library(skimr)
library(corrplot)
library(data.table)
library(psych)
library(mvtnorm)
library(caret)
library(PRROC)
library(ggplot2)
library(caTools)
library(pROC)
library(dplyr)
library(DMwR)
library(ROSE)
data<-read.csv('C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Logistic regression\\bank-full.csv',sep = ';')
head(data)
View(data)
#EDA
str(data)
summary(data)
table(data$y)
prop.table(table(data$y))
skim(data)
dim(data)
pairs(data)
missmap(data, col=c("black", "grey"), legend=FALSE) # checking for missing data
boxplot(data)


#### balancing the data ####
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8,0.2))
train <- data[ind==1,]
test  <- data[ind==2,]

prop.table(table(train$y))
prop.table(table(test$y))

#simple model
model_simple <- glm(y~., data = train, family = binomial)
pred_simple <- predict(model_simple, newdata = test[-17])
roc.curve(test$y, pred_simple) #AUC=0.902

## balancing the data ##
attach(data)

#over sampling
data_balanced_over <- ovun.sample(y ~ ., data = train, method = "over",N = 64056)$data #N must be double of good points 367*2
table(data_balanced_over$y)

#under sampling
data_balanced_under <- ovun.sample(y ~ ., data = train, method = "under", N = 8496, seed = 1)$data #N must be double of negative points 113*2
table(data_balanced_under$y)

#mixed sampling
data_balanced_both <- ovun.sample(y ~ ., data = train, method = "both", p=0.5, N=36276, seed = 1)$data #N is 367+113
table(data_balanced_both$y)

#rose sampling 
data.rose <- ROSE(y ~ ., data = train, seed = 111)$data
table(data.rose$y)

#SMOTE sampling
#data_balanced_smote <- SMOTE(y ~ ., data = train, perc.over = 64056, perc.under=8496)
#table(data_balanced_smote$y)

###################Apply Logistic classifier on balanced data###########################
#over
model_over <- glm(y~., data = data_balanced_over, family = binomial)
pred_over <- predict(model_over, newdata = test[-17])
roc.curve(test$y, pred_over) #AUC=0.906

#under
model_under <- glm(y~., data = data_balanced_under, family = binomial)
pred_under <- predict(model_under, newdata = test[-17])
roc.curve(test$y, pred_under) #AUC=0.905

#mixed
model_both <- glm(y~., data = data_balanced_both, family = binomial)
pred_both <- predict(model_both, newdata = test[-17])
roc.curve(test$y, pred_both) #AUC=0.906

#rose
model_rose <- glm(y~., data = data.rose, family = binomial)
pred_rose <- predict(model_rose, newdata = test[-17])
roc.curve(test$y, pred_rose) #AUC=0.903

#smote
#model_smote <- glm(y~., data = data_balanced_smote, family = binomial)
#pred_smote <- predict(model_smote, newdata = test[-17])
#roc.curve(test$y, pred_smote) #AUC=

#### mixed & over gives better AUC ##
final_both <- ovun.sample(y ~ ., data = data, method = "both", p=0.5, N=36726, seed = 1)$data 
table(final_both$y)

model_final <- glm(y~., data = final_both, family = binomial)
pred_final <- predict(model_final, newdata = data)
roc.curve(data$y, pred_final) #AUC=0.910

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(pred_final>0.5,data$y)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 87.38

######### modelling without balancing #######
# build glm model
datamodel<-glm(y~.,data = data,family = binomial)
summary(datamodel)

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(datamodel))

# Confusion matrix table 
prob <- predict(datamodel,data,type=c("response"))
prob

# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,data$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 90.18

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
data[,"prob"] <- prob
data[,"pred_values"] <- pred_values
data[,"yes_no"] <- yes_no

table(data$y,data$pred_values)

library(ROCR)
rocrpred<-prediction(prob,data$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

####### accuracy is better without balancing of data as per accuracy rating###

