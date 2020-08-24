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
library(ROSE)

affairs_data <- read.csv(choose.files())
View(affairs_data)
#EDA
str(affairs_data)
summary(affairs_data)
skim(affairs_data)
boxplot(affairs_data)
dim(affairs_data)
pairs(affairs_data)
missmap(affairs_data, col=c("black", "grey"), legend=FALSE) # checking for missing data


library(plyr)
affairs_data$affairs[affairs_data$affairs > 0] <- 1
affairs_data$affairs[affairs_data$affairs == 0] <- 0
affairs_data$gender <- as.factor(revalue(affairs_data$gender,c("male"=1, "female"=0)))
affairs_data$children <- as.factor(revalue(affairs_data$children,c("yes"=1, "no"=0)))
affairs_data$affairs <- as.factor(affairs_data$affairs)
train$affairs <- as.factor(train$affairs)
test$affairs <- as.factor(test$affairs)

#affairs_data$age <- as.factor(affairs_data$age)
#affairs_data$yearsmarried <- as.factor(affairs_data$yearsmarried)
#affairs_data$yes_no <- as.factor(affairs_data$yes_no)
#affairs_data$religiousness <- as.factor(affairs_data$religiousness)
#affairs_data$education <- as.factor(affairs_data$education)
#affairs_data$occupation <- as.factor(affairs_data$occupation)
#affairs_data$rating <- as.factor(affairs_data$rating)
#affairs_data$prob <- as.factor(affairs_data$prob)
#affairs_data$pred_values <- as.factor(affairs_data$pred_values)

as.data.frame(table(affairs_data$affairs))## checking for imbalance in data
prop.table(table(affairs_data$affairs))

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(affairs_data), replace = TRUE, prob = c(0.8,0.2))
train <- affairs_data[ind==1,]
test  <- affairs_data[ind==2,]

prop.table(table(train$affairs))
prop.table(table(test$affairs))

#simple model
model_simple <- glm(affairs~., data = train, family = binomial)
pred_simple <- predict(model_simple, newdata = test[-1])
roc.curve(test$affairs, pred_simple) #AUC=0.73

## balancing the data ##
attach(affairs_data)

#over sampling
data_balanced_over <- ovun.sample(affairs ~ ., data = train, method = "over",N = 734)$data #N must be double of good points 367*2
table(data_balanced_over$affairs)

#under sampling
data_balanced_under <- ovun.sample(affairs ~ ., data = train, method = "under", N = 226, seed = 1)$data #N must be double of negative points 113*2
table(data_balanced_under$affairs)

#mixed sampling
data_balanced_both <- ovun.sample(affairs ~ ., data = train, method = "both", p=0.5, N=480, seed = 1)$data #N is 367+113
table(data_balanced_both$affairs)

#rose sampling 
data.rose <- ROSE(affairs ~ ., data = train, seed = 111)$data
table(data.rose$affairs)

#SMOTE sampling
data_balanced_smote <- SMOTE(affairs ~ ., data = train, perc.over = 734, perc.under=226)
table(data_balanced_smote$affairs)

###################Apply Logistic classifier on balanced data###########################
#over
model_over <- glm(affairs~., data = data_balanced_over, family = binomial)
pred_over <- predict(model_over, newdata = test[-1])
roc.curve(test$affairs, pred_over) #AUC=0.719

#under
model_under <- glm(affairs~., data = data_balanced_under, family = binomial)
pred_under <- predict(model_under, newdata = test[-1])
roc.curve(test$affairs, pred_under) #AUC=0.681

#mixed
model_both <- glm(affairs~., data = data_balanced_both, family = binomial)
pred_both <- predict(model_both, newdata = test[-1])
roc.curve(test$affairs, pred_both) #AUC=0.751

#rose
model_rose <- glm(affairs~., data = data.rose, family = binomial)
pred_rose <- predict(model_rose, newdata = test[-1])
roc.curve(test$affairs, pred_rose) #AUC=0.726

#smote
model_smote <- glm(affairs~., data = data_balanced_smote, family = binomial)
pred_smote <- predict(model_smote, newdata = test[-1])
roc.curve(test$affairs, pred_smote) #AUC=0.715

#### mixed gives better AUC ##
final_both <- ovun.sample(affairs ~ ., data = affairs_data, method = "both", p=0.5, N=601, seed = 1)$data #N is 451+150
table(final_both$affairs)

model_final <- glm(affairs~., data = final_both, family = binomial)
pred_final <- predict(model_final, newdata = affairs_data)
roc.curve(affairs_data$affairs, pred_final) #AUC=0.7

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(pred_final>0.5,affairs_data$affairs)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 68.05

######################### Modelling without any balancing of data ############

# build glm model
datamodel<-glm(affairs~gender + age + yearsmarried + 
                      children + factor(religiousness) + factor(education) + factor(occupation) + 
                      factor(rating),data = affairs_data,family = binomial)
summary(datamodel)

exp(coef(datamodel))

# Confusion matrix table 
prob <- predict(datamodel,affairs_data,type="response")
prob

# Confusion matrix and considering the threshold value as 0.5 
confusion1<-table(prob>0.5,affairs_data$affairs)
confusion1

# Model Accuracy 
Accuracy<-sum(diag(confusion1)/sum(confusion1))
Accuracy # 77.87

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
affairs_data[,"prob"] <- prob
affairs_data[,"pred_values"] <- pred_values
affairs_data[,"yes_no"] <- yes_no

View(affairs_data[,c(1,9:11)])

table(affairs_data$affairs,affairs_data$pred_values)

library(ROCR)
rocrpred<-prediction(prob,affairs_data$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

#roc.curve(rocrperf,rocrpred, plotit = TRUE)

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))

########### since the data was in ratio of 75:25, further balancing of data 
#was not required as per accuracy rating 





