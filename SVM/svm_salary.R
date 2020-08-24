library(kernlab)
library(caret)
library(e1071)
library(plyr)

salary_train <- read.csv(choose.files())
View(salary_train)
str(salary_train)
summary(salary_train)
salary_train$educationno <- as.factor(salary_train$educationno)
class(salary_train$educationno)

salary_test <- read.csv(choose.files())
View(salary_test)
str(salary_test)
summary(salary_test)
salary_test$educationno <- as.factor(salary_test$educationno)
class(salary_test$educationno)

# Building model 
model1<-ksvm(salary_train$Salary~., 
             data= salary_train, kernel = "vanilladot")
model1

#Evaluating model
Salary_prediction <- predict(model1, salary_test)

table(Salary_prediction,salary_test$Salary)

agreement <- Salary_prediction == salary_test$Salary
table(agreement)

prop.table(table(agreement)) #84.64

# kernel = rfdot 
model_rfdot<-ksvm(salary_train$Salary~., 
                  data= salary_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=salary_test)
mean(pred_rfdot==salary_test$Salary) # 85.19

# kernel = besseldot
model_vanilla<-ksvm(salary_train$Salary~., 
                    data= salary_train,kernel = "besseldot")
pred_vanilla<-predict(model_vanilla,newdata=salary_test)
mean(pred_vanilla==salary_test$Salary) # 78.97

# kernel = polydot
model_vanilla<-ksvm(salary_train$Salary~., 
                    data= salary_train,kernel = "polydot")
pred_vanilla<-predict(model_vanilla,newdata=salary_test)
mean(pred_vanilla==salary_test$Salary) # 84.61
