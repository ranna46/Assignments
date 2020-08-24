library(e1071)
library(caret)
library(skimr)
library(corrplot)
library(mlbench)
library(Amelia)

#EDA
train <- read.csv(choose.files())
test <- read.csv(choose.files())

attach(train)
str(train)
summary(train)
skim(train)
missmap(train, col=c("black", "grey"), legend=FALSE) # checking for missing data
pairs(train)
hist(train$age)
hist(train$educationno)
hist(train$capitalgain)
hist(train$capitalloss)
hist(train$hoursperweek)

boxplot(train$age)
boxplot(train$educationno)
boxplot(train$capitalgain)
boxplot(train$capitalloss)
boxplot(train$hoursperweek)


attach(test)
str(test)
summary(test)
skim(test)
missmap(test, col=c("black", "grey"), legend=FALSE) # checking for missing data
pairs(test)
hist(test$age)
hist(test$educationno)
hist(test$capitalgain)
hist(test$capitalloss)
hist(test$hoursperweek)

boxplot(test$age)
boxplot(test$educationno)
boxplot(test$capitalgain)
boxplot(test$capitalloss)
boxplot(test$hoursperweek)


train$educationno <- as.factor(train$educationno)
class(train$educationno)
test$educationno <- as.factor(test$educationno)
class(test$educationno)

model <- naiveBayes(train$Salary~., data = train)
model
summary(model)

pred <- predict(model, train)
mean(pred==train$Salary) # 82.2%
CrossTable(pred, train$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

pred1 <- predict(model, newdata = test)
mean(pred1==test$Salary) # 81.87%
CrossTable(pred1, test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
###########
model1 <- naiveBayes(train$Salary~., data = train, laplace = 2)
pred2 <- predict(model1, train)
mean(pred2==train$Salary) # 82.2%

pred3 <- predict(model1, newdata = test)
mean(pred3==test$Salary) # 81.87%
