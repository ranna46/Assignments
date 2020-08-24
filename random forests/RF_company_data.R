company_data <- read.csv(choose.files())
View(company_data)
str(company_data)
summary(company_data)
hist(company_data$Sales)

highsales = ifelse(company_data$Sales<9, "No", "Yes")  # if greater than 9 then high sales else Low
CD = data.frame(company_data[2:11], highsales)
str(CD)
table(CD$highsales)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]

####### using randomforest/ bagging technique #########
rf <- randomForest(highsales~., data=train, na.action=na.roughfix,importance=TRUE)
rf  # oob = 16.84% 
# Description of the random forest with no of trees, mtry = no of variables for splittin
attributes(rf)
importance(rf)
# no of nodes of trees
hist(treesize(rf), main = "No of Nodes for the trees", col = "yellow")

# training
pred1 <- predict(rf, train)
perf1 <- table(train$highsales, pred1,
                       dnn=c("Actual", "Predicted"))
perf1
mean(train$highsales==pred1) # 100% accuracy 
confusionMatrix(pred1, train$highsales)

#testing
pred2 <- predict(rf, test)
perf2 <- table(test$highsales, pred2,
               dnn=c("Actual", "Predicted"))
perf2
mean(test$highsales==pred2) # 84.3% accuracy 
confusionMatrix(pred2, test$highsales)

plot(rf,lwd=2)
legend("topright", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)
varImpPlot(rf)


#### Tune Random Forest Model mtry ####
tune <- tuneRF(train[,-11], train[,11], stepFactor = 2, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 1)

rf2 <- randomForest(highsales~., data=train, ntree = 300, mtry = 6, importance = TRUE,
                    proximity = TRUE)
rf2
#training
pred3 <- predict(rf2, train)
confusionMatrix(pred3, train$highsales)  # 100 % accuracy on training data 
# test data prediction using the Tuned RF2 model
pred4 <- predict(rf2, test)
confusionMatrix(pred4, test$highsales) # 84.35 % accuracy on test data

# no of nodes of trees
hist(treesize(rf2), main = "No of Nodes for the trees", col = "brown")
varImpPlot(rf2)
importance(rf2)
# which predictor variables are actually used in the random forest.
varUsed(rf2)  

# Partial Dependence Plot 
partialPlot(rf2, train, Price, "Yes")

# Extract single tree from the forest :
getTree(rf2, 1, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf2, CD$highsales)


### using cforest() function###########
library(party)
rf1 <- cforest(highsales~., data=train)
rf1
#attributes(rf1)
# training
pred_rf1 <- predict(rf1, newdata=train)
perf_rf1 <- table(train$highsales, pred_rf1,
               dnn=c("Actual", "Predicted"))
perf_rf1
mean(train$highsales==pred_rf1) # 89.12% accuracy 
confusionMatrix(pred_rf1, train$highsales)

#testing
pred2_rf1 <- predict(rf1, newdata=test)
perf2_rf1 <- table(test$highsales, pred2_rf1,
               dnn=c("Actual", "Predicted"))
perf2_rf1
mean(test$highsales==pred2_rf1) # 78.2% accuracy 
confusionMatrix(pred2_rf1, test$highsales)

########### using boosting technique########

# Building model on training data 
library(C50)
rf3 <- C5.0(CD[,-11],CD$highsales, trials = 5) #C5.0(x,y)
rf3
summary(rf3)
pred_rf3 <- predict(rf3, newdata=train)
perf_rf3 <- table(train$highsales, pred_rf3,
                  dnn=c("Actual", "Predicted"))
perf_rf3
mean(train$highsales==pred_rf3) # 100% accuracy 
confusionMatrix(pred_rf3, train$highsales)

#testing
pred2_rf3 <- predict(rf3, newdata=test)
perf2_rf3 <- table(test$highsales, pred2_rf3,
                   dnn=c("Actual", "Predicted"))
perf2_rf3
mean(test$highsales==pred2_rf3) # 100% accuracy 
confusionMatrix(pred2_rf3, test$highsales)
plot(rf3)

############ rf3 model has 100% accuracy ###########



