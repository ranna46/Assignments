fraud_data <- read.csv(choose.files())
View(fraud_data)
str(fraud_data)
summary(fraud_data)
attach(fraud_data)

Risky_Good = ifelse(fraud_data$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(fraud_data[ ,-3],Risky_Good)
View(FC)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.6,0.4))
train <- FC[ind==1,]
test  <- FC[ind==2,]

####### using randomforest/ bagging technique #########
rf1 <- randomForest(Risky_Good~., data=train, na.action=na.roughfix,importance=TRUE)
rf1  # oob = 18.85% 
attributes(rf1)
importance(rf1)
# no of nodes of trees
hist(treesize(rf1), main = "No of Nodes for the trees", col = "yellow")

# training
pred1 <- predict(rf1, train)
perf1 <- table(train$Risky_Good, pred1,
               dnn=c("Actual", "Predicted"))
perf1
mean(train$Risky_Good==pred1) # 91.53% accuracy 
confusionMatrix(pred1, train$Risky_Good)

#testing
pred2 <- predict(rf1, test)
perf2 <- table(test$Risky_Good, pred2,
               dnn=c("Actual", "Predicted"))
perf2
mean(test$Risky_Good==pred2) # 74.78% accuracy 
confusionMatrix(pred2, test$Risky_Good)

plot(rf1,lwd=2)
legend("topright", colnames(rf1$err.rate),col=1:4,cex=0.8,fill=1:4)
varImpPlot(rf1)


#### Tune Random Forest Model mtry ####
tune <- tuneRF(train[,-6], train[,6], stepFactor = 2, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 1)
# oob = 18.31%
rf2 <- randomForest(Risky_Good~., data=train, ntree = 300, mtry = 1, importance = TRUE,
                    proximity = TRUE)
rf2
#training
pred3 <- predict(rf2, train)
confusionMatrix(pred3, train$Risky_Good)  # 81.69 % accuracy on training data 
# test data prediction using the Tuned RF2 model
pred4 <- predict(rf2, test)
confusionMatrix(pred4, test$Risky_Good) # 75.64 % accuracy on test data
# which predictor variables are actually used in the random forest.
varUsed(rf2)  

# Extract single tree from the forest :
getTree(rf2, 1, labelVar = TRUE)

### using cforest() function###########
library(party)
rf3 <- cforest(Risky_Good~., data=train)
rf3
#attributes(rf1)
# training
pred_rf3 <- predict(rf3, newdata=train)
perf_rf3 <- table(train$Risky_Good, pred_rf3,
                  dnn=c("Actual", "Predicted"))
perf_rf3
mean(train$Risky_Good==pred_rf3) # 81.69% accuracy 
confusionMatrix(pred_rf3, train$Risky_Good)

#testing
pred2_rf3 <- predict(rf3, newdata=test)
perf2_rf3 <- table(test$Risky_Good, pred2_rf3,
                   dnn=c("Actual", "Predicted"))
perf2_rf3
mean(test$Risky_Good==pred2_rf3) # 75.6% accuracy 
confusionMatrix(pred2_rf3, test$Risky_Good)

########### using boosting technique########

# Building model on training data 
library(C50)
rf4 <- C5.0(FC[,-6],FC$Risky_Good, trials = 9) #C5.0(x,y)
rf4
summary(rf4)
pred_rf4 <- predict(rf4, newdata=train)
perf_rf4 <- table(train$Risky_Good, pred_rf4,
                  dnn=c("Actual", "Predicted"))
perf_rf4
mean(train$Risky_Good==pred_rf4) # 81.69% accuracy 
confusionMatrix(pred_rf4, train$Risky_Good)

#testing
pred2_rf4 <- predict(rf4, newdata=test)
perf2_rf4 <- table(test$Risky_Good, pred2_rf4,
                   dnn=c("Actual", "Predicted"))
perf2_rf4
mean(test$Risky_Good==pred2_rf4) # 75.64% accuracy 
confusionMatrix(pred2_rf4, test$Risky_Good)


############  ###########
