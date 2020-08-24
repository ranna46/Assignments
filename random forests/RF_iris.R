data()
?iris
View(iris)
data("iris")
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 


iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

####### using randomforest/ bagging technique #########
rf <- randomForest(Species~., data=train, na.action=na.roughfix,importance=TRUE)
rf  # oob = 4% 
# Description of the random forest with no of trees, mtry = no of variables for splittin
attributes(rf)
importance(rf)
# no of nodes of trees
hist(treesize(rf), main = "No of Nodes for the trees", col = "yellow")

# training
pred <- predict(rf, train)
perf <- table(train$Species, pred,
               dnn=c("Actual", "Predicted"))
perf
mean(train$Species==pred) # 100% accuracy 
confusionMatrix(pred, train$Species)

#testing
pred1 <- predict(rf, test)
perf1 <- table(test$Species, pred1,
               dnn=c("Actual", "Predicted"))
perf1
mean(test$Species==pred1) # 94.4% accuracy 
confusionMatrix(pred1, test$Species)

plot(rf,lwd=2)
legend("topright", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)
varImpPlot(rf)

#### Tune Random Forest Model mtry ####
tune <- tuneRF(train[,-5], train[,5], stepFactor = 2, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.5)

rf2 <- randomForest(Species~., data=train, ntree = 300, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf2
#training
pred2 <- predict(rf2, train)
confusionMatrix(pred2, train$Species)  # 100 % accuracy on training data 
# test data prediction using the Tuned RF2 model
pred3 <- predict(rf2, test)
confusionMatrix(pred3, test$Species) # 94.6% accuracy on test data

# no of nodes of trees
hist(treesize(rf2), main = "No of Nodes for the trees", col = "brown")
varImpPlot(rf2)
importance(rf2)
# which predictor variables are actually used in the random forest.
varUsed(rf2)  

# Partial Dependence Plot 
partialPlot(rf2, train, Petal.Length, "versicolor")
partialPlot(rf2, train, Petal.Length, "setosa")
partialPlot(rf2, train, Petal.Length, "virginica")

# Extract single tree from the forest :
getTree(rf2, 1, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf2, iris$Species)

### using cforest() function###########
library(party)
rf3 <- cforest(Species~., data=train)
rf3
# training
pred_rf3 <- predict(rf3, newdata=train)
perf_rf3 <- table(train$Species, pred_rf3,
                  dnn=c("Actual", "Predicted"))
perf_rf3
mean(train$Species==pred_rf3) # 97.33% accuracy 
confusionMatrix(pred_rf3, train$Species)

#testing
pred2_rf3 <- predict(rf3, newdata=test)
perf2_rf3 <- table(test$Species, pred2_rf3,
                   dnn=c("Actual", "Predicted"))
perf2_rf3
mean(test$Species==pred2_rf3) # 94.6% accuracy 
confusionMatrix(pred2_rf3, test$Species)

########### using boosting technique########

# Building model on training data 
library(C50)
rf4 <- C5.0(iris[,-5],iris$Species, trials = 8) #C5.0(x,y)
rf4
summary(rf4)
pred_rf4 <- predict(rf4, newdata=train)
perf_rf4 <- table(train$Species, pred_rf4,
                  dnn=c("Actual", "Predicted"))
perf_rf4
mean(train$Species==pred_rf4) # 100% accuracy 
confusionMatrix(pred_rf4, train$Species)

#testing
pred2_rf4 <- predict(rf4, newdata=test)
perf2_rf4 <- table(test$Species, pred2_rf4,
                   dnn=c("Actual", "Predicted"))
perf2_rf4
mean(test$Species==pred2_rf4) # 100% accuracy 
confusionMatrix(pred2_rf4, test$Species)
plot(rf4)

############ rf4 model has 100% accuracy ###########


