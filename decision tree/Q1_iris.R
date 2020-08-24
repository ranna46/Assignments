data()
?iris
View(iris)
data("iris")
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 


iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# building model
library(party)
fit.ctree <- ctree(Species~., data=iris_train)
plot(fit.ctree, main="Conditional Inference Tree")

# Training accuracy
ctree.pred <- predict(fit.ctree, iris_train, type="response")

mean(iris_train$Species==ctree.pred) # 97.33% Accuracy

ctree.perf <- table(iris_train$Species, ctree.pred,
                     dnn=c("Actual", "Predicted"))
ctree.perf

library(caret)
confusionMatrix(ctree.pred,iris_train$Species)

# predicting on test data
ctree.pred1 <- predict(fit.ctree, iris_test, type="response")
ctree.perf1 <- table(iris_test$Species, ctree.pred1,
                      dnn=c("Actual", "Predicted"))
ctree.perf1
mean(ctree.pred1==iris_test$Species) # 94.67% accuracy 
confusionMatrix(ctree.pred1,iris_test$Species)
library(gmodels)
# Cross tablez
CrossTable(iris_test$Species,ctree.pred1)




















