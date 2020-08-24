fraud_data <- read.csv(choose.files())
View(fraud_data)
str(fraud_data)
summary(fraud_data)
attach(fraud_data)

Risky_Good = ifelse(fraud_data$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(fraud_data,Risky_Good)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.5,0.5))
FC_train <- FC[ind==1,]
FC_test  <- FC[ind==2,]


# building model
library(party)
fit.ctree <- ctree(Risky_Good~Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data=FC_train)
plot(fit.ctree, main="Conditional Inference Tree")

# Training accuracy
fit.pred <- predict(fit.ctree, FC_train, type="response")

mean(FC_train$Risky_Good==fit.pred) # 77.24% Accuracy

ctree.perf <- table(FC_train$Risky_Good, fit.pred,
                    dnn=c("Actual", "Predicted"))
ctree.perf

library(caret)
confusionMatrix(fit.pred,FC_train$Risky_Good)

# predicting on test data
ctree.pred1 <- predict(fit.ctree, FC_test, type="response")
ctree.perf1 <- table(FC_test$Risky_Good, ctree.pred1,
                     dnn=c("Actual", "Predicted"))
ctree.perf1
mean(ctree.pred1==FC_test$Risky_Good) # 81.29% accuracy 
confusionMatrix(ctree.pred1,FC_test$Risky_Good)
library(gmodels)
# Cross tablez
CrossTable(FC_test$Risky_Good,ctree.pred1)

#####################################
library(rpart)
fit <- rpart(Risky_Good~Undergrad + Marital.Status + City.Population + 
               Work.Experience + Urban, data=FC_train)
fit
plot(fit,margin=0.1)
text(fit, use.n = TRUE, pretty = TRUE, cex=0.6)
# Training accuracy
fit.pred1 <- predict(fit, newdata=FC_train, type="class")

mean(FC_train$Risky_Good==fit.pred1) # 78.62% Accuracy

fit.perf <- table(FC_train$Risky_Good, fit.pred1,
                    dnn=c("Actual", "Predicted"))
fit.perf

library(caret)
confusionMatrix(fit.pred1,FC_train$Risky_Good)

# predicting on test data
fit.pred2 <- predict(fit, newdata=FC_test, type="class")
fit.perf2 <- table(FC_test$Risky_Good, fit.pred2,
                     dnn=c("Actual", "Predicted"))
fit.perf2
mean(fit.pred2==FC_test$Risky_Good) # 78.06% accuracy 
confusionMatrix(fit.pred2,FC_test$Risky_Good)
library(gmodels)
# Cross tablez
CrossTable(FC_test$Risky_Good,fit.pred2)

######## the ctree model is more accurate######













