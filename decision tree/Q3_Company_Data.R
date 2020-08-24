company_data <- read.csv(choose.files())
View(company_data)
str(company_data)
summary(company_data)
hist(company_data$Sales)
attach(company_data)

high = ifelse(company_data$Sales<10, "No", "Yes")
cd <- data.frame(company_data, high)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(cd), replace = TRUE, prob = c(0.5,0.5))
cd_train <- cd[ind==1,]
cd_test  <- cd[ind==2,]

# building model
library(party)
fit1 <- ctree(high~CompPrice + Income + Advertising + 
                Population + Price + ShelveLoc + Age + Education + Urban + US, data=cd_train)
plot(fit1, main="Conditional Inference Tree")

# Training accuracy
pred1 <- predict(fit1, newdata=cd_train, type="response")

mean(cd_train$high==pred1) # 90.48% Accuracy

ctree.perf1 <- table(cd_train$high, pred1,
                    dnn=c("Actual", "Predicted"))
ctree.perf1

library(caret)
confusionMatrix(pred1,cd_train$high)


# predicting on test data
pred2 <- predict(fit1, newdata=cd_test, type="response")

mean(cd_test$high==pred2) # 78.67% Accuracy

ctree.perf2 <- table(cd_test$high, pred2,
                     dnn=c("Actual", "Predicted"))
ctree.perf2
 
confusionMatrix(pred2,cd_test$high)
library(gmodels)
# Cross tablez
CrossTable(cd_test$high,pred2)

#####################
# building model
library(rpart)
fit2 <- rpart(high~CompPrice + Income + Advertising + 
                Population + Price + ShelveLoc + Age + Education + Urban + US, data=cd_train)
plot(fit2,margin=0.1)
text(fit2, use.n = TRUE, pretty = TRUE, cex=0.6)

# Training accuracy
pred3 <- predict(fit2, newdata=cd_train, type="class")

mean(cd_train$high==pred3) # 91.01% Accuracy

ctree.perf3 <- table(cd_train$high, pred3,
                     dnn=c("Actual", "Predicted"))
ctree.perf3

library(caret)
confusionMatrix(pred3,cd_train$high)


# predicting on test data
pred4 <- predict(fit2, newdata=cd_test, type="class")

mean(cd_test$high==pred4) # 83.89% Accuracy

ctree.perf4 <- table(cd_test$high, pred4,
                     dnn=c("Actual", "Predicted"))
ctree.perf4

confusionMatrix(pred4,cd_test$high)
library(gmodels)
# Cross tablez
CrossTable(cd_test$high,pred4)

######## the rpart model is more accurate######







