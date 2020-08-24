library(mlbench)
library(Amelia)
library(e1071)
library(skimr)
library(corrplot)
library(caret)
glass <- read.csv(choose.files())
View(glass)

#EDA
summary(glass)
skim(glass)
str(glass)
dim(glass)
sapply(glass[,1:9], sd)
skew <- apply(glass[,1:9], 2, skewness)
skew
correlations <- cor(glass[,1:9])
corrplot(correlations, method="circle")
y <- glass$Type # checking for imbalance in data
cbind(freq=table(y), percentage=prop.table(table(y))*100)
pairs(glass)
windows()
par(mfrow=c(3,3)) # histograms
for(i in 1:9) {
  hist(glass[,i], main=names(glass)[i])
}
windows()
par(mfrow=c(3,3)) # density plots
for(i in 1:9) {
  plot(density(glass[,i]), main=names(glass)[i])
}
windows()
par(mfrow=c(3,3)) # boxplots
for(i in 1:9) {
  boxplot(glass[,i], main=names(glass)[i])
}
# removing outliers
outliers1 <- boxplot(glass$RI,plot=FALSE)$out
glass[which(glass$RI %in% outliers1),]
glass <- glass[-which(glass$RI %in% outliers1),]
boxplot(glass$RI)

missmap(glass, col=c("black", "grey"), legend=FALSE) # checking for missing data


glass$Type <- as.factor(glass$Type)
class(glass$Type)

# Data partition
set.seed(123)
ind <- sample(2,nrow(glass), replace = T, prob = c(0.6,0.4))
train <- glass[ind==1,]
test <- glass[ind==2,]

# KNN Model 

trcontrol <- trainControl(method = "repeatedcv", number = 15,repeats = 3)

set.seed(1234)
fit <- train(Type ~., data = train, method = 'knn', tuneLength = 20, metric="Accuracy",
             trControl = trcontrol, preProc = c("center","scale"))

fit # k=5, acc=65.8%

plot(fit)
varImp(fit)
pred <- predict(fit, newdata = test, k=5)
confusionMatrix(pred, test$Type)
mean(pred==test$Type) # 70%
























