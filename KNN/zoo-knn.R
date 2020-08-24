#install.packages("Amelia")
#install.packages("corrplot")
library(Amelia)
library(mlbench)
library(e1071)
library(corrplot)
library(skimr)
library(caret)
zoo <- read.csv(choose.files())
View(zoo)
#EDA
str(zoo)
summary(zoo)
dim(zoo)
sapply(zoo[,2:18], sd)
skew <- apply(zoo[,2:18], 2, skewness)
skew
skim(zoo)
correlations <- cor(zoo[,2:18])
corrplot(correlations, method="circle")
y <- zoo$type # checking for imbalance in data
cbind(freq=table(y), percentage=prop.table(table(y))*100)
missmap(zoo, col=c("black", "grey"), legend=FALSE) # checking for missing data
pairs(zoo)

#zoo$hair <- as.factor(zoo$hair)
#zoo$feathers <- as.factor(zoo$feathers)
#zoo$eggs <- as.factor(zoo$eggs)
#zoo$milk <- as.factor(zoo$milk)
#zoo$airborne <- as.factor(zoo$airborne)
#zoo$aquatic <- as.factor(zoo$aquatic)
#zoo$predator <- as.factor(zoo$predator)
#zoo$toothed <- as.factor(zoo$toothed)
#zoo$backbone <- as.factor(zoo$backbone)
#zoo$breathes <- as.factor(zoo$breathes)
#zoo$venomous <- as.factor(zoo$venomous)
#zoo$fins <- as.factor(zoo$fins)
#zoo$legs <- as.factor(zoo$legs)
#zoo$tail <- as.factor(zoo$tail)
#zoo$domestic <- as.factor(zoo$domestic)
#zoo$catsize <- as.factor(zoo$catsize)
zoo$type <- as.factor(zoo$type)

str(zoo)
zoo <- zoo[,2:18]

# Data partition
set.seed(123)
ind <- sample(2,nrow(zoo), replace = T, prob = c(0.6,0.4))
train <- zoo[ind==1,]
test <- zoo[ind==2,]

# KNN Model 

trcontrol <- trainControl(method = "repeatedcv", number = 15,repeats = 3)
                         
set.seed(1234)
fit <- train(type ~., data = train, method = 'knn', tuneLength = 20, metric="Accuracy",
             trControl = trcontrol, preProc = c("center","scale"))

fit # k=5, acc=88.92%
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = test, k=5)
confusionMatrix(pred, test$type)
mean(pred==test$type) # 95%








