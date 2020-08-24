concrete <- read.csv(file.choose())
View(concrete)
str(concrete)

# Exploratory data Analysis :
hist(concrete$cement, prob = T, breaks = 30)
lines(density(concrete$cement))
summary(concrete$cement)

hist(concrete$slag, prob = T, breaks = 30)
lines(density(concrete$slag))
summary(concrete$slag)

summary(concrete)
# data has different scales and needs a normalization

# normalization function
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
View(concrete_norm)

concrete_train<-concrete_norm[1:773, ]
concrete_test<-concrete_norm[774:1030, ]

## or
# Data Partition 
#set.seed(123)
#ind <- sample(2, nrow(concrete_norm), replace = TRUE, prob = c(0.7,0.3))
#concrete_train <- concrete_norm[ind==1,]
#concrete_test  <- concrete_norm[ind==2,]


# training a model
library(neuralnet)  # regression
library(nnet) # classification 
library(NeuralNetTools)

# Building model
# simple ANN with only a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag + ash + water+
                              superplastic + coarseagg + fineagg + age, data = concrete_train)

# visualise the network topology
str(concrete_model)
summary(concrete_model)
plot(concrete_model, rep = "best")

par(mar = numeric(4), family = 'serif')
plotnet(concrete_model, alpha = 0.6)

# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(concrete_model,concrete_test[1:8])

# predicted_strength
predicted_strength <- model_results$net.result

# examine corelation b/w predicted and actual values
cor(predicted_strength,concrete_test$strength)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on strength.
str_max <- max(concrete$strength)
str_min <- min(concrete$strength)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualStrength_pred <- unnormalize(predicted_strength,str_min,str_max)
head(ActualStrength_pred)


#improving model perforance
# a more complex NN topology with 5 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water +
                               superplastic + coarseagg + fineagg + age,data=concrete_norm,hidden = 5)
plot(concrete_model2)
summary(concrete_model2)

model_results2<-compute(concrete_model2,concrete_test[1:8])

pred_strn_5<-model_results2$net.result
cor(pred_strn_5,concrete_test$strength)

plot(pred_strn_5,concrete_test$strength)

par(mar = numeric(4), family = 'serif')
plotnet(concrete_model2, alpha = 0.6)

# SSE(Error) has reduced and training steps had been increased as the number of neurons  under hidden layer are increased