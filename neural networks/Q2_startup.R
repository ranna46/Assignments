startup <- read.csv(file.choose())
View(startup)
str(startup)
summary(startup)
pairs(startup)

# Exploratory data Analysis :
hist(startup$R.D.Spend, prob = T, breaks = 30)
lines(density(startup$R.D.Spend))
summary(startup$R.D.Spend)

hist(startup$Administration, prob = T, breaks = 30)
lines(density(startup$Administration))
summary(startup$Administration)

hist(startup$Marketing.Spend, prob = T, breaks = 30)
lines(density(startup$Marketing.Spend))
summary(startup$Marketing.Spend)

startup_dummies <- model.matrix(~State -1, data=startup)
head(startup_dummies)

startup <- startup[,-4]
startup <- cbind(startup,startup_dummies)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

startup_norm<-as.data.frame(lapply(startup,FUN=normalize))
View(startup_norm)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(startup_norm), replace = TRUE, prob = c(0.7,0.3))
startup_train <- startup_norm[ind==1,]
startup_test  <- startup_norm[ind==2,]


# training a model
library(neuralnet)  # regression
library(nnet) # classification 
library(NeuralNetTools)

# Building model
# simple ANN with only a single hidden neuron
startup_model <- neuralnet(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + 
                             StateCalifornia + StateFlorida + StateNew.York, data = startup_train)

# visualise the network topology
str(startup_model)
summary(startup_model)
plot(startup_model, rep = "best")

par(mar = numeric(4), family = 'serif')
plotnet(startup_model, alpha = 0.6)


# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(startup_model,startup_test[-4])

# predicted_profit
predicted_profit <- model_results$net.result

# examine corelation b/w predicted and actual values
cor(predicted_profit,startup_test$Profit)


# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on strength.
str_max <- max(startup$Profit)
str_min <- min(startup$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualprofit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(Actualprofit_pred)

#improving model perforance
# a more complex NN topology with 2 hidden neurons
startup_model2 <- neuralnet(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + 
                             StateCalifornia + StateFlorida + StateNew.York, data = startup_train, hidden = 2)
plot(startup_model2)
summary(startup_model2)

model_results2<-compute(startup_model2,startup_test[-4])

pred_profit_2<-model_results2$net.result
cor(pred_profit_2,startup_test$Profit)

plot(pred_profit_2,startup_test$Profit)

par(mar = numeric(4), family = 'serif')
plotnet(startup_model2, alpha = 0.6)

# SSE(Error) has reduced and training steps had been increased as the number of neurons  under hidden layer are increased





















