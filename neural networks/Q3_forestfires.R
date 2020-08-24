##getting negative area and very low correlation
forestfire <- read.csv(file.choose())
View(forestfire)
str(forestfire)
summary(forestfire)

forestfire_dummies <- model.matrix(~size_category -1, data=forestfire)
head(forestfire_dummies)

forestfire <- forestfire[, -c(1:2,31)]
forestfire <- cbind(forestfire,forestfire_dummies)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

forestfire_norm<-as.data.frame(lapply(forestfire,FUN=normalize))
View(forestfire_norm)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(forestfire_norm), replace = TRUE, prob = c(0.7,0.3))
forestfire_train <- forestfire_norm[ind==1,]
forestfire_test  <- forestfire_norm[ind==2,]

# training a model
library(neuralnet)  # regression
library(nnet) # classification 
library(NeuralNetTools)

# Building model
# simple ANN with only a single hidden neuron
forestfire_model <- neuralnet(formula = area ~ FFMC + DMC + DC + ISI +
                             temp + RH + wind + rain + dayfri+daymon+daysat+daysun+
                               daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+
                               monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+
                               monthsep, data = forestfire_train)

# visualise the network topology
str(forestfire_model)
summary(forestfire_model)
windows()
plot(forestfire_model, rep = "best")

par(mar = numeric(4), family = 'serif')
plotnet(forestfire_model, alpha = 0.6)


# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(forestfire_model,forestfire_test[-9])

# predicted_profit
predicted_area <- model_results$net.result

# examine corelation b/w predicted and actual values
cor(predicted_area,forestfire_test$area)


# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on strength.
str_max <- max(forestfire$area)
str_min <- min(forestfire$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualarea_pred <- unnormalize(predicted_area,str_min,str_max)
head(Actualarea_pred)



#improving model perforance
# a more complex NN topology with 2 hidden neurons
forestfire_model2 <- neuralnet(formula = area ~ FFMC + DMC + DC + ISI +
                                temp + RH + wind + rain + dayfri+daymon+daysat+daysun+
                                daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+
                                monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+
                                monthsep, data = forestfire_train, hidden = 5)
plot(forestfire_model2)
summary(forestfire_model2)

model_results2<-compute(forestfire_model2,forestfire_test[-9])

pred_area_2<-model_results2$net.result
cor(pred_area_2,forestfire_test$area)

plot(pred_area_2,forestfire_test$area)

par(mar = numeric(4), family = 'serif')
plotnet(forestfire_model2, alpha = 0.6)


























