calories_consumed <- read.csv(choose.files())
View(calories_consumed)

# Exploratory data analysis
#1st moment business decision
mean(calories_consumed$Weight.gained..grams.)
mean(calories_consumed$Calories.Consumed)
median(calories_consumed$Weight.gained..grams.)
median(calories_consumed$Calories.Consumed)
#mode
getmode <- function(calories_consumed){
  uniquv <- unique(calories_consumed)
  uniquv[which.max(tabulate(match(calories_consumed,uniquv)))]
}
getmode(calories_consumed$Calories.Consumed)
getmode(calories_consumed$Weight.gained..grams.)

#2nd moment/Measures of Dispersion
var(calories_consumed$Weight.gained..grams.)
sd(calories_consumed$Weight.gained..grams.)
range(calories_consumed$Weight.gained..grams.)
rangevalue <- function(calories_consumed){max(calories_consumed)-min(calories_consumed)}
rangevalue(calories_consumed$Weight.gained..grams.)


var(calories_consumed$Calories.Consumed)
sd(calories_consumed$Calories.Consumed)
range(calories_consumed$Calories.Consumed)
rangevalue <- function(calories_consumed){max(calories_consumed)-min(calories_consumed)}
rangevalue(calories_consumed$Calories.Consumed)

#Measures of skewness
library(moments)

#Measures of skewness
skewness(calories_consumed$Weight.gained..grams.)
skewness(calories_consumed$Calories.Consumed)

#Measures of Kurtosis 
kurtosis(calories_consumed$Weight.gained..grams.)
kurtosis(calories_consumed$Calories.Consumed)

boxplot(calories_consumed$Weight.gained..grams.)
boxplot(calories_consumed$Calories.Consumed)
hist(calories_consumed$Weight.gained..grams.)
hist(calories_consumed$Calories.Consumed)
plot(calories_consumed$Weight.gained..grams., calories_consumed$Calories.Consumed)

attach(calories_consumed)

#Correlation Coefficient (r)
cor(Calories.Consumed, Weight.gained..grams.)


# Simple Linear Regression model
reg <- lm(Weight.gained..grams. ~ Calories.Consumed) # lm(Y ~ X)

summary(reg)
pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(calories_consumed))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = calories_consumed, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=Calories.Consumed, y=pred))

