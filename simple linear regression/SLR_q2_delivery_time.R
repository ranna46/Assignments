timesheet <- read.csv(choose.files())
View(timesheet)

#EDA
#1st moment
summary(timesheet)
#mode
getmode <- function(timesheet){
  uniquv <- unique(timesheet)
  uniquv[which.max(tabulate(match(timesheet,uniquv)))]
}  

getmode(timesheet$Delivery.Time)
getmode(timesheet$Sorting.Time)

#2nd moment
var(timesheet$Delivery.Time)
sd(timesheet$Delivery.Time)
range(timesheet$Delivery.Time)
rangevalue <- function(timesheet){max(timesheet)-min(timesheet)}
rangevalue(timesheet$Delivery.Time)

var(timesheet$Sorting.Time)
sd(timesheet$Sorting.Time)
range(timesheet$Sorting.Time)
rangevalue <- function(timesheet){max(timesheet)-min(timesheet)}
rangevalue(timesheet$Sorting.Time)

#3rd moment
library(moments)
skewness(timesheet$Delivery.Time)
skewness(timesheet$Sorting.Time)
#4th moment
kurtosis(timesheet$Delivery.Time)
kurtosis(timesheet$Sorting.Time)

boxplot(timesheet)
hist(timesheet$Delivery.Time)
hist(timesheet$Sorting.Time)
plot(timesheet$Delivery.Time,timesheet$Sorting.Time)


attach(timesheet)
cor(Sorting.Time, Delivery.Time)

# Simple Linear Regression model
reg <- lm(Delivery.Time ~ Sorting.Time) # lm(Y ~ X)
summary(reg)
pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(timesheet))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = timesheet, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = timesheet, aes(x=Sorting.Time, y=pred))


# Logrithamic Model

# x = log(sorting.time); y = delivery time

plot(log(Sorting.Time), Delivery.Time)
cor(log(Sorting.Time), Delivery.Time)

reg_log <- lm(Delivery.Time ~ log(Sorting.Time))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(timesheet))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")


######################

# Exponential Model

# x = sorting.time and y = log(delivery time)

plot(Sorting.Time, log(Delivery.Time))

cor(Sorting.Time, log(Delivery.Time))

reg_exp <- lm(log(Delivery.Time) ~ Sorting.Time)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error = timesheet$AT - at
error

sqrt(sum(error^2)/nrow(timesheet))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(Sorting.Time, Delivery.Time)
plot(Sorting.Time*Sorting.Time, Delivery.Time)

cor(Sorting.Time*Sorting.Time, Delivery.Time)

plot(Sorting.Time*Sorting.Time, log(Delivery.Time))

cor(Sorting.Time, log(Delivery.Time))
cor(Sorting.Time*Sorting.Time, log(Delivery.Time))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Delivery.Time) ~ Sorting.Time + I(Sorting.Time*Sorting.Time))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = timesheet$Delivery.Time - expy

sqrt(sum(err^2)/nrow(timesheet))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = timesheet, aes(x = Sorting.Time + I(Sorting.Time^2), y = log(Delivery.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = timesheet, aes(x=Sorting.Time+I(Sorting.Time^2), y=logpol))

##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Delivery.Time)~Sorting.Time + I(Sorting.Time*Sorting.Time) + I(Sorting.Time*Sorting.Time*Sorting.Time))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)
predict(reg3degree,interval = "confidence")

# visualization
ggplot(data = timesheet, aes(x = Sorting.Time + I(Sorting.Time^2) + I(Sorting.Time^3), y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = timesheet, aes(x=Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3), y=expy3))

################################
