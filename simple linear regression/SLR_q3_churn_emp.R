emp <- read.csv(file.choose())
View(emp)
#EDA
#1st moment
summary(emp)
#mode
getmode <- function(emp){
  uniquv <- unique(emp)
  uniquv[which.max(tabulate(match(emp,uniquv)))]
}  

getmode(emp$Salary_hike)
getmode(emp$Churn_out_rate)

#2nd moment
var(emp$Salary_hike)
sd(emp$Salary_hike)
range(emp$Salary_hike)
rangevalue <- function(emp){max(emp)-min(emp)}
rangevalue(emp$Salary_hike)

var(emp$Churn_out_rate)
sd(emp$Churn_out_rate)
range(emp$Churn_out_rate)
rangevalue <- function(emp){max(emp)-min(emp)}
rangevalue(emp$Churn_out_rate)

#3rd moment
library(moments)
skewness(emp$Salary_hike)
skewness(emp$Churn_out_rate)
#4th moment
kurtosis(emp$Salary_hike)
kurtosis(emp$Churn_out_rate)

boxplot(emp)
hist(emp$Salary_hike)
hist(emp$Churn_out_rate)
plot(emp$Salary_hike,emp$Churn_out_rate)


attach(emp)
cor(Salary_hike, Churn_out_rate)

# Simple Linear Regression model
reg <- lm(Churn_out_rate ~ Salary_hike) # lm(Y ~ X)
summary(reg)
pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = emp, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=Salary_hike, y=pred))


