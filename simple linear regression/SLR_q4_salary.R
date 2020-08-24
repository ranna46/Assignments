salary_data <- read.csv(choose.files())
View(salary_data)
#EDA
#1st moment
summary(salary_data)
#mode
getmode <- function(salary_data){
  uniquv <- unique(salary_data)
  uniquv[which.max(tabulate(match(salary_data,uniquv)))]
}  

getmode(salary_data$YearsExperience)
getmode(salary_data$Salary)

#2nd moment
var(salary_data$YearsExperience)
sd(salary_data$YearsExperience)
range(salary_data$YearsExperience)
rangevalue <- function(salary_data){max(salary_data)-min(salary_data)}
rangevalue(salary_data$YearsExperience)

var(salary_data$Salary)
sd(salary_data$Salary)
range(salary_data$Salary)
rangevalue <- function(salary_data){max(salary_data)-min(salary_data)}
rangevalue(salary_data$Salary)

#3rd moment
library(moments)
skewness(salary_data$YearsExperience)
skewness(salary_data$Salary)
#4th moment
kurtosis(salary_data$YearsExperience)
kurtosis(salary_data$Salary)

boxplot(salary_data)
hist(salary_data$YearsExperience)
hist(salary_data$Salary)
plot(salary_data$YearsExperience,salary_data$Salary)


attach(salary_data)
cor(YearsExperience, Salary)

# Simple Linear Regression model
reg <- lm(Salary ~ YearsExperience) # lm(Y ~ X)
summary(reg)
pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(salary_data))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = salary_data, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience, y=pred))


