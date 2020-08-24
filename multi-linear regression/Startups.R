#choose file
startup <- read.csv(choose.files())
View(startup)

#EDA
summary(startup)
library(skimr)
skim(startup)
var(startup$R.D.Spend)
var(startup$Administration)
var(startup$Marketing.Spend)
library(e1071)
skewness(startup$R.D.Spend)
skewness(startup$Administration)
skewness(startup$Marketing.Spend)
kurtosis(startup$R.D.Spend)
kurtosis(startup$Administration)
kurtosis(startup$Marketing.Spend)
boxplot(startup)

library(stats)
library(dplyr)
str(startup)
unique(startup$State)
pairs(startup)

#creating dummy matrix
dummies <- model.matrix(~State -1, data = startup)
#making it numeric
as.numeric(startup$State)
attach(startup)
startup <- cbind(R.D.Spend=R.D.Spend,Administration,Marketing.Spend=Marketing.Spend,State,Profit)
startup <- as.data.frame(startup)
attach(startup)

#finding correlation coefficients
cor(startup)
#finding partial correlation
library(corpcor)
cor2pcor(cor(startup))

### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.6/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(startup,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")


profit_model1 <- lm(Profit~., data = startup)
summary(profit_model1)

profit_model1$residuals
sum(profit_model1$residuals)
mean(profit_model1$residuals)
sqrt(sum(profit_model1$residuals^2)/nrow(startup))  #RMSE
sqrt(mean(profit_model1$residuals^2))


profit_model1.1 <- lm(Profit~., data = startup[-c(49,50),])
summary(profit_model1.1)


profit_model2 <- lm(Profit~log(Administration), data = startup)
summary(profit_model2)
      
profit_model3 <- lm(Profit~Marketing.Spend, data = startup)
summary(profit_model3) ##is sig.

profit_model4 <- lm(Profit~log(State), data = startup)
summary(profit_model4)

library(car)
vif(profit_model1)
avPlots(profit_model1,id.n=2,id.cex=0.7)

profit_model5 <- lm(Profit~R.D.Spend+Marketing.Spend, data = startup)
summary(profit_model5)

influence.measures(profit_model1)
windows()
influenceIndexPlot(profit_model1, id.n=3)
influencePlot(profit_model1, id.n=3)

profit_model6 <- lm(Profit~R.D.Spend+Marketing.Spend, data = startup[-c(49,50),])
summary(profit_model6)

profit_model6$residuals
sum(profit_model6$residuals)
mean(profit_model6$residuals)
sqrt(sum(profit_model6$residuals^2)/nrow(startup[-c(49,50),]))  #RMSE
sqrt(mean(profit_model6$residuals^2))

#exponential model
profit_model7 <- lm(log(Profit)~R.D.Spend+Marketing.Spend, data = startup[-c(49,50),])
summary(profit_model7)
#poly model (quad model = 2 deg)
profit_model8 <-  lm(Profit~R.D.Spend+I(R.D.Spend^2)+Administration+I(Administration^2)
                     +Marketing.Spend+I(Marketing.Spend^2)+State+I(State^2),data=startup[-c(49,50),])
summary(profit_model8)

profit_model9 <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+Marketing.Spend+I(Marketing.Spend^2)
                           ,data=startup[-c(49,50),])
summary(profit_model9)

#poly model (3 deg)
profit_model10 <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3)+
                            Administration+I(Administration^2)+I(Administration^3)+
                            Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3)+
                            State+I(State^2)+I(State^3),data=startup[-c(49,50),])
summary(profit_model10)

profit_model11 <- lm(Profit~R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3)+
                             Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3)
                           ,data=startup[-c(49,50),])
summary(profit_model11)

library(MASS)
stepAIC(profit_model1)
library(MASS)
stepAIC(profit_model1.1)
library(MASS)
stepAIC(profit_model6)

#final model = profit_model6
final.model <- lm(Profit~R.D.Spend+Marketing.Spend, data = startup[-c(49,50),])
summary(final.model)
plot(final.model)
hist(residuals(final.model))
qqPlot(final.model, id.n=5)
pred <- predict(final.model)
pred
confint(final.model, level = 0.95)
predict(final.model, interval = "predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = startup[-c(49,50),], aes(x = R.D.Spend+Marketing.Spend, y = Profit)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = startup[-c(49,50),], aes(x=R.D.Spend+Marketing.Spend, y=Profit))
