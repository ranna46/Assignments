tc <- read.csv(choose.files())
View(tc)
tc<-tc[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(tc)
str(tc)
summary(tc)
library(skimr)
skim(tc)
pairs(tc)
library(stats)
library(dplyr)
cor(tc)
library(corpcor)
cor2pcor(cor(tc))
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.8/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(tc,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

fit <- lm(Price~., data = tc)
summary(fit)

fit <- lm(Price~cc, data = tc)
summary(fit)
fit <- lm(Price~Doors, data = tc)
summary(fit)
fit <- lm(Price~cc+Doors, data = tc)
summary(fit)

fit <- lm(Price~.-cc-Doors, data = tc)
summary(fit)
library(MASS)
stepAIC(fit)

library(car)
vif(fit)
avPlots(fit,id.n=2,id.cex=0.7)
windows()
influence.measures(fit)
influenceIndexPlot(fit,id.n=2)
influencePlot(fit,id.n=2)

fit <- lm(Price~.-cc-Doors, data = tc[-c(222),])
summary(fit)
fit <- lm(Price~., data = tc[-c(222,961,602),])
summary(fit)

finalmodel <- lm(Price~., data = tc[-c(222,961,602),])
summary(finalmodel)
plot(finalmodel)
hist(residuals(finalmodel))
qqPlot(finalmodel, id.n=5)
pred <- predict(finalmodel)
pred
confint(finalmodel, level = 0.95)
predict(finalmodel, interval = "predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data =  tc[-c(222,961,602),], aes(x = Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, y = Price)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = tc[-c(222,961,602),], aes(x=Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, y=Price))
