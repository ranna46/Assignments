Computer_data <- read.csv(choose.files())
View(Computer_data)

dummy1 <- model.matrix(~cd -1, data = Computer_data)
dummy2 <- model.matrix(~multi -1, data = Computer_data)
dummy3 <- model.matrix(~premium -1, data = Computer_data)

as.numeric(Computer_data$cd)
as.numeric(Computer_data$multi)
as.numeric(Computer_data$premium)
attach(Computer_data)

Computer_data <- cbind(price,speed,hd,ram,screen,cd,multi,premium,ads,trend)
Computer_data <- as.data.frame(Computer_data)
attach(Computer_data)

summary(Computer_data)

library(skimr)
skim(Computer_data)

library(e1071)
skewness(Computer_data$price)
skewness(Computer_data$speed)
skewness(Computer_data$hd)
skewness(Computer_data$ram)
skewness(Computer_data$screen)
skewness(Computer_data$cd)
skewness(Computer_data$multi)
skewness(Computer_data$premium)
skewness(Computer_data$ads)
skewness(Computer_data$trend)

kurtosis(Computer_data$price)
kurtosis(Computer_data$speed)
kurtosis(Computer_data$hd)
kurtosis(Computer_data$ram)
kurtosis(Computer_data$screen)
kurtosis(Computer_data$cd)
kurtosis(Computer_data$multi)
kurtosis(Computer_data$premium)
kurtosis(Computer_data$ads)
kurtosis(Computer_data$trend)

#mode
getmode <- function(Computer_data){
  uniquv <- unique(Computer_data)
  uniquv[which.max(tabulate(match(Computer_data,uniquv)))]
}
getmode(Computer_data$price)
getmode(Computer_data$speed)
getmode(Computer_data$hd)
getmode(Computer_data$ram)
getmode(Computer_data$screen)
getmode(Computer_data$cd)
getmode(Computer_data$multi)
getmode(Computer_data$premium)
getmode(Computer_data$ads)
getmode(Computer_data$trend)

library(stats)
library(dplyr)
str(Computer_data)
pairs(Computer_data)
cor(Computer_data)
library(corpcor)
cor2pcor(cor(Computer_data))

### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-1/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(Computer_data,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")


model.price <- lm(price~., data = Computer_data)
summary(model.price)

model.priceS <- lm(price~log(speed), data = Computer_data)
summary(model.priceS)


model.priceHD <- lm(price~log(hd), data = Computer_data)
summary(model.priceHD)

model.priceR <- lm(price~log(ram), data = Computer_data)
summary(model.priceR)

model.priceHR <- lm(price~hd+ram, data = Computer_data)
summary(model.priceHR)

model.priceHR <- lm(price~log(hd)+log(ram), data = Computer_data)
summary(model.priceHR)

library(car)
vif(model.price)
avPlots(model.price,id.n=2,id.cex=0.7)


model.price <- lm(price~., data = Computer_data)
plot(model.price)
windows()
influence.measures(model.price)
influenceIndexPlot(model.price,id.n=2)
influencePlot(model.price,id.n=2)


model.price1 <- lm(price~., data = Computer_data[-c(1441,1701),])
summary(model.price1)

model.price2 <- lm(price~speed+log(hd)+log(ram)+screen+premium+trend, data = Computer_data[-c(1441,1701),])
summary(model.price2)
#exp model
model.price3 <- lm(log(price)~., data = Computer_data[-c(1441,1701),])
summary(model.price3)
# quad model / poly model (2 deg)
model.price4 <-  lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+cd+I(cd^2)
                    +multi+I(multi^2)+premium+I(premium^2)+ads+I(ads^2)+trend+I(trend^2),data=Computer_data[-c(1441,1701),])
summary(model.price4)
avPlots(model.price4,id.n=2,id.cex=0.7)
model.price5 <-  lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2),data=Computer_data[-c(1441,1701),])
summary(model.price5)
#poly model (3deg)
model.price6 <-  lm(price~speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)
                    +screen+I(screen^2)+I(screen^3)+cd+I(cd^2)+I(cd^3)+multi+I(multi^2)+I(multi^3)
                    +premium+I(premium^2)+I(premium^3)+ads+I(ads^2)+I(ads^3)+trend+I(trend^2)+I(trend^3)
                    ,data=Computer_data[-c(1441,1701),])
summary(model.price6)

plot(lm(price~., data = Computer_data[-c(1441),]))
summary(lm(price~., data = Computer_data[-c(1441),]))
plot(lm(price~., data = Computer_data[-c(1441,1701),]))
summary(lm(price~., data = Computer_data[-c(1441,1701),]))
summary(lm(price~., data = Computer_data[-c(1701),]))

library(MASS)
stepAIC(model.price1)
stepAIC(model.price)


finalmodel <- lm(price~., data = Computer_data[-c(1441,1701),])
summary(finalmodel)
hist(residuals(finalmodel))
predict(finalmodel)

hist(Computer_data$price)
hist(Computer_data$speed)
hist(Computer_data$hd)
hist(Computer_data$ram)
hist(Computer_data$screen)
hist(Computer_data$cd)
hist(Computer_data$multi)
hist(Computer_data$premium)
hist(Computer_data$ads)
hist(Computer_data$trend)



plot(model.price6)

fit <- lm(price ~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(fit)
plot(fit)

fit <- lm(log(price) ~speed+log(hd)+ram+screen+cd+multi+premium+log(ads)+trend)
summary(fit)

fit <- lm(log(price) ~speed+log(hd)+ram+screen+cd+multi+premium+log(ads)+trend, data = Computer_data[-c(1441,1701,1806,4328),])
summary(fit)
plot(fit)
library(car)
vif(fit)
avPlots(fit,id.n=2,id.cex=0.7)

windows()
influence.measures(fit)
influenceIndexPlot(fit,id.n=2)
influencePlot(fit,id.n=2)

library(MASS)
stepAIC(fit)

final <- lm(log(price) ~speed+log(hd)+ram+screen+cd+multi+premium+log(ads)+trend, data = Computer_data[-c(1441,1701,1806,4328),])
summary(final)
plot(final)
hist(residuals(final))
qqPlot(final, id.n=5)
pred <- predict(final)
pred
confint(final, level = 0.95)
predict(final, interval = "predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data =  Computer_data[-c(1441,1701,1806,4328),], aes(x = speed+log(hd)+ram+screen+cd+multi+premium+log(ads)+trend, y = log(price))) + 
  geom_point(color='blue') +
  geom_line(color='red',data =  Computer_data[-c(1441,1701,1806,4328),], aes(x=speed+log(hd)+ram+screen+cd+multi+premium+log(ads)+trend, y=log(price)))

