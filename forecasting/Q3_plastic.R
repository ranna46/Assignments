library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(ggfortify)
library(TTR)
# Loading CocaCola Data
plastic_sales<-read.csv(file.choose()) 
View(plastic_sales)
class(plastic_sales)

########## EDA############
plastic<-ts(plastic_sales$Sales,frequency = 12,start=c(49))  # Converting data into time series object
View(plastic)

class(plastic)
sum(is.na(plastic))
frequency(plastic)
cycle(plastic)
summary(plastic)

plot(plastic,xlab="Month", ylab = "Sales",main="Plastic sales from 1949 to 1953")  
boxplot(plastic~cycle(plastic),xlab="Month", ylab = "Sales" ,main ="Plastic sales from 1949 to 1953")
boxplot(plastic_sales)$out

########## Time series decomposition ############
decomposeplastic<- decompose(plastic,"additive")
autoplot(decomposeplastic)

##########Test Stationarity of the Time series###
# 1. Dickey-fuller test
adf.test(plastic) 
# the dataset is stationary, hence Arima forecasting is the right method

# 2. lag analysis
acf(plastic)
decomposeplastic$random
# Autoplot the random time series from 3:40 which exclude the NA values
autoplot(acf(decomposeplastic$random[7:54],plot=FALSE))+ labs(title="Correlogram of plastic Random Component from 1949 to 1953") 


########## fit a Time series model ############
arimaplastic <- auto.arima(plastic)
arimaplastic
auto.arima(plastic, ic = "aic", trace = TRUE)

######### forecast #######
forecastplastic <- forecast(arimaplastic, level = c(95), h = 12)
autoplot(forecastplastic)
forecastplastic

# Test your final model
ggtsdiag(arimaplastic)
#plot.ts(arimacoke$residuals)
#acf(ts(arimacoke$residuals),main = 'ACF Residual')
hist(arimaplastic$residuals)
qqnorm(arimaplastic$residuals)
qqline(arimaplastic$residuals)

Box.test(arimaplastic$residuals, type = "Ljung-Box") 
