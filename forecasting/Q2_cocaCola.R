library(readxl)
library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(ggfortify)
library(xts)
library(zoo)
# Loading CocaCola Data
CocaCola<-read_excel(file.choose()) 
View(CocaCola)
class(CocaCola)

########## EDA############
coke<-ts(CocaCola$Sales,frequency = 4,start=c(86))  # Converting data into time series object
View(coke)

class(coke)
sum(is.na(coke))
frequency(coke)
cycle(coke)
summary(coke)

plot(coke,xlab="Quarter", ylab = "Sales",main="CocaCola sales from 1986 to 1996") # or
#autoplot(coke) + labs(x ="Quarter", y = "Sales", title="CocaCola sales from 1986 to 1996")  
boxplot(coke~cycle(coke),xlab="Quarter", ylab = "Sales" ,main ="CocaCola sales from 1986 to 1996")

# Visualization shows that it has level, trend, seasonality => Additive seasonality
# no outliers, no data cleaning required
# there is a cycle of 12 months


########## Time series decomposition ############
decomposecoke <- decompose(coke,"additive")
autoplot(decomposecoke)

##########Test Stationarity of the Time series###
# 1. Dickey-fuller test
adf.test(coke) 
# the dataset is stationary, hence Arima forecasting is the right method

# 2. lag analysis
acf(coke)
decomposecoke$random
# Autoplot the random time series from 3:40 which exclude the NA values
autoplot(acf(decomposecoke$random[3:40],plot=FALSE))+ labs(title="Correlogram of CocaCola Random Component from 1986 to 1996") 


########## fit a Time series model ############
arimacoke <- auto.arima(coke)
arimacoke
auto.arima(coke, ic = "aic", trace = TRUE)

######### forecast #######
forecastcoke <- forecast(arimacoke, level = c(95), h = 12)
autoplot(forecastcoke)
forecastcoke

# Test your final model
ggtsdiag(arimacoke)
#plot.ts(arimacoke$residuals)
#acf(ts(arimacoke$residuals),main = 'ACF Residual')
hist(arimacoke$residuals)
qqnorm(arimacoke$residuals)
qqline(arimacoke$residuals)

Box.test(arimacoke$residuals, type = "Ljung-Box") # p value more than 0.05, hence indicating 
#autocorelation is still in effect, and some changes have to be made to the model













