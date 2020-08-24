# -*- coding: utf-8 -*-
"""
Credted on Mon Jul  6 12:43:26 2020
SLR : y = delivery time, x= sorting time
@author: Ranna
"""


# importing libraries
import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statistics
#import seaborn as seabornInstance 
#from sklearn.model_selection import train_test_split 
from sklearn.linear_model import LinearRegression
from sklearn import metrics

#pip install yellowbrick
# importing dataset
data=pd.read_csv("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Simple linear regression\\delivery_time.csv")
print(data.rename(columns={'Delivery Time': 'dt', 'Sorting Time': 'st'}, inplace=True))

data.info()
data.describe()

# 1st moment
data.mean()
data.dt.mean()
data.mode()
data.median()

# 2nd moment
# sample var & std
data.var()
data.std()
# populdtion var & std
np.var(data)
np.std(data)
range = max(data['dt'])-min(data['dt'])
print(range)
range2 = max(data['st'])-min(data['st'])
print(range2)

# 3rd & 4th moments
data.skew()
data.kurt()

# data visualizdtion
sns.distplot(data.st,bins=5, rug=True)
sns.distplot(data.dt,bins=5,rug=True)
#plt.figure(figsize=(15,10))
#plt.tight_layout()
#seabornInstance.distplot(data['dt'])
#seabornInstance.distplot(data['st'])
plt.boxplot(data.st)
plt.boxplot(data.dt)
#sns.boxplot(x=data["cal"])
#sns.boxplot(y=data["wt"])
#plt.plot(data.st,data.dt,"ro");plt.xlabel("Calories Consumed");plt.ylabel("Weight gained (grams)")
sns.jointplot(x='st', y='dt', data=data)

data.corr() # 0.825

#
plt.scatter(data.st, data.dt)
model1 = LinearRegression()
model1.fit(data.st.values.reshape(-1,1), data.dt)
pred1 = model1.predict(data.st.values.reshape(-1,1))

# adjusting r squred value
model1.score(data.st.values.reshape(-1,1), data.dt)
rmse1 = np.sqrt(np.mean((pred1-data.dt)**2))
model1.coef_
model1.intercept_

# residuals vs fitted vaues
plt.scatter(pred1,(pred1-data.dt),c='r')
plt.hlines(y=0, xmin=0, xmax=500)
# checking normal distribution  fro residual
plt.hist(pred1-data.dt)

# fitting quadrdtic regression
data["st_sqrd"] = data.st*data.st
model2 = LinearRegression()
model2.fit(X = data.iloc[:,[0,2]], y=data.dt)
pred2 = model2.predict(data.iloc[:,[0,2]])

# adjusted r sqrd value
model2.score(data.iloc[:,[0,2]], data.dt)
rmse2 = np.sqrt(np.mean((pred2-data.dt)**2))
model2.coef_
model2.intercept_

## residuals vs fitted vaues
plt.scatter(pred2,(pred2-data.dt),c='r')
plt.hlines(y=0, xmin=0, xmax=200)
# checking normal distribution  fro residual
plt.hist(pred2-data.dt)
import pylab
import scipy.stats as st
st.probplot(pred2-data.dt, dist="norm", plot=pylab)

## model prepardtoin using transformdtion on dependent variable
data["dt_sqrt"] = np.sqrt(data.dt)
model3 = LinearRegression()
model3.fit(X = data.iloc[:,[0,2]], y=data.dt_sqrt)
pred3 = model3.predict(data.iloc[:,[0,2]])

# adjusted r sqrd value
model3.score(data.iloc[:,[0,2]], data.dt_sqrt)
rmse3 = np.sqrt(np.mean(((pred3)-data.dt))**2)
model3.coef_
model3.intercept_

## residuals vs fitted vaues
plt.scatter((pred3)**2,((pred3)**2-data.dt),c='r')
plt.hlines(y=0, xmin=0, xmax=200)
# checking normal distribution  fro residual
plt.hist((pred3)**2-data.dt_sqrt)
st.probplot((pred3)**2-data.dt, dist="norm", plot=pylab)

# preparing model by applying transformdtion on dependent varibale without transformdtion on input variables
model4 = LinearRegression()
model4.fit(data.st.values.reshape(-1,1), data.dt_sqrt)
pred4 = model4.predict(data.st.values.reshape(-1,1))

## adjusted r sqrd value
model4.score(data.st.values.reshape(-1,1), data.dt_sqrt)
rmse4 = np.sqrt(np.mean(((pred4)**2-data.dt)**2))
model4.coef_
model4.intercept_

# residuals vs fitted vaues
plt.scatter((pred4)**2,((pred4)**2-data.dt),c='r')
plt.hlines(y=0, xmin=0, xmax=300)
st.probplot((pred4)**2-data.dt, dist="norm", plot=pylab)
# checking normal distribution  fro residual
plt.hist(pred4-data.dt)
