# -*- coding: utf-8 -*-
"""
Cresyed on Wed Jul  8 17:01:55 2020
y = salary , x = years of experience
@author: Ranna
"""

# importing libraries
import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statistics
from sklearn.linear_model import LinearRegression
from sklearn import metrics


data=pd.read_csv("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Simple linear regression\\Salary_data.csv")
print(data.rename(columns={'Salary': 'sy', 'YearsExperience': 'ye'}, inplace=True))

data.info()
data.describe()

# 1st moment
data.mean()
data.mode()
data.median()

# 2nd moment
# sample var & std
data.var()
data.std()
# populdtion var & std
np.var(data)
np.std(data)
range = max(data['sy'])-min(data['sy'])
print(range)
range2 = max(data['ye'])-min(data['ye'])
print(range2)

# 3rd & 4th moments
data.skew()
data.kurt()

# data visualizdtion
sns.distplot(data.sy, rug=True)
sns.distplot(data.ye,bins=5,rug=True)
#plt.figure(figsize=(15,10))
#plt.tight_layout()
#seabornInstance.distplot(data['dt'])
#seabornInstance.distplot(data['st'])
plt.boxplot(data.sy)
plt.boxplot(data.ye)
#sns.boxplot(x=data["cal"])
#sns.boxplot(y=data["wt"])
#plt.plot(data.st,data.dt,"ro");plt.xlabel("Calories Consumed");plt.ylabel("Weight gained (grams)")
sns.jointplot(x='ye', y='sy', data=data)

data.corr() # 0.978

##
plt.scatter(data.ye, data.sy)
model1 = LinearRegression()
model1.fit(data.ye.values.reshape(-1,1), data.sy)
pred1 = model1.predict(data.ye.values.reshape(-1,1))

# adjusting r squred value
model1.score(data.ye.values.reshape(-1,1), data.sy)
rmse1 = np.sqrt(np.mean((pred1-data.sy)**2))
model1.coef_
model1.intercept_

# residuals vs fitted vaues
plt.scatter(pred1,(pred1-data.sy),c='r')
plt.hlines(y=0, xmin=0, xmax=500)
# checking normal distribution  fro residual
plt.hist(pred1-data.sy)

# fitting quadrsyic regression
data["ye_sqrd"] = data.ye*data.ye
model2 = LinearRegression()
model2.fit(X = data.iloc[:,[0,2]], y=data.sy)
pred2 = model2.predict(data.iloc[:,[0,2]])

# adjusted r sqrd value
model2.score(data.iloc[:,[0,2]], data.sy)
rmse2 = np.sqrt(np.mean((pred2-data.sy)**2))
model2.coef_
model2.intercept_

## residuals vs fitted vaues
plt.scatter(pred2,(pred2-data.sy),c='r')
plt.hlines(y=0, xmin=0, xmax=200)
# checking normal distribution  fro residual
plt.hist(pred2-data.sy)
import pylab
import scipy.stats as st
st.probplot(pred2-data.sy, dist="norm", plot=pylab)

## model preparsyoin using transformsyion on dependent variable
data["sy_sqrt"] = np.sqrt(data.sy)
model3 = LinearRegression()
model3.fit(data.iloc[:,[0,2]], data.sy_sqrt)
pred3 = model3.predict(data.iloc[:,[0,2]])

# adjusted r sqrd value
model3.score(data.iloc[:,[0,2]], data.sy_sqrt)
rmse3 = np.sqrt(np.mean(((pred3)-data.sy))**2)
model3.coef_
model3.intercept_

## residuals vs fitted vaues
plt.scatter((pred3)**2,((pred3)**2-data.sy),c='r')
plt.hlines(y=0, xmin=0, xmax=200)
# checking normal distribution  fro residual
plt.hist((pred3)**2-data.sy_sqrt)
st.probplot((pred3)**2-data.sy, dist="norm", plot=pylab)

# preparing model by applying transformsyion on dependent varibale without transformsyion on input variables
model4 = LinearRegression()
model4.fit(data.ye.values.reshape(-1,1), data.sy_sqrt)
pred4 = model4.predict(data.ye.values.reshape(-1,1))

## adjusted r sqrd value
model4.score(data.ye.values.reshape(-1,1), data.sy_sqrt)
rmse4 = np.sqrt(np.mean(((pred4)**2-data.sy)**2))
model4.coef_
model4.intercept_

# residuals vs fitted vaues
plt.scatter((pred4)**2,((pred4)**2-data.sy),c='r')
plt.hlines(y=0, xmin=0, xmax=300)
st.probplot((pred4)**2-data.sy, dist="norm", plot=pylab)
# checking normal distribution  fro residual
plt.hist(pred4-data.sy)





























































