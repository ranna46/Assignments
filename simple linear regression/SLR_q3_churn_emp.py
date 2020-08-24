# -*- coding: utf-8 -*-
"""
Created on Wed Jul  8 16:26:44 2020
y = churn out rate, x = salary hike
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
from sklearn.model_selection import train_test_split 
from yellowbrick.regressor import ResidualsPlot

data=pd.read_csv("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Simple linear regression\\emp_data.csv")
print(data.rename(columns={'Salary_hike': 'sh', 'Churn_out_rate': 'cr'}, inplace=True))

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
range = max(data['sh'])-min(data['sh'])
print(range)
range2 = max(data['cr'])-min(data['cr'])
print(range2)

# 3rd & 4th moments
data.skew()
data.kurt()

# data visualizdtion
sns.distplot(data.sh, rug=True)
sns.distplot(data.cr,bins=5,rug=True)
#plt.figure(figsize=(15,10))
#plt.tight_layout()
#seabornInstance.distplot(data['dt'])
#seabornInstance.distplot(data['st'])
plt.boxplot(data.sh)
plt.boxplot(data.cr)
#sns.boxplot(x=data["cal"])
#sns.boxplot(y=data["wt"])
#plt.plot(data.st,data.dt,"ro");plt.xlabel("Calories Consumed");plt.ylabel("Weight gained (grams)")
sns.jointplot(x='sh', y='cr', data=data)

data.corr() # -0.911

####
X = data['sh'].values.reshape(-1,1)
y = data['cr'].values.reshape(-1,1)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0)

model1 = LinearRegression()  
model1.fit(X_train, y_train) #training the algorithm

#To retrieve the intercept:
print(model1.intercept_)
#For retrieving the slope:
print(model1.coef_)

y_pred = model1.predict(X_test)

df = pd.DataFrame({'Actual': y_test.flatten(), 'Predicted': y_pred.flatten()})
df

df1 = df.head(25)
df1.plot(kind='bar',figsize=(16,10))
plt.grid(which='major', linestyle='-', linewidth='0.5', color='green')
plt.grid(which='minor', linestyle=':', linewidth='0.5', color='black')
plt.show()

plt.scatter(X_test, y_test,  color='gray')
plt.plot(X_test, y_pred, color='red', linewidth=2)
plt.show()

print('Mean Absolute Error:', metrics.mean_absolute_error(y_test, y_pred))  
print('Mean Squared Error:', metrics.mean_squared_error(y_test, y_pred))  
print('Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(y_test, y_pred)))


visualizer = ResidualsPlot(model1)

visualizer.fit(X_train, y_train)  # Fit the training data to the visualizer
visualizer.score(X_test, y_test)  # Evaluate the model on the test data
visualizer.show()
#from yellowbrick.regressor import residuals_plot
#residuals_plot(model1, X_train, y_train, X_test, y_test)





























































































