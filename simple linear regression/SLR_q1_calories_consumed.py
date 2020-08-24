# -*- coding: utf-8 -*-
"""
Created on Sat Jul  4 21:07:38 2020
SLR : y = weight gained, x= calories consumed
@author: Ranna
"""

# importing libraries
import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statistics
import statsmodels.formula.api as smf

# importing dataset
data=pd.read_csv("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Simple linear regression\\calories_consumed.csv")
print(data.rename(columns={'Weight gained (grams)': 'wt', 'Calories Consumed': 'cal'}, inplace=True))

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
# population var & std
np.var(data)
np.std(data)
range = max(data['wt'])-min(data['wt'])
print(range)
range2 = max(data['cal'])-min(data['cal'])
print(range2)

# 3rd & 4th moments
data.skew()
data.kurt()

# data visualization
sns.distplot(data.cal,bins=5, rug=True);
sns.distplot(data.wt,bins=5,rug=True);
plt.boxplot(data.cal)
plt.boxplot(data.wt)
#sns.boxplot(x=data["cal"])
#sns.boxplot(y=data["wt"])
#plt.plot(data.cal,data.wt,"ro");plt.xlabel("Calories Consumed");plt.ylabel("Weight gained (grams)")
sns.jointplot(x='cal', y='wt', data=data)

data.corr() # 0.946

# preparing LM model using statsmodel
model=smf.ols("wt~cal",data=data).fit()
type(model)
model.params
model.summary() # r^2=0.897
model.conf_int(0.05) # 95% confidence interval
pred = model.predict(data) # Predicted values of AT using the model
pred
plt.scatter(x=data['cal'],y=data['wt'],color='red');plt.plot(data['cal'],pred,color='black');plt.xlabel('calories consumed');plt.ylabel('weight gained')
pred.corr(data.wt) # 0.946
plt.hist(model.resid_pearson) # histogram for residual values 

# Transforming variables for accuracy
model2 = smf.ols('wt~np.log(cal)',data=data).fit()
model2.params
model2.summary() # r^2=0.808
print(model2.conf_int(0.01)) # 99% confidence level
pred2 = model2.predict(data)
pred2.corr(data.wt) # 0.898
# pred2 = model2.predict(wcat.iloc[:,0])
pred2
plt.scatter(x=data['cal'],y=data['wt'],color='red');plt.plot(data['cal'],pred2,color='black');plt.xlabel('calories consumed');plt.ylabel('weight gained')
plt.hist(model2.resid_pearson) # histogram for residual values 

# Exponential transformation
model3 = smf.ols('np.log(wt)~cal',data=data).fit()
model3.params
model3.summary() # r^2=0.878
print(model3.conf_int(0.01)) # 99% confidence level
pred_log = model3.predict(data)
pred_log
pred3=np.exp(pred_log)  # as we have used log(AT) in preparing model so we need to convert it back
pred3
pred3.corr(data.wt) # 0.943
plt.scatter(x=data['cal'],y=data['wt'],color='red');plt.plot(data['cal'],pred3,color='black');plt.xlabel('calories consumed');plt.ylabel('weight gained')
resid_3 = pred3-data.wt

# so we will consider the model having highest R-Squared value which is the log transformation - model3
# getting residuals of the entire data set
student_resid = model3.resid_pearson 
student_resid
plt.plot(pred3,model3.resid_pearson,"o");plt.axhline(y=0,color='green');plt.xlabel("Observation Number");plt.ylabel("Standardized Residual")

# Predicted vs actual values
plt.scatter(x=pred3,y=data.wt);plt.xlabel("Predicted");plt.ylabel("Actual")
plt.hist(model3.resid_pearson) # histogram for residual values 

# Quadratic model
data["cal_Sq"] = data.cal*data.cal
model_quad = smf.ols("wt~cal+cal_Sq",data=data).fit()
model_quad.params
model_quad.summary() #r^2=0.952
pred_quad = model_quad.predict(data)
pred_quad.corr(data.wt) # 0.975
model_quad.conf_int(0.05) # 
plt.scatter(data.cal,data.wt,c="b");plt.plot(data.cal,pred_quad,"r")

plt.hist(model_quad.resid_pearson) # histogram for residual values 

##quad model has the highest r^2 value, hence the best model.

