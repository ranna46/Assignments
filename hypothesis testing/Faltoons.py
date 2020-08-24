# -*- coding: utf-8 -*-
"""
Created on Sat Jul 11 17:52:28 2020

@author: Ranna
"""

import numpy as np
import pandas as pd

fal = pd.read_csv("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Hypothesis testing\\Faltoons.csv")
fal
# the data is discrete and comparing 2 population with each other
# Here we use 2 Proportion Test

#importing packages to do 2 proportion test
from statsmodels.stats.proportion import proportions_ztest

#we do the cross table
#we do the cross table and see How many males and females on week days and week end
tab = fal.groupby(['Weekdays', 'Weekend']).size()
count=pd.crosstab(fal["Weekdays"],fal["Weekend"])

count = np.array([120, 47]) #How many females and males on weekend
nobs = np.array([287, 113]) #Total number of  Females and males

proportions_ztest(count, nobs,alternative='two-sided')
# Ho <- there is no difference in percentage of males vs females walking in to the store
# Ho <- there is a difference in percentage of males vs females walking in to the store
# p-value = 0.96 > 0.05, falied to reject Ho
# hence, there is no difference in percentage of males vs females walking in to the store








