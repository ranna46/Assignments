# -*- coding: utf-8 -*-
"""
Created on Sat Jul 11 14:53:04 2020
y = diameter-continuous, x = discrete in 2 categories
@author: Ranna
"""

import pandas as pd
import numpy as np
import scipy
from scipy import stats


cutlets=pd.read_csv("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Hypothesis testing\\Cutlets.csv")
cutlets.rename(columns={"Unit A":"UA","Unit B":"UB"},inplace=True)

######### normality test#########
print(stats.shapiro(cutlets.UA))    #Shapiro Test
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution
print(stats.shapiro(cutlets.UB))
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution

#################### Variance Test #############
scipy.stats.levene(cutlets.UA, cutlets.UB)
# p-value = 0.41 > 0.05 so p high null fly => Equal variances

######## 2 Sample T test ################
scipy.stats.ttest_ind(cutlets.UA, cutlets.UB)

# null Hypothesis -> there is no significant difference in either unit
# Alternate Hypothesis -> there is a significant difference in 1 unit
# p-value = 0.47 > 0.05 accept null Hypothesis/failed to reject null hypothesis 
































