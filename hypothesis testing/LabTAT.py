# -*- coding: utf-8 -*-
"""
Created on Sat Jul 11 15:14:28 2020

@author: Ranna
"""

import pandas as pd
import numpy as np
import scipy
from scipy import stats


lab=pd.read_csv("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Hypothesis testing\\LabTAT.csv")
lab.rename(columns={"Laboratory 1":"L1","Laboratory 2":"L2", "Laboratory 3":"L3", "Laboratory 4":"L4"},inplace=True)
lab1 = lab.stack()

######### normality test#########
print(stats.shapiro(lab1))    #Shapiro Test
# p-value = 0.11 >0.05 so p high null fly => It follows normal distribution

#################### Variance Test #############
scipy.stats.levene(lab.L1,lab.L2,lab.L3,lab.L4)
# p-value = 0.051 > 0.05 so p high null fly => Equal variances

#ANOVA test (Analysis of Variance ) for unequal variance
#help(scipy.stats.f_oneway)
scipy.stats.f_oneway(lab.L1,lab.L2,lab.L3,lab.L4)
# pvalue= 2.1156708949992414e-57 < 0.05
# H0 = there is no difference in avg.TAT in any of the 4 reports
# Ha = there is a difference in avg.TAT in any one of the 4 reports

# p-value = 0.2e-16 < 0.05 reject null hypothesis 
#  There is a difference in avg.TAT in any one of the 4 reports































