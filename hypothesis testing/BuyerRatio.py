# -*- coding: utf-8 -*-
"""
Created on Sat Jul 11 15:46:32 2020

@author: Ranna
"""

import pandas as pd
import numpy as np
import scipy
from scipy import stats


BR = pd.read_csv("C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Hypothesis testing\\BuyerRatio.csv")
BR
BR1 = BR.loc[0:2,['North','South','East','West']]
BR1

## Chi-square test
Chi_square_results = scipy.stats.chi2_contingency(BR1)
Chi_square=[['','Test Statistic','p-value'],['Sample Data',Chi_square_results[0],Chi_square_results[1]]]
Chi_square
# p-value = 0.6603 > 0.05, failed to reject null hypothesis
# Hence, all male-female buyer ratios are equal across the regions






























